use convert_case::{Case, Casing};
use gluon_parser::{CustomType, EnumDef, Field, Interface, Protocol, StructDef, Type};
use gluon_wire::ExternalGluonProtocol;
use quote::{format_ident, quote};
use std::collections::HashSet;
use std::ops::Deref;

pub use gluon_wire::Derives;

pub mod helpers;

/// the [`ExternalGluonProtocol`] should come from the `EXTERNAL_PROTOCOL` const from the module
/// defined in `rust_module`
pub struct ExternalProtocol {
    pub rust_module: &'static str,
    pub external_protocol: ExternalGluonProtocol,
}
pub struct LocalProtocol {
    /// Short module name (e.g. `"test"`, `"types"`), used to match `TypeProxy` prefixes.
    pub module_name: String,
    pub rust_module: String,
    pub protocol: Protocol,
}
/// Maps a protocol type name to an existing Rust type that should be used in the public API
/// instead. The crate implementing the protocol must provide `From<WireType> for RustType` and
/// `From<RustType> for WireType` impls. The generated wire types become `pub(crate)`.
pub struct TypeProxy {
    /// `"module::TypeName"` — module is the short name passed to `gen_multiple_modules`
    pub protocol_type_name: String,
    /// The Rust type path to surface in the public API (e.g. `"glam::Vec3"`)
    pub rust_type: String,
    /// Derives that the proxy type implements, so the codegen can propagate them correctly
    /// to structs/enums that contain this type as a field.
    pub derives: Derives,
}
impl Deref for LocalProtocol {
    type Target = Protocol;

    fn deref(&self) -> &Self::Target {
        &self.protocol
    }
}
impl Deref for ExternalProtocol {
    type Target = ExternalGluonProtocol;

    fn deref(&self) -> &Self::Target {
        &self.external_protocol
    }
}
#[derive(Clone, Copy)]
pub struct GenCtx<'a> {
    pub curr_protocol: &'a LocalProtocol,
    pub other_local_protocols: &'a [&'a LocalProtocol],
    pub external_protocols: &'a [&'a ExternalProtocol],
    /// Which derives to attempt on generated structs/enums. A derive is only
    /// applied if every field/variant member supports it.
    pub requested_derives: Derives,
    /// Protocol type names mapped to their public Rust proxy types.
    pub type_proxies: &'a [TypeProxy],
}

pub fn gen_module(
    proto: &LocalProtocol,
    other_local_protocols: &[&LocalProtocol],
    external_protocols: &[&ExternalProtocol],
    requested_derives: Derives,
    type_proxies: &[TypeProxy],
) -> proc_macro2::TokenStream {
    let gen_ctx = &GenCtx {
        curr_protocol: proto,
        other_local_protocols,
        external_protocols,
        requested_derives,
        type_proxies,
    };
    let interfaces = proto
        .interfaces
        .iter()
        .map(|(name, interface)| gen_interface(name, interface, gen_ctx));
    let structs = proto
        .structs
        .iter()
        .map(|(_name, def)| gen_struct(def, gen_ctx));
    let enums = proto
        .enums
        .iter()
        .map(|(_name, def)| gen_enum(def, gen_ctx));
    let external_proto_const = gen_external_protocol_const(gen_ctx);
    quote! {
        #![allow(unused, clippy::single_match, clippy::match_single_binding, clippy::large_enum_variant, private_bounds, private_interfaces)]
        use gluon_wire::GluonConvertable;
        #external_proto_const
        #(#structs)*
        #(#enums)*
        #(#interfaces)*
    }
}
pub fn gen_external_protocol_const(gen_ctx: &GenCtx) -> proc_macro2::TokenStream {
    let types = gen_ctx
        .curr_protocol
        .structs
        .iter()
        .map(|(name, v)| (name.clone(), struct_supported_derives(v, gen_ctx)))
        .chain(
            gen_ctx
                .curr_protocol
                .enums
                .iter()
                .map(|(name, v)| (name.clone(), enum_supported_derives(v, gen_ctx))),
        )
        .map(|(name, derives)| {
            let bits = derives.bits();
            quote! {
                gluon_wire::ExternalGluonType {
                    name: #name,
                    supported_derives: gluon_wire::Derives::from_bits_truncate(#bits)
                }
            }
        });
    let proto_name = &gen_ctx.curr_protocol.name;
    quote! {
        pub const EXTERNAL_PROTOCOL: gluon_wire::ExternalGluonProtocol = gluon_wire::ExternalGluonProtocol {
            protocol_name: #proto_name,
            types: &[#(#types),*],
        };
    }
}
fn proxy_matches(p: &TypeProxy, type_name: &str, gen_ctx: &GenCtx) -> bool {
    match p.protocol_type_name.split_once("::") {
        Some((prefix, name)) => prefix == gen_ctx.curr_protocol.module_name && name == type_name,
        None => panic!(
            "TypeProxy::protocol_type_name must be \"module::TypeName\", got {:?}",
            p.protocol_type_name
        ),
    }
}

/// Finds the registered proxy for a bare type name within the current protocol.
fn find_proxy_by_name<'a>(type_name: &str, gen_ctx: &'a GenCtx) -> Option<&'a TypeProxy> {
    gen_ctx.type_proxies.iter().find(|p| proxy_matches(p, type_name, gen_ctx))
}

/// Finds the registered proxy for a qualified type (`namespace::TypeName`), resolving the import
/// alias to the target protocol's `module_name` before matching.
fn find_proxy_for_qualified<'a>(
    namespace: &str,
    type_name: &str,
    gen_ctx: &'a GenCtx,
) -> Option<&'a TypeProxy> {
    let import = gen_ctx.curr_protocol.imports.iter().find(|v| v.alias == namespace)?;
    let target = gen_ctx.other_local_protocols.iter().find(|v| v.name == import.name)?;
    gen_ctx.type_proxies.iter().find(|p| match p.protocol_type_name.split_once("::") {
        Some((prefix, name)) => prefix == target.module_name && name == type_name,
        None => panic!(
            "TypeProxy::protocol_type_name must be \"module::TypeName\", got {:?}",
            p.protocol_type_name
        ),
    })
}

/// Finds the registered proxy for a `Type`, returning its parsed `TokenStream`.
fn find_proxy(ty: &Type, gen_ctx: &GenCtx) -> Option<proc_macro2::TokenStream> {
    let proxy = match ty {
        Type::Custom(CustomType::Named(type_name)) => find_proxy_by_name(type_name, gen_ctx),
        Type::Custom(CustomType::Qualified(namespace, type_name)) => {
            find_proxy_for_qualified(namespace, type_name, gen_ctx)
        }
        _ => None,
    };
    proxy.map(|p| p.rust_type.parse().expect("TypeProxy::rust_type is not a valid token stream"))
}

/// Like `gen_type` but substitutes the proxy rust type when one is registered.
fn gen_public_type(ty: &Type, gen_ctx: &GenCtx) -> proc_macro2::TokenStream {
    find_proxy(ty, gen_ctx).unwrap_or_else(|| gen_type(ty, gen_ctx))
}

pub fn gen_interface(
    interface_name: &str,
    def: &Interface,
    gen_ctx: &GenCtx,
) -> proc_macro2::TokenStream {
    let name = format_ident!("{}", interface_name.to_case(Case::Pascal));
    let handler_name = format_ident!("{name}Handler");
    let handler = {
        // Dispatch arms: read wire types from the binder, convert to proxy types for the handler
        // call, then convert return values back to wire types for the response.
        let methods_dispatch = def.methods.iter().enumerate().map(|(i, method)| {
            let i = i + 8;
            let names = method.params.iter().map(|v| format_ident!("param_{}", v.name)).collect::<Vec<_>>();
            let params = names.iter().zip(method.params.iter()).map(|(var, param)| {
                if let Some(proxy_ty) = find_proxy(&param.ty, gen_ctx) {
                    let wire_ty = gen_type(&param.ty, gen_ctx);
                    quote! {
                        let #var: #proxy_ty = {
                            let __w: #wire_ty = gluon_wire::GluonConvertable::read(&mut gluon_data)?;
                            __w.into()
                        };
                    }
                } else {
                    quote! { let #var = gluon_wire::GluonConvertable::read(&mut gluon_data)?; }
                }
            });
            let name = format_ident!("{}", method.name.to_case(Case::Snake));
            let return_names = method.returns.as_ref().map(|v| {
                v.iter()
                    .map(|v| format_ident!("{}", v.name.to_case(Case::Snake)))
                    .collect::<Vec<_>>()
            });
            let i = i as u32;
            if let Some(ref return_names) = return_names {
                let return_writes = return_names.iter().zip(method.returns.as_ref().unwrap().iter()).map(|(ret_name, ret_def)| {
                    if find_proxy(&ret_def.ty, gen_ctx).is_some() {
                        let wire_ty = gen_type(&ret_def.ty, gen_ctx);
                        quote! {
                            let __w: #wire_ty = #ret_name.into();
                            __w.write_owned(&mut gluon_out)?;
                        }
                    } else {
                        quote! { #ret_name.write_owned(&mut gluon_out)?; }
                    }
                }).collect::<Vec<_>>();
                quote! {
                    #i => {
                        let return_callback = gluon_data.read_binder()?;
                        let mut gluon_out = gluon_wire::GluonDataBuilder::new();
                        #(#params)*
                        let (#(#return_names),*) = self.#name(ctx, #(#names),*).await;
                        drop(gluon_data);
                        #(#return_writes)*
                        return_callback.device().transact_one_way(&return_callback, 0, gluon_out.to_payload())?;
                    },
                }
            } else {
                quote! {
                    #i => {
                        #(#params)*
                        drop(gluon_data);
                        self.#name(ctx, #(#names),*).await;
                    },
                }
            }
        });
        // Handler trait: public-facing signatures use proxy types where registered.
        let methods = def.methods.iter().map(|method| {
            let params = method.params.iter().map(|param| {
                let type_def = gen_public_type(&param.ty, gen_ctx);
                let name = format_ident!("{}", param.name.to_case(Case::Snake));
                quote! { #name: #type_def }
            });
            let name = format_ident!("{}", method.name.to_case(Case::Snake));
            let doc_comment = method.doc.as_ref().map(|str| quote! {#[doc = #str]});
            let return_types = method.returns.as_ref().map(|v| {
                v.iter().map(|v| gen_public_type(&v.ty, gen_ctx)).collect::<Vec<_>>()
            });
            let fn_return = match return_types.as_deref() {
                None => quote! { -> impl Future<Output=()> + Send + Sync },
                Some(types) => {
                    let types = match types {
                        [] => quote! {()},
                        [ty] => quote! {#ty},
                        types => quote! {(#(#types),*)},
                    };
                    quote! { -> impl Future<Output=#types> + Send + Sync }
                }
            };
            quote! {
                #doc_comment
                fn #name(&self, _ctx: gluon_wire::GluonCtx, #(#params),*) #fn_return;
            }
        });
        quote! {
            pub trait #handler_name: binderbinder::device::TransactionHandler + Send + Sync + 'static {
                #(#methods)*

                fn dispatch_one_way(&self, transaction_code: u32, mut gluon_data: gluon_wire::GluonDataReader, ctx: gluon_wire::GluonCtx) -> impl Future<Output=Result<(),gluon_wire::GluonSendError>> + Send + Sync {
                    async move {
                        match transaction_code {
                            #(#methods_dispatch)*
                            _ => {}
                        }
                        Ok(())
                    }
                }
            }
        }
    };
    let proxy = {
        let methods = def.methods.iter().enumerate().map(|(i, method)| {
            // Params: proxy type taken directly; non-proxy uses impl Into<WireType> for ergonomics.
            let params = method.params.iter().map(|param| {
                let name = format_ident!("{}", param.name.to_case(Case::Snake));
                if let Some(proxy_ty) = find_proxy(&param.ty, gen_ctx) {
                    quote! { #name: #proxy_ty }
                } else {
                    let wire_ty = gen_type(&param.ty, gen_ctx);
                    quote! { #name: impl Into<#wire_ty> }
                }
            }).collect::<Vec<_>>();
            // Convert every param to its wire type before writing.
            let params_convert = method.params.iter().map(|param| {
                let wire_ty = gen_type(&param.ty, gen_ctx);
                let name = format_ident!("{}", param.name.to_case(Case::Snake));
                quote! { let #name: #wire_ty = #name.into(); }
            }).collect::<Vec<_>>();
            let params_write = method.params.iter().map(|param| {
                let name = format_ident!("{}", param.name.to_case(Case::Snake));
                quote! { #name.write(&mut gluon_builder)?; }
            }).collect::<Vec<_>>();
            let name = format_ident!("{}", method.name.to_case(Case::Snake));
            let doc_comment = method.doc.as_ref().map(|str| quote! {#[doc = #str]});
            // Return types: public (proxy) types in the signature.
            let pub_return_types = method.returns.as_ref().map(|v| {
                v.iter().map(|v| gen_public_type(&v.ty, gen_ctx)).collect::<Vec<_>>()
            });
            let i = i as u32 + 8;
            match pub_return_types {
                Some(ref pub_types) => {
                    let ret_defs = method.returns.as_ref().unwrap();
                    let fn_return = match pub_types.as_slice() {
                        [] => quote! {()},
                        [ty] => quote! {#ty},
                        types => quote! {(#(#types),*)},
                    };
                    // Read each return from wire, converting to proxy type when needed.
                    let return_reads: Vec<proc_macro2::TokenStream> = ret_defs.iter().map(|ret_def| {
                        let base = quote! { gluon_wire::GluonConvertable::read(&mut reader)? };
                        if find_proxy(&ret_def.ty, gen_ctx).is_some() {
                            let wire_ty = gen_type(&ret_def.ty, gen_ctx);
                            quote! { { let __w: #wire_ty = #base; __w.into() } }
                        } else {
                            base
                        }
                    }).collect();
                    let return_tuple = match return_reads.as_slice() {
                        [] => quote! {()},
                        [single] => single.clone(),
                        reads => quote! {(#(#reads),*)},
                    };
                    quote! {
                        #doc_comment
                        pub async fn #name(&self, #(#params),*) -> Result<#fn_return, gluon_wire::GluonSendError> {
                            #(#params_convert)*
                            let mut gluon_builder = gluon_wire::GluonDataBuilder::new();
                            let (gluon_ret_handler, mut gluon_recv) = gluon_wire::ReturnHandler::new();
                            let gluon_ret = self.obj.device().register_object(gluon_ret_handler);
                            gluon_builder.write_binder(&gluon_ret)?;
                            #(#params_write)*
                            self.obj.device().transact_one_way(&self.obj, #i, gluon_builder.to_payload())?;
                            // safe since we're also holding the channel sender
                            let transaction = gluon_recv.recv().await.unwrap();
                            let mut reader = gluon_wire::GluonDataReader::from_payload(transaction.payload);
                            Ok(#return_tuple)
                        }
                    }
                }
                None => quote! {
                    #doc_comment
                    pub fn #name(&self, #(#params),*) -> Result<(), gluon_wire::GluonSendError> {
                        #(#params_convert)*
                        let mut gluon_builder = gluon_wire::GluonDataBuilder::new();
                        #(#params_write)*
                        self.obj.device().transact_one_way(&self.obj, #i, gluon_builder.to_payload())?;
                        Ok(())
                    }
                },
            }
        });
        quote! {
            #[derive(Debug, Clone)]
            pub struct #name {
                obj: binderbinder::binder_object::BinderObjectOrRef,
            }

            impl gluon_wire::GluonConvertable for #name {
                fn write<'a, 'b: 'a>(
                    &'b self,
                    gluon_data: &mut gluon_wire::GluonDataBuilder<'a>,
                ) -> Result<(), gluon_wire::GluonWriteError> {
                    self.obj.write(gluon_data)
                }

                fn read(gluon_data: &mut gluon_wire::GluonDataReader) -> Result<Self, gluon_wire::GluonReadError> {
                    let obj = binderbinder::binder_object::BinderObjectOrRef::read(gluon_data)?;
                    Ok(#name::from_object_or_ref(obj))
                }

                fn write_owned(self, gluon_data: &mut gluon_wire::GluonDataBuilder<'_>) -> Result<(), gluon_wire::GluonWriteError> {
                    self.obj.write_owned(gluon_data)
                }
            }
            impl #name {
                #(#methods)*
                pub fn from_handler<H: #handler_name>(obj: &impl binderbinder::binder_object::OwnedBinderObjectRefTrait<H>) -> #name {
                    #name::from_object_or_ref(binderbinder::binder_object::ToBinderObjectOrRef::to_binder_object_or_ref(obj))
                }
                #[doc = "only use this when you know the binder ref implements this interface, else the consquences are for you to find out"]
                pub fn from_object_or_ref(obj: binderbinder::binder_object::BinderObjectOrRef) -> #name {
                    #name {
                        obj,
                    }
                }
            }
            impl binderbinder::binder_object::ToBinderObjectOrRef for #name {
                fn to_binder_object_or_ref(&self) -> binderbinder::binder_object::BinderObjectOrRef {
                    self.obj.to_binder_object_or_ref()
                }
            }
            impl std::hash::Hash for #name {
                fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                    self.obj.hash(state);
                }
            }
            impl PartialEq for #name {
                fn eq(&self, other: &Self) -> bool {
                    self.obj == other.obj
                }
            }
            impl Eq for #name {}
        }
    };
    quote! {
        #proxy
        #handler
    }
}

/// Returns true if `ty` transitively contains `target` as an inline (non-heap-allocated) type.
/// Vec/Set/Map/Ref are heap-allocated and break the cycle; Option/Result/Array are inline.
fn type_is_recursive(ty: &Type, target: &str, gen_ctx: &GenCtx) -> bool {
    let mut visiting = HashSet::new();
    type_contains_inline(ty, target, gen_ctx, &mut visiting)
}

fn type_contains_inline(
    ty: &Type,
    target: &str,
    gen_ctx: &GenCtx,
    visiting: &mut HashSet<String>,
) -> bool {
    match ty {
        Type::Bool
        | Type::U8
        | Type::U16
        | Type::U32
        | Type::U64
        | Type::I8
        | Type::I16
        | Type::I32
        | Type::I64
        | Type::F32
        | Type::F64
        | Type::String
        | Type::Fd => false,
        // heap-allocated or pointer — break any recursion cycle
        Type::Vec(_) | Type::Set(_) | Type::Map(_, _) | Type::Ref(_) => false,
        // inline wrappers — propagate
        Type::Option(inner) | Type::Array(inner, _) => {
            type_contains_inline(inner, target, gen_ctx, visiting)
        }
        Type::Result(ok, err) => {
            type_contains_inline(ok, target, gen_ctx, visiting)
                || type_contains_inline(err, target, gen_ctx, visiting)
        }
        Type::Custom(CustomType::Qualified(_, _)) => false, // external type, safe
        Type::Custom(CustomType::Named(name)) => {
            if name == target {
                return true;
            }
            if visiting.contains(name) {
                return false;
            }
            visiting.insert(name.clone());
            let result = named_type_contains_inline(name, target, gen_ctx, visiting);
            visiting.remove(name);
            result
        }
    }
}

fn named_type_contains_inline(
    name: &str,
    target: &str,
    gen_ctx: &GenCtx,
    visiting: &mut HashSet<String>,
) -> bool {
    let proto = &gen_ctx.curr_protocol.protocol;
    if let Some((_, s)) = proto.structs.iter().find(|(n, _)| n == name) {
        return s
            .fields
            .iter()
            .any(|f| type_contains_inline(&f.ty, target, gen_ctx, visiting));
    }
    if let Some((_, e)) = proto.enums.iter().find(|(n, _)| n == name) {
        return e
            .variants
            .iter()
            .flat_map(|v| v.fields.iter())
            .any(|f| type_contains_inline(&f.ty, target, gen_ctx, visiting));
    }
    false
}

pub fn gen_struct(def: &StructDef, gen_ctx: &GenCtx) -> proc_macro2::TokenStream {
    let is_proxied = find_proxy_by_name(&def.name, gen_ctx).is_some();
    let vis = if is_proxied {
        quote! { pub(crate) }
    } else {
        quote! { pub }
    };
    let fields = def
        .fields
        .iter()
        .map(|f| gen_field_struct(f, gen_ctx, type_is_recursive(&f.ty, &def.name, gen_ctx), is_proxied));
    let name = def.name.to_case(Case::Pascal);
    let derives = derives_to_tokens(struct_supported_derives(def, gen_ctx));
    let name = format_ident!("{}", name);
    let doc = &def.doc;
    let gluon_trait_impl = {
        let field_names = def
            .fields
            .iter()
            .map(|v| format_ident!("{}", v.name))
            .collect::<Vec<_>>();
        let writes = def.fields.iter().map(|f| {
            let fname = format_ident!("{}", f.name);
            if find_proxy(&f.ty, gen_ctx).is_some() {
                let wire_ty = gen_type(&f.ty, gen_ctx);
                // write_owned avoids the 'b:'a lifetime constraint on a local value
                quote! { { let __w: #wire_ty = self.#fname.clone().into(); __w.write_owned(gluon_data)?; } }
            } else {
                quote! { self.#fname.write(gluon_data)?; }
            }
        });
        let reads = def.fields.iter().map(|f| {
            let fname = format_ident!("{}", f.name);
            if find_proxy(&f.ty, gen_ctx).is_some() {
                let wire_ty = gen_type(&f.ty, gen_ctx);
                let pub_ty = gen_public_type(&f.ty, gen_ctx);
                quote! {
                    let #fname: #pub_ty = {
                        let __w: #wire_ty = gluon_wire::GluonConvertable::read(gluon_data)?;
                        __w.into()
                    };
                }
            } else {
                quote! { let #fname = gluon_wire::GluonConvertable::read(gluon_data)?; }
            }
        });
        let writes_owned = def.fields.iter().map(|f| {
            let fname = format_ident!("{}", f.name);
            if find_proxy(&f.ty, gen_ctx).is_some() {
                let wire_ty = gen_type(&f.ty, gen_ctx);
                quote! { { let __w: #wire_ty = self.#fname.into(); __w.write_owned(gluon_data)?; } }
            } else {
                quote! { self.#fname.write_owned(gluon_data)?; }
            }
        });
        quote! {
            impl gluon_wire::GluonConvertable for #name {
                fn write<'a, 'b: 'a>(
                    &'b self,
                    gluon_data: &mut gluon_wire::GluonDataBuilder<'a>,
                ) -> Result<(), gluon_wire::GluonWriteError> {
                    #(#writes)*
                    Ok(())
                }

                fn read(gluon_data: &mut gluon_wire::GluonDataReader) -> Result<Self, gluon_wire::GluonReadError> {
                    #(#reads)*
                    Ok(#name {#(#field_names,)*})
                }

                fn write_owned(self, gluon_data: &mut gluon_wire::GluonDataBuilder<'_>) -> Result<(), gluon_wire::GluonWriteError> {
                    #(#writes_owned)*
                    Ok(())
                }
            }
        }
    };
    quote! {
        #[doc = #doc]
        #[derive(Debug, #(#derives),*)]
        #vis struct #name {
            #(#fields)*
        }

        #gluon_trait_impl
    }
}

pub fn gen_enum(def: &EnumDef, gen_ctx: &GenCtx) -> proc_macro2::TokenStream {
    let variants = def.variants.iter().map(|variant| {
        let fields = variant.fields.iter().map(|f| {
            gen_field_enum(f, gen_ctx, type_is_recursive(&f.ty, &def.name, gen_ctx))
        });
        let name = format_ident!("{}", variant.name.to_case(Case::Pascal));
        let doc_comment = variant.doc.as_ref().map(|str| quote! {#[doc = #str]});
        if !variant.fields.is_empty() {
            quote! {
                #doc_comment
                #name {
                    #(#fields)*
                }
            }
        } else {
            quote! {
                #doc_comment
                #name
            }
        }
    });
    let is_proxied = find_proxy_by_name(&def.name, gen_ctx).is_some();
    let vis = if is_proxied {
        quote! { pub(crate) }
    } else {
        quote! { pub }
    };
    let name = def.name.to_case(Case::Pascal);
    let derives = derives_to_tokens(enum_supported_derives(def, gen_ctx));
    let enum_name = format_ident!("{}", name);
    let doc = &def.doc;
    let gluon_trait_impl = {
        let write_variants = def.variants.iter().enumerate().map(|(i, variant)| {
            let field_names = variant.fields.iter()
                .map(|v| format_ident!("{}", v.name.to_case(Case::Snake)))
                .collect::<Vec<_>>();
            let name = format_ident!("{}", variant.name.to_case(Case::Pascal));
            let i = i as u16;
            if field_names.is_empty() {
                quote! { #enum_name::#name => { gluon_data.write_u16(#i)?; }, }
            } else {
                let field_writes = variant.fields.iter().map(|f| {
                    let fname = format_ident!("{}", f.name.to_case(Case::Snake));
                    if find_proxy(&f.ty, gen_ctx).is_some() {
                        let wire_ty = gen_type(&f.ty, gen_ctx);
                        quote! { { let __w: #wire_ty = #fname.clone().into(); __w.write_owned(gluon_data)?; } }
                    } else {
                        quote! { #fname.write(gluon_data)?; }
                    }
                });
                quote! {
                    #enum_name::#name { #(#field_names),* } => {
                        gluon_data.write_u16(#i)?;
                        #(#field_writes)*
                    },
                }
            }
        });
        let write_owned_variants = def.variants.iter().enumerate().map(|(i, variant)| {
            let field_names = variant.fields.iter()
                .map(|v| format_ident!("{}", v.name.to_case(Case::Snake)))
                .collect::<Vec<_>>();
            let name = format_ident!("{}", variant.name.to_case(Case::Pascal));
            let i = i as u16;
            if field_names.is_empty() {
                quote! { #enum_name::#name => { gluon_data.write_u16(#i)?; }, }
            } else {
                let field_writes_owned = variant.fields.iter().map(|f| {
                    let fname = format_ident!("{}", f.name.to_case(Case::Snake));
                    if find_proxy(&f.ty, gen_ctx).is_some() {
                        let wire_ty = gen_type(&f.ty, gen_ctx);
                        quote! { { let __w: #wire_ty = #fname.into(); __w.write_owned(gluon_data)?; } }
                    } else {
                        quote! { #fname.write_owned(gluon_data)?; }
                    }
                });
                quote! {
                    #enum_name::#name { #(#field_names),* } => {
                        gluon_data.write_u16(#i)?;
                        #(#field_writes_owned)*
                    },
                }
            }
        });
        let read_variants = def.variants.iter().enumerate().map(|(i, variant)| {
            let field_names = variant.fields.iter()
                .map(|v| format_ident!("{}", v.name.to_case(Case::Snake)))
                .collect::<Vec<_>>();
            let name = format_ident!("{}", variant.name.to_case(Case::Pascal));
            let i = i as u16;
            if variant.fields.is_empty() {
                quote! { #i => { #enum_name::#name }, }
            } else {
                let field_reads = variant.fields.iter().map(|f| {
                    let fname = format_ident!("{}", f.name.to_case(Case::Snake));
                    if find_proxy(&f.ty, gen_ctx).is_some() {
                        let wire_ty = gen_type(&f.ty, gen_ctx);
                        let pub_ty = gen_public_type(&f.ty, gen_ctx);
                        quote! {
                            let #fname: #pub_ty = {
                                let __w: #wire_ty = gluon_wire::GluonConvertable::read(gluon_data)?;
                                __w.into()
                            };
                        }
                    } else {
                        quote! { let #fname = gluon_wire::GluonConvertable::read(gluon_data)?; }
                    }
                });
                quote! {
                    #i => {
                        #(#field_reads)*
                        #enum_name::#name { #(#field_names,)* }
                    },
                }
            }
        });
        quote! {
            impl gluon_wire::GluonConvertable for #enum_name {
                fn write<'a, 'b: 'a>(
                    &'b self,
                    gluon_data: &mut gluon_wire::GluonDataBuilder<'a>,
                ) -> Result<(), gluon_wire::GluonWriteError> {
                    match self {
                        #(#write_variants)*
                    };
                    Ok(())
                }

                fn read(gluon_data: &mut gluon_wire::GluonDataReader) -> Result<Self, gluon_wire::GluonReadError> {
                    Ok(match gluon_data.read_u16()? {
                        #(#read_variants)*
                        v => return Err(gluon_wire::GluonReadError::UnknownEnumVariant(v)),
                    })
                }

                fn write_owned(self, gluon_data: &mut gluon_wire::GluonDataBuilder<'_>) -> Result<(), gluon_wire::GluonWriteError> {
                    match self {
                        #(#write_owned_variants)*
                    };
                    Ok(())
                }
            }
        }
    };
    quote! {
        #[doc = #doc]
        #[derive(Debug, #(#derives),*)]
        #vis enum #enum_name {
            #(#variants),*
        }

        #gluon_trait_impl
    }
}

pub fn gen_field_enum(def: &Field, gen_ctx: &GenCtx, boxed: bool) -> proc_macro2::TokenStream {
    let type_def = gen_public_type(&def.ty, gen_ctx);
    let type_def = if boxed {
        quote! { Box<#type_def> }
    } else {
        type_def
    };
    let name = format_ident!("{}", def.name.to_case(Case::Snake));
    let doc_comment = def.doc.as_ref().map(|str| quote! {#[doc = #str]});
    quote! {
        #doc_comment
        #name: #type_def,
    }
}
pub fn gen_field_struct(def: &Field, gen_ctx: &GenCtx, boxed: bool, parent_is_proxied: bool) -> proc_macro2::TokenStream {
    let type_def = gen_public_type(&def.ty, gen_ctx);
    let type_def = if boxed {
        quote! { Box<#type_def> }
    } else {
        type_def
    };
    let vis = if parent_is_proxied {
        quote! { pub(crate) }
    } else {
        quote! { pub }
    };
    let name = format_ident!("{}", def.name.to_case(Case::Snake));
    let doc_comment = def.doc.as_ref().map(|str| quote! {#[doc = #str]});
    quote! {
        #doc_comment
        #vis #name: #type_def,
    }
}

pub fn gen_custom_type(custom: &CustomType, gen_ctx: &GenCtx) -> proc_macro2::TokenStream {
    match custom {
        CustomType::Named(name) => {
            let name = format_ident!("{}", name.to_case(Case::Pascal));
            quote! {#name}
        }
        CustomType::Qualified(namespace, name) => {
            let import = gen_ctx
                .curr_protocol
                .imports
                .iter()
                .find(|v| &v.alias == namespace)
                .expect("unknown namespace used in qualified type");
            let name = format_ident!("{}", name.to_case(Case::Pascal));
            // Check local protocols first (sibling modules in the same output file)
            let rust_mod = gen_ctx
                .other_local_protocols
                .iter()
                .find(|v| v.name == import.name)
                .map(|v| v.rust_module.clone())
                .or_else(|| {
                    gen_ctx
                        .external_protocols
                        .iter()
                        .find(|v| v.protocol_name == import.name)
                        .map(|v| v.rust_module.to_string())
                })
                .expect("failed to resolve namespace for qualified type");
            let namespace_path = rust_mod.split("::").map(|v| format_ident!("{}", v));
            quote! {#(#namespace_path)::*::#name}
        }
    }
}

pub fn gen_type(def: &Type, gen_ctx: &GenCtx) -> proc_macro2::TokenStream {
    match def {
        Type::Bool => quote! {bool},
        Type::U8 => quote! {u8},
        Type::U16 => quote! {u16},
        Type::U32 => quote! {u32},
        Type::U64 => quote! {u64},
        Type::I8 => quote! {i8},
        Type::I16 => quote! {i16},
        Type::I32 => quote! {i32},
        Type::I64 => quote! {i64},
        Type::F32 => quote! {f32},
        Type::F64 => quote! {i64},
        Type::String => quote! {String},
        Type::Fd => quote! {std::os::fd::OwnedFd},
        Type::Ref(ref_type) => match ref_type {
            Some(custom) => gen_custom_type(custom, gen_ctx),
            None => quote! {binderbinder::binder_object::BinderObjectOrRef},
        },
        Type::Custom(custom) => gen_custom_type(custom, gen_ctx),
        Type::Array(type_def, len) => {
            let type_def = gen_type(type_def, gen_ctx);
            quote! {[#type_def; #len]}
        }
        Type::Vec(type_def) => {
            let type_def = gen_type(type_def, gen_ctx);
            quote! {Vec<#type_def>}
        }
        Type::Set(type_def) => {
            let type_def = gen_type(type_def, gen_ctx);
            quote! {std::collections::HashSet<#type_def>}
        }
        Type::Map(key, value) => {
            let key = gen_type(key, gen_ctx);
            let value = gen_type(value, gen_ctx);
            quote! {std::collections::HashMap<#key,#value>}
        }
        Type::Option(type_def) => {
            let type_def = gen_type(type_def, gen_ctx);
            quote! {Option<#type_def>}
        }
        Type::Result(ok, err) => {
            let ok = gen_type(ok, gen_ctx);
            let err = gen_type(err, gen_ctx);
            quote! {Result<#ok, #err>}
        }
    }
}

fn struct_supported_derives(def: &StructDef, gen_ctx: &GenCtx) -> Derives {
    let mut visiting = HashSet::new();
    def.fields
        .iter()
        .map(|f| supported_derives_inner(&f.ty, gen_ctx, &mut visiting))
        .fold(gen_ctx.requested_derives, |acc, d| acc & d)
}

fn enum_supported_derives(def: &EnumDef, gen_ctx: &GenCtx) -> Derives {
    let mut visiting = HashSet::new();
    // Enums can't use #[derive(Default)] without a #[default] attribute on a variant,
    // which we don't generate, so always exclude it.
    let requested = gen_ctx.requested_derives - Derives::DEFAULT;
    def.variants
        .iter()
        .flat_map(|v| v.fields.iter())
        .map(|f| supported_derives_inner(&f.ty, gen_ctx, &mut visiting))
        .fold(requested, |acc, d| acc & d)
}

/// Returns which of the requested derives this type can support.
pub fn supported_derives(def: &Type, gen_ctx: &GenCtx) -> Derives {
    supported_derives_inner(def, gen_ctx, &mut HashSet::new())
}

fn supported_derives_inner(
    def: &Type,
    gen_ctx: &GenCtx,
    visiting: &mut HashSet<String>,
) -> Derives {
    let requested = gen_ctx.requested_derives;
    match def {
        Type::Bool
        | Type::U8
        | Type::U16
        | Type::U32
        | Type::U64
        | Type::I8
        | Type::I16
        | Type::I32
        | Type::I64 => requested & Derives::INTEGERS,
        Type::F32 | Type::F64 => requested & Derives::FLOATS,
        Type::String => {
            requested
                & (Derives::CLONE
                    | Derives::HASH
                    | Derives::PARTIAL_EQ
                    | Derives::EQ
                    | Derives::PARTIAL_ORD
                    | Derives::ORD
                    | Derives::DEFAULT)
        }
        // OwnedFd doesn't implement any derivable traits (other than Debug)
        Type::Fd => Derives::empty(),
        Type::Ref(_) => requested & Derives::CLONE,
        Type::Custom(custom) => custom_type_derives_inner(custom, gen_ctx, visiting),
        Type::Array(v, _) => supported_derives_inner(v, gen_ctx, visiting),
        Type::Vec(v) => supported_derives_inner(v, gen_ctx, visiting) - Derives::COPY,
        Type::Set(v) => supported_derives_inner(v, gen_ctx, visiting),
        Type::Option(v) => supported_derives_inner(v, gen_ctx, visiting),
        // TODO: figure out correct semantics
        Type::Result(_, _) => Derives::empty(),
        // TODO: figure out correct semantics
        Type::Map(_, _) => Derives::empty(),
    }
}

fn custom_type_derives_inner(
    custom: &CustomType,
    gen_ctx: &GenCtx,
    visiting: &mut HashSet<String>,
) -> Derives {
    match custom {
        CustomType::Named(name) => {
            if let Some(proxy) = find_proxy_by_name(name, gen_ctx) {
                return gen_ctx.requested_derives & proxy.derives;
            }
            derives_from_protocol_inner(name, gen_ctx, visiting)
        }
        CustomType::Qualified(namespace, type_name) => {
            if let Some(proxy) = find_proxy_for_qualified(namespace, type_name, gen_ctx) {
                return gen_ctx.requested_derives & proxy.derives;
            }
            let import = gen_ctx
                .curr_protocol
                .imports
                .iter()
                .find(|v| &v.alias == namespace)
                .expect("unknown namespace used in qualified type");
            if let Some(v) = gen_ctx
                .other_local_protocols
                .iter()
                .find(|v| v.name == import.name)
            {
                return derives_from_protocol_inner(
                    type_name,
                    &GenCtx {
                        curr_protocol: v,
                        ..*gen_ctx
                    },
                    visiting,
                );
            }

            let proto = gen_ctx
                .external_protocols
                .iter()
                .find(|v| v.protocol_name == import.name)
                .unwrap_or_else(|| panic!("unknown import: {namespace}"));
            proto
                .types
                .iter()
                .find(|v| v.name == type_name)
                .unwrap_or_else(|| panic!("unknown type: {type_name}"))
                .supported_derives
        }
    }
}

fn derives_from_protocol_inner(
    type_name: &str,
    gen_ctx: &GenCtx,
    visiting: &mut HashSet<String>,
) -> Derives {
    // Cycle guard: if already computing this type's derives, assume all requested
    // derives are supported so other fields in the cycle can restrict further.
    if !visiting.insert(type_name.to_string()) {
        return gen_ctx.requested_derives;
    }
    let result = gen_ctx
        .curr_protocol
        .enums
        .iter()
        .find(|(n, _)| n == type_name)
        .map(|(_, v)| {
            v.variants
                .iter()
                .flat_map(|v| v.fields.iter())
                .map(|v| supported_derives_inner(&v.ty, gen_ctx, visiting))
                .reduce(|a, b| a & b)
                .unwrap_or(gen_ctx.requested_derives)
        })
        .or_else(|| {
            gen_ctx
                .curr_protocol
                .structs
                .iter()
                .find(|(n, _)| n == type_name)
                .map(|(_, v)| {
                    v.fields
                        .iter()
                        .map(|v| supported_derives_inner(&v.ty, gen_ctx, visiting))
                        .reduce(|a, b| a & b)
                        .unwrap_or(gen_ctx.requested_derives)
                })
        })
        .or_else(|| {
            gen_ctx
                .curr_protocol
                .interfaces
                .iter()
                .find(|(name, _)| name == type_name)
                .map(|_| gen_ctx.requested_derives & Derives::CLONE)
        })
        // for types with no fields, they support all requested derives
        .unwrap_or_else(|| panic!("unknown type: {type_name}"));
    visiting.remove(type_name);
    result
}

fn derives_to_tokens(derives: Derives) -> Vec<proc_macro2::Ident> {
    let mut out = Vec::new();
    if derives.contains(Derives::COPY) {
        out.push(format_ident!("Copy"));
    }
    if derives.contains(Derives::CLONE) {
        out.push(format_ident!("Clone"));
    }
    if derives.contains(Derives::HASH) {
        out.push(format_ident!("Hash"));
    }
    if derives.contains(Derives::PARTIAL_EQ) {
        out.push(format_ident!("PartialEq"));
    }
    if derives.contains(Derives::EQ) {
        out.push(format_ident!("Eq"));
    }
    if derives.contains(Derives::PARTIAL_ORD) {
        out.push(format_ident!("PartialOrd"));
    }
    if derives.contains(Derives::ORD) {
        out.push(format_ident!("Ord"));
    }
    if derives.contains(Derives::DEFAULT) {
        out.push(format_ident!("Default"));
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use gluon_parser::parse_idl;

    fn make_ctx(protocol: gluon_parser::Protocol) -> (LocalProtocol, Derives) {
        let local = LocalProtocol {
            module_name: "test".to_string(),
            rust_module: "test".to_string(),
            protocol,
        };
        (local, Derives::empty())
    }

    #[test]
    fn recursive_struct_direct() {
        // Node directly contains Option<Node> — Option is inline so needs Box
        let (local, derives) = make_ctx(
            parse_idl(
                "test",
                r#"
                /// A linked list node
                struct Node {
                    value: u32,
                    next: Option<Node>,
                }
            "#,
            )
            .unwrap(),
        );
        let gen_ctx = GenCtx {
            curr_protocol: &local,
            other_local_protocols: &[],
            external_protocols: &[],
            requested_derives: derives,
            type_proxies: &[],
        };
        let def = &local.protocol.structs.iter().find(|(n, _)| n == "Node").unwrap().1;
        let tokens = gen_struct(def, &gen_ctx).to_string();
        assert!(tokens.contains("Box"), "expected Box for direct recursive struct:\n{tokens}");
    }

    #[test]
    fn recursive_enum_direct() {
        // Tree enum whose Branch variant embeds Tree directly
        let (local, derives) = make_ctx(
            parse_idl(
                "test",
                r#"
                /// A binary tree
                enum Tree {
                    Leaf {
                        value: u32,
                    },
                    Branch {
                        left: Tree,
                        right: Tree,
                    },
                }
            "#,
            )
            .unwrap(),
        );
        let gen_ctx = GenCtx {
            curr_protocol: &local,
            other_local_protocols: &[],
            external_protocols: &[],
            requested_derives: derives,
            type_proxies: &[],
        };
        let def = &local.protocol.enums.iter().find(|(n, _)| n == "Tree").unwrap().1;
        let tokens = gen_enum(def, &gen_ctx).to_string();
        assert!(tokens.contains("Box"), "expected Box for recursive enum:\n{tokens}");
    }

    #[test]
    fn recursive_mutual_struct_enum() {
        // Expr → Node → Expr is a mutual cycle; both should be boxed
        let (local, derives) = make_ctx(
            parse_idl(
                "test",
                r#"
                /// An expression
                enum Expr {
                    Lit {
                        value: u32,
                    },
                    Add {
                        node: Node,
                    },
                }

                /// A binary node
                struct Node {
                    left: Expr,
                    right: Expr,
                }
            "#,
            )
            .unwrap(),
        );
        let gen_ctx = GenCtx {
            curr_protocol: &local,
            other_local_protocols: &[],
            external_protocols: &[],
            requested_derives: derives,
            type_proxies: &[],
        };

        let expr_def = &local.protocol.enums.iter().find(|(n, _)| n == "Expr").unwrap().1;
        let node_def = &local.protocol.structs.iter().find(|(n, _)| n == "Node").unwrap().1;

        let expr_tokens = gen_enum(expr_def, &gen_ctx).to_string();
        let node_tokens = gen_struct(node_def, &gen_ctx).to_string();

        assert!(
            expr_tokens.contains("Box"),
            "expected Box in Expr (mutual recursion):\n{expr_tokens}"
        );
        assert!(
            node_tokens.contains("Box"),
            "expected Box in Node (mutual recursion):\n{node_tokens}"
        );
    }

    #[test]
    fn vec_breaks_recursion_no_box() {
        // Vec is heap-allocated, so children: Vec<Node> should NOT trigger boxing
        let (local, derives) = make_ctx(
            parse_idl(
                "test",
                r#"
                /// A tree node with heap-allocated children
                struct TreeNode {
                    value: u32,
                    children: Vec<TreeNode>,
                }
            "#,
            )
            .unwrap(),
        );
        let gen_ctx = GenCtx {
            curr_protocol: &local,
            other_local_protocols: &[],
            external_protocols: &[],
            requested_derives: derives,
            type_proxies: &[],
        };
        let def = &local.protocol.structs.iter().find(|(n, _)| n == "TreeNode").unwrap().1;
        let tokens = gen_struct(def, &gen_ctx).to_string();
        assert!(
            !tokens.contains("Box"),
            "did not expect Box for Vec<T> recursion:\n{tokens}"
        );
    }

    #[test]
    fn non_recursive_no_box() {
        let (local, derives) = make_ctx(
            parse_idl(
                "test",
                r#"
                /// A simple point
                struct Point {
                    x: f32,
                    y: f32,
                }
            "#,
            )
            .unwrap(),
        );
        let gen_ctx = GenCtx {
            curr_protocol: &local,
            other_local_protocols: &[],
            external_protocols: &[],
            requested_derives: derives,
            type_proxies: &[],
        };
        let def = &local.protocol.structs.iter().find(|(n, _)| n == "Point").unwrap().1;
        let tokens = gen_struct(def, &gen_ctx).to_string();
        assert!(
            !tokens.contains("Box"),
            "did not expect Box for non-recursive struct:\n{tokens}"
        );
    }
}
