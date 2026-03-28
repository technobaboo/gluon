use convert_case::{Case, Casing};
use gluon_parser::{CustomType, EnumDef, Field, Interface, Protocol, StructDef, Type};
use gluon_wire::ExternalGluonProtocol;
use quote::{format_ident, quote};
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
    pub rust_module: String,
    pub protocol: Protocol,
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
}

pub fn gen_module(
    proto: &LocalProtocol,
    other_local_protocols: &[&LocalProtocol],
    external_protocols: &[&ExternalProtocol],
    requested_derives: Derives,
) -> proc_macro2::TokenStream {
    let gen_ctx = &GenCtx {
        curr_protocol: proto,
        other_local_protocols,
        external_protocols,
        requested_derives,
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
        #![allow(unused, clippy::single_match, clippy::match_single_binding)]
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
pub fn gen_interface(
    interface_name: &str,
    def: &Interface,
    gen_ctx: &GenCtx,
) -> proc_macro2::TokenStream {
    let name = format_ident!("{}", interface_name.to_case(Case::Pascal));
    let handler_name = format_ident!("{name}Handler");
    let handler = {
        let methods_dispatch = def
            .methods
            .iter()
            .enumerate()
            .filter(|(_, m)| m.returns.is_some())
            .map(|(i, method)| {
                let i = i + 8;
                let params = method
                    .params
                    .iter()
                    .map(|_| quote! {gluon_wire::GluonConvertable::read(gluon_data)?});
                let name = format_ident!("{}", method.name.to_case(Case::Snake));
                let return_names = method
                    .returns
                    .as_ref()
                    .unwrap()
                    .iter()
                    .map(|v| format_ident!("{}", v.name.to_case(Case::Snake)))
                    .collect::<Vec<_>>();
                let i = i as u32;
                quote! {
                    #i => {
                        let (#(#return_names),*) = self.#name(ctx, #(#params),*).await;
                        #(
                            #return_names.write_owned(&mut out)?;
                        )*
                    },
                }
            });
        let oneway_methods_dispatch = def
            .methods
            .iter()
            .enumerate()
            .filter(|(_, m)| m.returns.is_none())
            .map(|(i, method)| {
                let i = i + 8;
                let params = method
                    .params
                    .iter()
                    .map(|_| quote! {gluon_wire::GluonConvertable::read(gluon_data)?});
                let name = format_ident!("{}", method.name.to_case(Case::Snake));
                let i = i as u32;
                quote! {
                    #i => {
                        self.#name(ctx, #(#params),*);
                    },
                }
            });
        let methods = def.methods.iter().map(|method| {
            let params = method.params.iter().map(|param| {
                let type_def = gen_type(&param.ty, gen_ctx);
                let name = format_ident!("{}", param.name.to_case(Case::Snake));
                quote! {
                    #name: #type_def
                }
            });
            let name = format_ident!("{}", method.name.to_case(Case::Snake));
            let doc_comment = method.doc.as_ref().map(|str| quote! {#[doc = #str]});
            // TODO: gen return docs and names into main fn docs?
            let return_types = method.returns.as_ref().map(|v| {
                v.iter()
                    .map(|v| gen_type(&v.ty, gen_ctx))
                    .collect::<Vec<_>>()
            });
            let fn_return = match return_types.as_deref() {
                None => {
                    quote! {}
                }
                Some(types) => {
                    let types = match types {
                        [] => quote! {()},
                        [ty] => quote! {#ty},
                        types => quote! {(#(#types),*)},
                    };
                    quote! {
                        -> impl Future<Output=#types> + Send + Sync
                    }
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

                fn drop_notification_requested(&self, notifier: gluon_wire::drop_tracking::DropNotifier) -> impl Future<Output=()> + Send + Sync;
                fn dispatch_two_way(&self, transaction_code: u32, gluon_data: &mut gluon_wire::GluonDataReader, ctx: gluon_wire::GluonCtx) -> impl Future<Output=Result<gluon_wire::GluonDataBuilder<'static>, gluon_wire::GluonSendError>> + Send + Sync {
                    async move {
                        let mut out = gluon_wire::GluonDataBuilder::new();
                        match transaction_code {
                            #(#methods_dispatch)*
                            _ => {}
                        }
                        Ok(out)
                    }
                }
                fn dispatch_one_way(&self, transaction_code: u32, gluon_data: &mut gluon_wire::GluonDataReader, ctx: gluon_wire::GluonCtx) -> impl Future<Output=Result<(),gluon_wire::GluonSendError>> + Send + Sync {
                    async move {
                        match transaction_code {
                            4 => {
                                let Ok(obj) = gluon_data.read_binder() else { return Ok(()); };
                                self.drop_notification_requested(gluon_wire::drop_tracking::DropNotifier::new(&obj)).await;
                            }
                            #(#oneway_methods_dispatch)*
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
            let params = method.params.iter().map(|param| {
                let type_def = gen_type(&param.ty,gen_ctx);
                let name = format_ident!("{}", param.name.to_case(Case::Snake));
                quote! {
                    #name: #type_def
                }
            }).collect::<Vec<_>>();
            let params_write = method.params.iter().map(|param| {
                let name = format_ident!("{}", param.name.to_case(Case::Snake));
                quote! {#name.write(&mut gluon_builder)?;}
            }).collect::<Vec<_>>();
            let param_names = method.params.iter().map(|param| {
                format_ident!("{}", param.name.to_case(Case::Snake))
            }).collect::<Vec<_>>();
            let name = format_ident!("{}", method.name.to_case(Case::Snake));
            let doc_comment = method.doc.as_ref().map(|str| quote! {#[doc = #str]});
            let return_types = method
                .returns
                .as_ref()
                .map(|v| v.iter().map(|v| gen_type(&v.ty,gen_ctx)).collect::<Vec<_>>());
            let i = i as u32 + 8;
            match return_types {
                Some(types) => {
                    let fn_return = match types.as_slice() {
                        [] => quote! {()},
                        [ty] => quote! {#ty},
                        types => quote! {(#(#types),*)},
                    };
                    let return_tuple = match types.as_slice() {
                        [] => quote! {()},
                        [_] => quote! {gluon_wire::GluonConvertable::read(&mut reader)?},
                        types => {
                            let types = types
                                .iter()
                                .map(|_| quote! {gluon_wire::GluonConvertable::read(&mut reader)?});
                            quote! {(#(#types),*)}
                        }
                    };
                    let blocking_name = format_ident!("{name}_blocking");
                    quote! {
                        #doc_comment
                        pub async fn #name(&self, #(#params),*) -> Result<#fn_return, gluon_wire::GluonSendError> {
                            let this = self.clone();
                            tokio::task::spawn_blocking(move ||
                                this.#blocking_name(#(#param_names),*)
                            ).await.unwrap()
                        }
                        pub fn #blocking_name(&self, #(#params),*) -> Result<#fn_return, gluon_wire::GluonSendError> {
                            let mut gluon_builder = gluon_wire::GluonDataBuilder::new();
                            #(#params_write)*
                            let reader = self.obj.device().transact_blocking(&self.obj, #i, gluon_builder.to_payload())?.1;
                            let mut reader = gluon_wire::GluonDataReader::from_payload(reader);
                            Ok(#return_tuple)
                        }
                    }
                }
                None => quote! {
                    #doc_comment
                    pub fn #name(&self, #(#params),*) -> Result<(), gluon_wire::GluonSendError> {
                        let mut builder = gluon_wire::GluonDataBuilder::new();
                        #(#params_write)*
                        self.obj.device().transact_one_way(&self.obj, #i, builder.to_payload())?;
                        Ok(())
                    }
                },
            }
        });
        quote! {
            #[derive(Debug, Clone)]
            pub struct #name {
                obj: binderbinder::binder_object::BinderObjectOrRef,
                drop_notification: std::sync::Arc<binderbinder::binder_object::BinderObject<gluon_wire::drop_tracking::DropNotifiedHandler>>,
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
                pub fn from_handler<H: #handler_name>(obj: &std::sync::Arc<binderbinder::binder_object::BinderObject<H>>) -> #name {
                    #name::from_object_or_ref(binderbinder::binder_object::ToBinderObjectOrRef::to_binder_object_or_ref(obj))
                }
                #[doc = "only use this when you know the binder ref implements this interface, else the consquences are for you to find out"]
                pub fn from_object_or_ref(obj: binderbinder::binder_object::BinderObjectOrRef) -> #name {
                    let drop_notification = obj.device().register_object(gluon_wire::drop_tracking::DropNotifiedHandler::new());
                    let mut builder = gluon_wire::GluonDataBuilder::new();
                    builder.write_binder(&drop_notification);
                    _ = obj.device().transact_one_way(&obj, 4, builder.to_payload());
                    #name {
                        obj,
                        drop_notification,
                    }
                }
                pub fn death_or_drop(&self) -> impl Future<Output=()> + Send + Sync + 'static {
                    let death_notification_future = match &self.obj {
                        binderbinder::binder_object::BinderObjectOrRef::Ref(r) => Some(r.death_notification()),
                        binderbinder::binder_object::BinderObjectOrRef::WeakRef(r) => Some(r.death_notification()),
                        _ => None
                    };
                    let drop_notification = self.drop_notification.clone();
                    async move {
                        if let Some(death) = death_notification_future {
                            tokio::select! {
                                _ = death => {}
                                _ = drop_notification.wait() => {}
                            }
                        } else {
                            drop_notification.wait().await;
                        }
                    }
                }
            }
            impl binderbinder::binder_object::ToBinderObjectOrRef for #name {
                fn to_binder_object_or_ref(&self) -> binderbinder::binder_object::BinderObjectOrRef {
                    self.obj.to_binder_object_or_ref()
                }
            }
        }
    };
    quote! {
        #proxy
        #handler
    }
}

pub fn gen_struct(def: &StructDef, gen_ctx: &GenCtx) -> proc_macro2::TokenStream {
    let fields = def.fields.iter().map(|f| gen_field_struct(f, gen_ctx));
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
        quote! {
            impl gluon_wire::GluonConvertable for #name {
                fn write<'a, 'b: 'a>(
                    &'b self,
                    gluon_data: &mut gluon_wire::GluonDataBuilder<'a>,
                ) -> Result<(), gluon_wire::GluonWriteError> {
                    #(self.#field_names.write(gluon_data)?;)*
                    Ok(())
                }

                fn read(gluon_data: &mut gluon_wire::GluonDataReader) -> Result<Self, gluon_wire::GluonReadError> {
                    #(let #field_names = gluon_wire::GluonConvertable::read(gluon_data)?;)*
                    Ok(#name {#(#field_names,)*})
                }

                fn write_owned(self, gluon_data: &mut gluon_wire::GluonDataBuilder<'_>) -> Result<(), gluon_wire::GluonWriteError> {
                    #(self.#field_names.write_owned(gluon_data)?;)*
                    Ok(())
                }
            }
        }
    };
    quote! {
        #[doc = #doc]
        #[derive(Debug, #(#derives),*)]
        pub struct #name {
            #(#fields)*
        }

        #gluon_trait_impl
    }
}

pub fn gen_enum(def: &EnumDef, gen_ctx: &GenCtx) -> proc_macro2::TokenStream {
    let variants = def.variants.iter().map(|variant| {
        let fields = variant.fields.iter().map(|f| gen_field_enum(f, gen_ctx));
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
    let name = def.name.to_case(Case::Pascal);
    let derives = derives_to_tokens(enum_supported_derives(def, gen_ctx));
    let enum_name = format_ident!("{}", name);
    let doc = &def.doc;
    let gluon_trait_impl = {
        let write_variants = def.variants.iter().enumerate().map(|(i, variant)| {
            let field_names = variant
                .fields
                .iter()
                .map(|v| format_ident!("{}", v.name.to_case(Case::Snake)))
                .collect::<Vec<_>>();
            let name = format_ident!("{}", variant.name.to_case(Case::Pascal));
            let i = i as u16;
            if field_names.is_empty() {
                quote! {
                    #enum_name::#name => {
                        gluon_data.write_u16(#i)?;
                    },
                }
            } else {
                quote! {
                    #enum_name::#name { #(#field_names),* } => {
                        gluon_data.write_u16(#i)?;
                        #(#field_names.write(gluon_data)?;)*
                    },
                }
            }
        });
        let write_owned_variants = def.variants.iter().enumerate().map(|(i, variant)| {
            let field_names = variant
                .fields
                .iter()
                .map(|v| format_ident!("{}", v.name.to_case(Case::Snake)))
                .collect::<Vec<_>>();
            let name = format_ident!("{}", variant.name.to_case(Case::Pascal));
            let i = i as u16;
            if field_names.is_empty() {
                quote! {
                    #enum_name::#name => {
                        gluon_data.write_u16(#i)?;
                    },
                }
            } else {
                quote! {
                    #enum_name::#name { #(#field_names),* } => {
                        gluon_data.write_u16(#i)?;
                        #(#field_names.write_owned(gluon_data)?;)*
                    },
                }
            }
        });
        let read_variants = def.variants.iter().enumerate().map(|(i, variant)| {
            let field_names = variant
                .fields
                .iter()
                .map(|v| format_ident!("{}", v.name.to_case(Case::Snake)))
                .collect::<Vec<_>>();
            let name = format_ident!("{}", variant.name.to_case(Case::Pascal));
            let i = i as u16;
            if variant.fields.is_empty() {
                quote! {
                    #i => {
                        #enum_name::#name
                    },
                }
            } else {
                quote! {
                    #i => {
                        #(let #field_names = gluon_wire::GluonConvertable::read(gluon_data)?;)*
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
        pub enum #enum_name {
            #(#variants),*
        }

        #gluon_trait_impl
    }
}

pub fn gen_field_enum(def: &Field, gen_ctx: &GenCtx) -> proc_macro2::TokenStream {
    let type_def = gen_type(&def.ty, gen_ctx);
    let name = format_ident!("{}", def.name.to_case(Case::Snake));
    let doc_comment = def.doc.as_ref().map(|str| quote! {#[doc = #str]});
    quote! {
        #doc_comment
        #name: #type_def,
    }
}
pub fn gen_field_struct(def: &Field, gen_ctx: &GenCtx) -> proc_macro2::TokenStream {
    let type_def = gen_type(&def.ty, gen_ctx);
    let name = format_ident!("{}", def.name.to_case(Case::Snake));
    let doc_comment = def.doc.as_ref().map(|str| quote! {#[doc = #str]});
    quote! {
        #doc_comment
        pub #name: #type_def,
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
    def.fields
        .iter()
        .map(|f| supported_derives(&f.ty, gen_ctx))
        .fold(gen_ctx.requested_derives, |acc, d| acc & d)
}

fn enum_supported_derives(def: &EnumDef, gen_ctx: &GenCtx) -> Derives {
    def.variants
        .iter()
        .flat_map(|v| v.fields.iter())
        .map(|f| supported_derives(&f.ty, gen_ctx))
        .fold(gen_ctx.requested_derives, |acc, d| acc & d)
}

/// Returns which of the requested derives this type can support.
pub fn supported_derives(def: &Type, gen_ctx: &GenCtx) -> Derives {
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
        Type::Custom(custom) => custom_type_derives(custom, gen_ctx),
        Type::Array(v, _) => supported_derives(v, gen_ctx),
        Type::Vec(v) => supported_derives(v, gen_ctx) - Derives::COPY,
        Type::Set(v) => supported_derives(v, gen_ctx),
        Type::Option(v) => supported_derives(v, gen_ctx),
        // TODO: figure out correct semantics
        Type::Result(_, _) => Derives::empty(),
        // TODO: figure out correct semantics
        Type::Map(_, _) => Derives::empty(),
    }
}

fn custom_type_derives(custom: &CustomType, gen_ctx: &GenCtx) -> Derives {
    match custom {
        CustomType::Named(name) => derives_from_protocol(name, gen_ctx),
        CustomType::Qualified(namespace, type_name) => {
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
                return derives_from_protocol(
                    type_name,
                    &GenCtx {
                        curr_protocol: v,
                        ..*gen_ctx
                    },
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

fn derives_from_protocol(type_name: &str, gen_ctx: &GenCtx) -> Derives {
    gen_ctx
        .curr_protocol
        .enums
        .iter()
        .find(|(n, _)| n == type_name)
        .map(|(_, v)| {
            v.variants
                .iter()
                .flat_map(|v| v.fields.iter())
                .map(|v| supported_derives(&v.ty, gen_ctx))
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
                        .map(|v| supported_derives(&v.ty, gen_ctx))
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
        .unwrap_or_else(|| panic!("unknown type: {type_name}"))
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
