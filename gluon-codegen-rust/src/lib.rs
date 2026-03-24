use convert_case::{Case, Casing};
use gluon_parser::{EnumDef, Field, Interface, Protocol, StructDef, Type};
use quote::{format_ident, quote};
pub fn gen_module(proto: &Protocol) -> proc_macro2::TokenStream {
    let interfaces = proto
        .interfaces
        .iter()
        .map(|(name, interface)| gen_interface(name, interface));
    let structs = proto.structs.iter().map(|(_name, def)| gen_struct(def));
    let enums = proto.enums.iter().map(|(_name, def)| gen_enum(def));
    let imports = proto.imports.iter().map(|def| {
        dbg!(&def);
        let path = def.path.split("::").map(|v| format_ident!("{}", v));
        quote! {use #(#path)::*;}
    });
    quote! {
        #![allow(unused, clippy::single_match, clippy::match_single_binding)]
        use gluon_wire::GluonConvertable;
        #(#imports)*
        #(#interfaces)*
        #(#structs)*
        #(#enums)*
    }
}
pub fn gen_interface(interface_name: &str, def: &Interface) -> proc_macro2::TokenStream {
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
                    .map(|_| quote! {gluon_wire::GluonConvertable::read(data)?});
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
                    .map(|_| quote! {gluon_wire::GluonConvertable::read(data)?});
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
                let type_def = gen_type(&param.ty);
                let name = format_ident!("{}", param.name.to_case(Case::Snake));
                quote! {
                    #name: #type_def
                }
            });
            let name = format_ident!("{}", method.name.to_case(Case::Snake));
            let doc_comment = method.doc.as_ref().map(|str| quote! {#[doc = #str]});
            // TODO: gen return docs and names into main fn docs?
            let return_types = method
                .returns
                .as_ref()
                .map(|v| v.iter().map(|v| gen_type(&v.ty)).collect::<Vec<_>>());
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
                fn dispatch_two_way(&self, transaction_code: u32, data: &mut gluon_wire::GluonDataReader, ctx: gluon_wire::GluonCtx) -> impl Future<Output=Result<gluon_wire::GluonDataBuilder<'static>, gluon_wire::GluonSendError>> + Send + Sync {
                    async move {
                        let mut out = gluon_wire::GluonDataBuilder::new();
                        match transaction_code {
                            #(#methods_dispatch)*
                            _ => {}
                        }
                        Ok(out)
                    }
                }
                fn dispatch_one_way(&self, transaction_code: u32, data: &mut gluon_wire::GluonDataReader, ctx: gluon_wire::GluonCtx) -> impl Future<Output=Result<(),gluon_wire::GluonSendError>> + Send + Sync {
                    async move {
                        match transaction_code {
                            4 => {
                                let Ok(obj) = data.read_binder() else { return Ok(()); };
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
                let type_def = gen_type(&param.ty);
                let name = format_ident!("{}", param.name.to_case(Case::Snake));
                quote! {
                    #name: #type_def
                }
            }).collect::<Vec<_>>();
            let params_write = method.params.iter().map(|param| {
                let name = format_ident!("{}", param.name.to_case(Case::Snake));
                quote! {#name.write(&mut builder)?;}
            }).collect::<Vec<_>>();
            let name = format_ident!("{}", method.name.to_case(Case::Snake));
            let doc_comment = method.doc.as_ref().map(|str| quote! {#[doc = #str]});
            let return_types = method
                .returns
                .as_ref()
                .map(|v| v.iter().map(|v| gen_type(&v.ty)).collect::<Vec<_>>());
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
                            let obj = binderbinder::binder_object::ToBinderObjectOrRef::to_binder_object_or_ref(&self.obj);
                            tokio::task::spawn_blocking(move || {
                                let mut builder = gluon_wire::GluonDataBuilder::new();
                                #(#params_write)*
                                let reader = obj.device().transact_blocking(&obj, #i, builder.to_payload())?.1;
                                let mut reader = gluon_wire::GluonDataReader::from_payload(reader);
                                Ok(#return_tuple)
                            }).await.unwrap()
                        }
                        pub fn #blocking_name(&self, #(#params),*) -> Result<#fn_return, gluon_wire::GluonSendError> {
                            let mut builder = gluon_wire::GluonDataBuilder::new();
                            #(#params_write)*
                            let reader = self.obj.device().transact_blocking(&self.obj, #i, builder.to_payload())?.1;
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
                    data: &mut gluon_wire::GluonDataBuilder<'a>,
                ) -> Result<(), gluon_wire::GluonWriteError> {
                    self.obj.write(data)
                }

                fn read(data: &mut gluon_wire::GluonDataReader) -> Result<Self, gluon_wire::GluonReadError> {
                    let obj = binderbinder::binder_object::BinderObjectOrRef::read(data)?;
                    Ok(#name::from_object_or_ref(obj))
                }

                fn write_owned(self, data: &mut gluon_wire::GluonDataBuilder<'_>) -> Result<(), gluon_wire::GluonWriteError> {
                    self.obj.write_owned(data)
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

pub fn gen_struct(def: &StructDef) -> proc_macro2::TokenStream {
    let fields = def.fields.iter().map(gen_field_struct);
    let name = def.name.to_case(Case::Pascal);
    let (derive, name) = derive_from_name(&name);
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
                    data: &mut gluon_wire::GluonDataBuilder<'a>,
                ) -> Result<(), gluon_wire::GluonWriteError> {
                    #(self.#field_names.write(data)?;)*
                    Ok(())
                }

                fn read(data: &mut gluon_wire::GluonDataReader) -> Result<Self, gluon_wire::GluonReadError> {
                    #(let #field_names = gluon_wire::GluonConvertable::read(data)?;)*
                    Ok(#name {#(#field_names,)*})
                }

                fn write_owned(self, data: &mut gluon_wire::GluonDataBuilder<'_>) -> Result<(), gluon_wire::GluonWriteError> {
                    #(self.#field_names.write_owned(data)?;)*
                    Ok(())
                }
            }
        }
    };
    quote! {
        #[doc = #doc]
        #derive
        pub struct #name {
            #(#fields)*
        }

        #gluon_trait_impl
    }
}

pub fn gen_enum(def: &EnumDef) -> proc_macro2::TokenStream {
    let variants = def.variants.iter().map(|variant| {
        let fields = variant.fields.iter().map(gen_field_enum);
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
    let (derive, name) = derive_from_name(&name);
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
                        data.write_u16(#i)?;
                    },
                }
            } else {
                quote! {
                    #enum_name::#name { #(#field_names),* } => {
                        data.write_u16(#i)?;
                        #(#field_names.write(data)?;)*
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
                        data.write_u16(#i)?;
                    },
                }
            } else {
                quote! {
                    #enum_name::#name { #(#field_names),* } => {
                        data.write_u16(#i)?;
                        #(#field_names.write_owned(data)?;)*
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
                        #(let #field_names = gluon_wire::GluonConvertable::read(data)?;)*
                        #enum_name::#name { #(#field_names,)* }
                    },
                }
            }
        });
        quote! {
            impl gluon_wire::GluonConvertable for #enum_name {
                fn write<'a, 'b: 'a>(
                    &'b self,
                    data: &mut gluon_wire::GluonDataBuilder<'a>,
                ) -> Result<(), gluon_wire::GluonWriteError> {
                    match self {
                        #(#write_variants)*
                    };
                    Ok(())
                }

                fn read(data: &mut gluon_wire::GluonDataReader) -> Result<Self, gluon_wire::GluonReadError> {
                    Ok(match data.read_u16()? {
                        #(#read_variants)*
                        v => return Err(gluon_wire::GluonReadError::UnknownEnumVariant(v)),
                    })
                }

                fn write_owned(self, data: &mut gluon_wire::GluonDataBuilder<'_>) -> Result<(), gluon_wire::GluonWriteError> {
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
        #derive
        pub enum #enum_name {
            #(#variants),*
        }

        #gluon_trait_impl
    }
}

fn derive_from_name(name: &str) -> (proc_macro2::TokenStream, &str) {
    if let Some(str) = name.strip_suffix("CloneHash") {
        (
            quote! {
                #[derive(Clone, Hash, Debug)]
            },
            str,
        )
    } else if let Some(str) = name.strip_suffix("Clone") {
        (
            quote! {
                #[derive(Clone, Debug)]
            },
            str,
        )
    } else if let Some(str) = name.strip_suffix("Hash") {
        (
            quote! {
                #[derive(Hash, Debug)]
            },
            str,
        )
    } else {
        (quote! {#[derive(Debug)]}, name)
    }
}

pub fn gen_field_enum(def: &Field) -> proc_macro2::TokenStream {
    let type_def = gen_type(&def.ty);
    let name = format_ident!("{}", def.name.to_case(Case::Snake));
    let doc_comment = def.doc.as_ref().map(|str| quote! {#[doc = #str]});
    quote! {
        #doc_comment
        #name: #type_def,
    }
}
pub fn gen_field_struct(def: &Field) -> proc_macro2::TokenStream {
    let type_def = gen_type(&def.ty);
    let name = format_ident!("{}", def.name.to_case(Case::Snake));
    let doc_comment = def.doc.as_ref().map(|str| quote! {#[doc = #str]});
    quote! {
        #doc_comment
        pub #name: #type_def,
    }
}

pub fn gen_type(def: &Type) -> proc_macro2::TokenStream {
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
            Some(name) => {
                let name = format_ident!("{}", name.to_case(Case::Pascal));
                quote! {#name}
            }
            None => quote! {binderbinder::binder_object::BinderObjectOrRef},
        },
        Type::Named(name) => {
            let name = format_ident!("{}", name.to_case(Case::Pascal));
            quote! {#name}
        }
        Type::Qualified(space, name) => {
            let space = format_ident!("{}", space.to_case(Case::Snake));
            let name = format_ident!("{}", name.to_case(Case::Pascal));
            quote! {#space::#name}
        }
        Type::Array(type_def, len) => {
            let type_def = gen_type(type_def);
            quote! {[#type_def; #len]}
        }
        Type::Vec(type_def) => {
            let type_def = gen_type(type_def);
            quote! {Vec<#type_def>}
        }
        Type::Set(type_def) => {
            let type_def = gen_type(type_def);
            quote! {std::collections::hash::HashSet<#type_def>}
        }
        Type::Map(key, value) => {
            let key = gen_type(key);
            let value = gen_type(value);
            quote! {std::collections::hash::HashMap<#key,#value>}
        }
        Type::Option(type_def) => {
            let type_def = gen_type(type_def);
            quote! {Option<#type_def>}
        }
        Type::Result(ok, err) => {
            let ok = gen_type(ok);
            let err = gen_type(err);
            quote! {Result<#ok, #err>}
        }
    }
}
