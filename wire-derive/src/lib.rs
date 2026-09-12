use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

/// Implements `gluon_ipc::Handler` for a type that implements a
/// generated `{Name}Handler` trait (which provides `dispatch_one_way`).
///
/// strong-ipc delivers a message as raw bytes plus descriptors, so this is where the
/// transaction code is split back off the front of the payload and the peer's
/// credentials become a `gluon_ipc::Context`.
#[proc_macro_derive(Handler)]
pub fn derive_handler(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    // A message too short to carry a code isn't ours; there is nothing to dispatch on
    // and no reply object to report to, so it can only be dropped.
    #[cfg(feature = "tracing")]
    let on_malformed = quote! {
        tracing::error!(
            error = %gluon_err,
            concat!("dropped a malformed message for ", stringify!(#name)),
        );
        return;
    };
    #[cfg(not(feature = "tracing"))]
    let on_malformed = quote! { let _ = gluon_err; return; };

    #[cfg(feature = "tracing")]
    let dispatch = quote! {
        _ = self
            .dispatch_one_way(gluon_code, gluon_data, gluon_ipc::Context::new(creds))
            .await
            .inspect_err(|err| {
                tracing::error!(
                    concat!("failed to dispatch one_way {} for ", stringify!(#name)),
                    err
                )
            });
    };
    #[cfg(not(feature = "tracing"))]
    let dispatch = quote! {
        _ = self
            .dispatch_one_way(gluon_code, gluon_data, gluon_ipc::Context::new(creds))
            .await;
    };

    quote! {
        impl #impl_generics gluon_ipc::Handler for #name #ty_generics #where_clause {
            async fn handle(
                &self,
                data: &mut [u8],
                fds: gluon_ipc::FdVec,
                creds: Option<gluon_ipc::UCred>,
            ) {
                let (gluon_code, gluon_data) = match gluon_ipc::DataReader::from_wire(data, fds) {
                    Ok(split) => split,
                    Err(gluon_err) => { #on_malformed }
                };
                #dispatch
            }
        }
    }
    .into()
}
