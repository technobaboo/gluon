use proc_macro::TokenStream;
use quote::quote;
use syn::{DeriveInput, parse_macro_input};

/// Implements `binderbinder::TransactionHandler` for a type that implements a
/// generated `{Name}Handler` trait (which provides `dispatch_one_way`).
///
/// Equivalent to the `impl_transaction_handler!` declarative macro but usable
/// as a `#[derive]`.
#[proc_macro_derive(Handler)]
pub fn derive_handler(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;

    #[cfg(feature = "tracing")]
    let handle = quote! {
        async fn handle(
            self: std::sync::Arc<Self>,
            _transaction: binderbinder::device::Transaction,
        ) -> binderbinder::payload::PayloadBuilder<'static> {
            tracing::warn!(concat!(
                "Received two way transaction for ",
                stringify!(#name)
            ));
            binderbinder::payload::PayloadBuilder::new()
        }
    };
    #[cfg(not(feature = "tracing"))]
    let handle = quote! {
        async fn handle(
            self: std::sync::Arc<Self>,
            _transaction: binderbinder::device::Transaction,
        ) -> binderbinder::payload::PayloadBuilder<'static> {
            binderbinder::payload::PayloadBuilder::new()
        }
    };

    #[cfg(feature = "tracing")]
    let handle_one_way = quote! {
        async fn handle_one_way(
            self: std::sync::Arc<Self>,
            transaction: binderbinder::device::Transaction,
        ) {
            let gluon_data = gluon_wire::GluonDataReader::from_payload(transaction.payload);
            _ = self
                .dispatch_one_way(
                    transaction.code,
                    gluon_data,
                    gluon_wire::GluonCtx {
                        sender_pid: transaction.sender_pid,
                        sender_euid: transaction.sender_euid,
                    },
                )
                .await
                .inspect_err(|err| {
                    tracing::error!(
                        concat!("failed to dispatch one_way {} for ", stringify!(#name)),
                        err
                    )
                });
        }
    };
    #[cfg(not(feature = "tracing"))]
    let handle_one_way = quote! {
        async fn handle_one_way(
            self: std::sync::Arc<Self>,
            transaction: binderbinder::device::Transaction,
        ) {
            let gluon_data = gluon_wire::GluonDataReader::from_payload(transaction.payload);
            _ = self
                .dispatch_one_way(
                    transaction.code,
                    gluon_data,
                    gluon_wire::GluonCtx {
                        sender_pid: transaction.sender_pid,
                        sender_euid: transaction.sender_euid,
                    },
                )
                .await;
        }
    };

    quote! {
        impl binderbinder::TransactionHandler for #name {
            #handle
            #handle_one_way
        }
    }
    .into()
}
