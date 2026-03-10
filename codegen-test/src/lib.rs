use std::{
    hash::{DefaultHasher, Hash},
    process,
};

use binderbinder::{TransactionHandler, device::Transaction, payload::PayloadBuilder};
use gluon_wire::{GluonDataReader, drop_tracking::DropNotifier};
use tokio::sync::RwLock;

use crate::protocol::TestHandler;

mod protocol;
#[derive(Debug)]
struct TestHandlerImpl {
    drop_notifications: RwLock<Vec<DropNotifier>>,
}

impl TestHandler for TestHandlerImpl {
    fn quit(&self) {
        process::exit(0);
    }

    async fn ping(&self) {
        println!("got ping");
        let mut hasher = DefaultHasher::new();
        c"nya~".to_owned().hash(&mut hasher);
    }

    async fn echo(&self, input: String) -> String {
        println!("echoing: {input}");
        input
    }

    async fn echo_ref(&self, input: protocol::Test2) -> protocol::Test2 {
        assert_eq!(
            input.echo("Hello World".to_string()).await,
            "Hello World".to_string()
        );
        input
    }

    async fn drop_notification_requested(
        &self,
        notifier: DropNotifier,
    ) {
        self.drop_notifications.write().await.push(notifier);
    }
}
impl TransactionHandler for TestHandlerImpl {
    async fn handle(&self, transaction: Transaction) -> PayloadBuilder<'_> {
        let mut reader = GluonDataReader::from_payload(transaction.payload);
        self.dispatch_two_way(transaction.code, &mut reader)
            .await
            .to_payload()
    }

    async fn handle_one_way(&self, transaction: Transaction) {
        let mut reader = GluonDataReader::from_payload(transaction.payload);
        self.dispatch_one_way(transaction.code, &mut reader).await
    }
}
