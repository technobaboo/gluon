use crate::protocol::test::TestHandler;
use binderbinder::{TransactionHandler, device::Transaction, payload::PayloadBuilder};
use gluon_wire::{GluonCtx, GluonDataReader, drop_tracking::DropNotifier};
use std::{
    hash::{DefaultHasher, Hash},
    process,
};
use tokio::sync::RwLock;

mod protocol;

#[allow(unused)]
#[derive(Debug)]
struct TestHandlerImpl {
    drop_notifications: RwLock<Vec<DropNotifier>>,
}

impl TestHandler for TestHandlerImpl {
    fn quit(&self, _ctx: GluonCtx) {
        process::exit(0);
    }

    async fn ping(&self, _ctx: GluonCtx) {
        println!("got ping");
        let mut hasher = DefaultHasher::new();
        c"nya~".to_owned().hash(&mut hasher);
    }

    async fn echo(
        &self,
        _ctx: GluonCtx,
        input: protocol::test::TestEnum,
    ) -> protocol::test::TestEnum {
        println!("echoing: {input:?}");
        input
    }

    async fn echo_ref(&self, _ctx: GluonCtx, input: protocol::test::Test) -> protocol::test::Test {
        input
    }

    async fn get_position(&self, _ctx: GluonCtx) -> protocol::types::Vec3 {
        protocol::types::Vec3 {
            x: 0.0,
            y: 0.0,
            z: 0.0,
        }
    }

    async fn drop_notification_requested(&self, notifier: DropNotifier) {
        self.drop_notifications.write().await.push(notifier);
    }
}
impl TransactionHandler for TestHandlerImpl {
    async fn handle(&self, transaction: Transaction) -> PayloadBuilder<'_> {
        let mut reader = GluonDataReader::from_payload(transaction.payload);
        self.dispatch_two_way(
            transaction.code,
            &mut reader,
            GluonCtx {
                sender_pid: transaction.sender_pid,
                sender_euid: transaction.sender_euid,
            },
        )
        .await
        .unwrap()
        .to_payload()
    }

    async fn handle_one_way(&self, transaction: Transaction) {
        let mut reader = GluonDataReader::from_payload(transaction.payload);
        self.dispatch_one_way(
            transaction.code,
            &mut reader,
            GluonCtx {
                sender_pid: transaction.sender_pid,
                sender_euid: transaction.sender_euid,
            },
        )
        .await
        .unwrap()
    }
}
