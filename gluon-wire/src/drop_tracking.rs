use binderbinder::{
    TransactionHandler,
    binder_object::{BinderObjectOrRef, ToBinderObjectOrRef},
    device::Transaction,
    payload::PayloadBuilder,
};
use tokio::sync::Notify;

#[derive(Debug)]
pub struct DropNotifier(BinderObjectOrRef);
impl DropNotifier {
    pub fn new(obj: &impl ToBinderObjectOrRef) -> Self {
        Self(obj.to_binder_object_or_ref())
    }
}
impl Drop for DropNotifier {
    fn drop(&mut self) {
        _ = self
            .0
            .device()
            .transact_one_way(&self.0, 4, PayloadBuilder::new());
    }
}
#[derive(Debug)]
pub struct DropNotifiedHandler {
    notify: Notify,
}
impl DropNotifiedHandler {
    pub fn new() -> Self {
        Self {
            notify: Notify::new(),
        }
    }
    pub async fn wait(&self) {
        self.notify.notified().await
    }
}
impl TransactionHandler for DropNotifiedHandler {
    async fn handle(&self, _transaction: Transaction) -> PayloadBuilder<'_> {
        PayloadBuilder::new()
    }

    async fn handle_one_way(&self, transaction: Transaction) {
        // should this also check for a magic number in the payload?
        if transaction.code == 4 {
            self.notify.notify_waiters();
        }
    }
}
