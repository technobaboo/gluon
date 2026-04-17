use std::sync::Arc;

use binderbinder::{
    TransactionHandler,
    binder_object::{BinderObjectOrRef, ToBinderObjectOrRef},
    device::Transaction,
    payload::PayloadBuilder,
};
use tokio::sync::Notify;

#[derive(Debug)]
pub struct DropNotifier {
    obj: BinderObjectOrRef,
    active: bool,
}
impl DropNotifier {
    pub fn new(obj: &impl ToBinderObjectOrRef) -> Self {
        Self {
            obj: obj.to_binder_object_or_ref(),
            active: true,
        }
    }
    pub fn abort(&mut self) {
        self.active = false;
    }
}
impl Drop for DropNotifier {
    fn drop(&mut self) {
        if self.active {
            _ = self
                .obj
                .device()
                .transact_one_way(&self.obj, 4, PayloadBuilder::new());
        }
    }
}
#[derive(Debug)]
pub struct DropNotifiedHandler {
    notify: Notify,
    target: BinderObjectOrRef,
}
impl Drop for DropNotifiedHandler {
    fn drop(&mut self) {
        _ = self
            .target
            .device()
            .transact_one_way(&self.target, 5, PayloadBuilder::new());
    }
}
impl DropNotifiedHandler {
    pub fn new(target: BinderObjectOrRef) -> Arc<Self> {
        Arc::new(Self {
            notify: Notify::new(),
            target,
        })
    }
    pub async fn wait(&self) {
        self.notify.notified().await
    }
}
impl TransactionHandler for DropNotifiedHandler {
    async fn handle(self: Arc<Self>, _transaction: Transaction) -> PayloadBuilder<'static> {
        PayloadBuilder::new()
    }

    async fn handle_one_way(self: Arc<Self>, transaction: Transaction) {
        // should this also check for a magic number in the payload?
        if transaction.code == 4 {
            self.notify.notify_waiters();
        }
    }
}
