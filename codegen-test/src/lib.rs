use crate::protocol::test::TestHandler;
use gluon_wire::{GluonCtx, impl_transaction_handler};
use std::{
    hash::{DefaultHasher, Hash},
    process,
};

mod protocol;

#[allow(unused)]
#[derive(Debug)]
struct TestHandlerImpl {}

impl TestHandler for TestHandlerImpl {
    async fn quit(&self, _ctx: GluonCtx) {
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
}
impl_transaction_handler!(TestHandlerImpl);
