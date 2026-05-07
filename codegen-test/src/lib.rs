use crate::protocol::test::{Test, TestHandler};
use binderbinder::BinderDevice;
use gluon_wire::GluonCtx;
use std::{
    hash::{DefaultHasher, Hash},
    process,
    sync::Arc,
};

mod protocol;

#[allow(unused)]
#[derive(Debug, gluon_wire::Handler)]
struct TestHandlerImpl {}

#[expect(unused)]
fn a(dev: Arc<BinderDevice>) {
    let v = dev.register_object(TestHandlerImpl {});
    let handler = Test::from_handler(&v);
}

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
