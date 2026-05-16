use crate::protocol::test::{Test, TestHandler};
use binderbinder::BinderDevice;
use gluon_wire::GluonCtx;
use std::{
    hash::{DefaultHasher, Hash},
    process,
    sync::Arc,
};

mod protocol;

/// Proxy type for the wire `TestEnum` — demonstrates method-signature proxying.
pub struct MyTestEnum(protocol::test::TestEnum);

/// Proxy type for the wire `Color` — demonstrates proxy propagation into struct fields.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MyColor {
    Red,
    Green,
    Blue,
}

impl From<protocol::test::TestEnum> for MyTestEnum {
    fn from(v: protocol::test::TestEnum) -> Self {
        MyTestEnum(v)
    }
}
impl From<MyTestEnum> for protocol::test::TestEnum {
    fn from(v: MyTestEnum) -> Self {
        v.0
    }
}

impl From<protocol::test::Color> for MyColor {
    fn from(c: protocol::test::Color) -> Self {
        match c {
            protocol::test::Color::Red => MyColor::Red,
            protocol::test::Color::Green => MyColor::Green,
            protocol::test::Color::Blue => MyColor::Blue,
        }
    }
}
impl From<MyColor> for protocol::test::Color {
    fn from(c: MyColor) -> Self {
        match c {
            MyColor::Red => protocol::test::Color::Red,
            MyColor::Green => protocol::test::Color::Green,
            MyColor::Blue => protocol::test::Color::Blue,
        }
    }
}

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

    async fn echo(&self, _ctx: GluonCtx, input: MyTestEnum) -> MyTestEnum {
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

#[cfg(test)]
mod tests {
    use super::*;

    // Palette.primary/secondary are MyColor (proxy), not the wire Color — verified by type.
    #[test]
    fn palette_fields_use_proxy_type() {
        let palette = protocol::test::Palette {
            primary: MyColor::Red,
            secondary: MyColor::Blue,
        };
        assert_eq!(palette.primary, MyColor::Red);
        assert_eq!(palette.secondary, MyColor::Blue);
    }

    #[test]
    fn my_color_from_wire_round_trips() {
        use protocol::test::Color;
        for (wire, expected) in [
            (Color::Red, MyColor::Red),
            (Color::Green, MyColor::Green),
            (Color::Blue, MyColor::Blue),
        ] {
            let proxy = MyColor::from(wire);
            assert_eq!(proxy, expected);
            let back = Color::from(proxy);
            assert_eq!(MyColor::from(back), expected);
        }
    }

    #[test]
    fn palette_copy_semantics() {
        // Palette derives Copy because MyColor declares Derives::COPY.
        let a = protocol::test::Palette {
            primary: MyColor::Green,
            secondary: MyColor::Red,
        };
        let b = a; // copy
        let _ = a; // still valid
        assert_eq!(b.primary, MyColor::Green);
    }
}
