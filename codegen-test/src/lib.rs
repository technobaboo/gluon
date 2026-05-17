use crate::protocol::test::TestHandler;
use gluon::Context;
use std::{
    hash::{DefaultHasher, Hash},
    process,
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

/// Proxy type for the wire `types::Vec3` — demonstrates cross-protocol proxying.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct MyVec3 {
    pub x: f32,
    pub y: f32,
    pub z: f32,
}

impl From<protocol::types::Vec3> for MyVec3 {
    fn from(v: protocol::types::Vec3) -> Self {
        MyVec3 {
            x: v.x,
            y: v.y,
            z: v.z,
        }
    }
}
impl From<MyVec3> for protocol::types::Vec3 {
    fn from(v: MyVec3) -> Self {
        protocol::types::Vec3 {
            x: v.x,
            y: v.y,
            z: v.z,
        }
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
#[derive(Debug, gluon::Handler)]
struct TestHandlerImpl {}

impl TestHandler for TestHandlerImpl {
    async fn quit(&self, _ctx: Context) {
        process::exit(0);
    }

    async fn ping(&self, _ctx: Context) {
        println!("got ping");
        let mut hasher = DefaultHasher::new();
        c"nya~".to_owned().hash(&mut hasher);
    }

    async fn echo(&self, _ctx: Context, input: MyTestEnum) -> MyTestEnum {
        input
    }

    async fn echo_ref(&self, _ctx: Context, input: protocol::test::Test) -> protocol::test::Test {
        input
    }

    async fn echo_untyped_ref(
        &self,
        _ctx: gluon::Context,
        input: gluon::ObjectOrRef,
    ) -> gluon::ObjectOrRef {
        input
    }

    async fn get_position(&self, _ctx: Context) -> MyVec3 {
        MyVec3 {
            x: 1.0,
            y: 2.0,
            z: 3.0,
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
    fn option_proxy_field_type() {
        // MaybeColor.color should be Option<MyColor>, not Option<wire Color>
        let mc = protocol::test::MaybeColor {
            color: Some(MyColor::Green),
        };
        assert_eq!(mc.color, Some(MyColor::Green));
        let none = protocol::test::MaybeColor { color: None };
        assert_eq!(none.color, None);
    }

    #[test]
    fn cross_protocol_vec3_round_trips() {
        use protocol::types::Vec3;
        let wire = Vec3 {
            x: 1.0,
            y: 2.0,
            z: 3.0,
        };
        let proxy = MyVec3::from(wire);
        assert_eq!(
            proxy,
            MyVec3 {
                x: 1.0,
                y: 2.0,
                z: 3.0
            }
        );
        let back = Vec3::from(proxy);
        assert_eq!(back.x, 1.0);
        assert_eq!(back.y, 2.0);
        assert_eq!(back.z, 3.0);
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
