use crate::protocol::test::TestHandler;
use gluon::Context;
use std::{
    hash::{DefaultHasher, Hash},
    process,
    sync::atomic::{AtomicI32, Ordering},
};

mod protocol;

/// Proxy type for the wire `TestEnum` — demonstrates method-signature proxying.
pub struct MyTestEnum(protocol::test::proxied::TestEnum);

/// Proxy type for the wire `Color` — demonstrates proxy propagation into struct fields.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MyColor {
    Red,
    Green,
    Blue,
}

impl From<protocol::test::proxied::TestEnum> for MyTestEnum {
    fn from(v: protocol::test::proxied::TestEnum) -> Self {
        MyTestEnum(v)
    }
}
impl From<MyTestEnum> for protocol::test::proxied::TestEnum {
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

impl From<protocol::types::proxied::Vec3> for MyVec3 {
    fn from(v: protocol::types::proxied::Vec3) -> Self {
        MyVec3 {
            x: v.x,
            y: v.y,
            z: v.z,
        }
    }
}
impl From<MyVec3> for protocol::types::proxied::Vec3 {
    fn from(v: MyVec3) -> Self {
        protocol::types::proxied::Vec3 {
            x: v.x,
            y: v.y,
            z: v.z,
        }
    }
}

impl From<protocol::test::proxied::Color> for MyColor {
    fn from(c: protocol::test::proxied::Color) -> Self {
        match c {
            protocol::test::proxied::Color::Red => MyColor::Red,
            protocol::test::proxied::Color::Green => MyColor::Green,
            protocol::test::proxied::Color::Blue => MyColor::Blue,
        }
    }
}
impl From<MyColor> for protocol::test::proxied::Color {
    fn from(c: MyColor) -> Self {
        match c {
            MyColor::Red => protocol::test::proxied::Color::Red,
            MyColor::Green => protocol::test::proxied::Color::Green,
            MyColor::Blue => protocol::test::proxied::Color::Blue,
        }
    }
}

#[allow(unused)]
#[derive(Debug, Default, gluon::Handler)]
struct TestHandlerImpl {
    /// pid the last `ping` came from, or 0 if the kernel reported no credentials —
    /// lets a test assert that `Context` really carries `SCM_CREDENTIALS` through
    last_ping_pid: AtomicI32,
}

impl TestHandler for TestHandlerImpl {
    async fn quit(&self, _ctx: Context) {
        process::exit(0);
    }

    async fn ping(&self, _ctx: Context) {
        self.last_ping_pid
            .store(_ctx.sender_pid().unwrap_or(0), Ordering::Relaxed);
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

    async fn echo_untyped_ref(&self, _ctx: gluon::Context, input: gluon::Ref) -> gluon::Ref {
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
    use gluon::RefExt;
    use protocol::test::Test;

    /// Stands up a handler and the proxy that reaches it.
    ///
    /// The node comes back alongside the proxy because nothing else holds it: drop it and
    /// its socket hangs up, so it has to outlive every call made through the proxy.
    fn test_node() -> (gluon::Node<TestHandlerImpl>, Test) {
        Test::new_node(TestHandlerImpl::default()).unwrap()
    }

    /// A handler already in an `Arc` goes in as-is — that's what the `Into<Arc<H>>` bound
    /// buys over a second `_raw` constructor.
    #[tokio::test]
    async fn a_shared_handler_needs_no_second_constructor() {
        let handler = std::sync::Arc::new(TestHandlerImpl::default());
        let (_node, proxy) = Test::new_node(handler.clone()).unwrap();
        proxy.ping().await.unwrap();
        assert_eq!(
            handler.last_ping_pid.load(Ordering::Relaxed),
            process::id() as i32
        );
    }

    /// A service keeps serving with no node held anywhere, which is the whole point of it
    /// — and `H` is inferred from the handler alone here, since it appears nowhere in the
    /// return type.
    #[tokio::test]
    async fn a_service_serves_without_a_node_to_hold() {
        let proxy = Test::new_service(TestHandlerImpl::default()).unwrap();
        proxy.ping().await.unwrap();
        assert!(gluon::Liveness::alive(&proxy));
    }

    /// The bootstrap path: a proxy built from a filesystem path rather than from a ref
    /// somebody handed us, talking to a `BoundNode` on the other end.
    #[tokio::test]
    async fn connect_reaches_a_bound_node() {
        let path = std::env::temp_dir().join(format!("gluon-connect-{}.sock", process::id()));
        let _ = std::fs::remove_file(&path);
        let _bound = gluon::BoundNode::bind(&path, TestHandlerImpl::default()).unwrap();

        let proxy = Test::connect(&path).await.unwrap();
        assert_eq!(
            proxy.get_position().await.unwrap(),
            MyVec3 {
                x: 1.0,
                y: 2.0,
                z: 3.0
            }
        );
    }

    // --- over a real socket ---

    #[tokio::test]
    async fn value_return_round_trips() {
        let (_node, proxy) = test_node();
        assert_eq!(
            proxy.get_position().await.unwrap(),
            MyVec3 {
                x: 1.0,
                y: 2.0,
                z: 3.0
            }
        );
    }

    #[tokio::test]
    async fn enum_param_and_return_round_trip() {
        let (_node, proxy) = test_node();
        let echoed = proxy
            .echo(MyTestEnum(protocol::test::proxied::TestEnum::EmptyVariant))
            .await
            .unwrap();
        assert!(matches!(
            echoed.0,
            protocol::test::proxied::TestEnum::EmptyVariant
        ));
    }

    /// A ref sent out and echoed back arrives as a descriptor for the same socket, and
    /// strong-ipc's registry is what makes that compare equal to the one we sent.
    #[tokio::test]
    async fn ref_identity_survives_a_round_trip() {
        let (_node, proxy) = test_node();
        let (_other_node, other_proxy) = test_node();

        let sent = gluon::ToRef::to_ref(&other_proxy);
        let returned = proxy.echo_untyped_ref(&other_proxy).await.unwrap();
        assert_eq!(returned, sent);
    }

    /// The typed variant of the same trip, through the proxy type rather than a bare ref.
    #[tokio::test]
    async fn typed_ref_round_trips() {
        let (_node, proxy) = test_node();
        let returned = proxy.echo_ref(proxy.clone()).await.unwrap();
        assert_eq!(returned, proxy);
    }

    /// `Context` is filled from `SO_PASSCRED`; over a socketpair to ourselves that has
    /// to come back as this process.
    #[tokio::test]
    async fn context_carries_peer_credentials() {
        let (node, proxy) = test_node();
        proxy.ping().await.unwrap();
        assert_eq!(
            node.handler().last_ping_pid.load(Ordering::Relaxed),
            process::id() as i32
        );
    }

    /// Dropping the node hangs its socket up, so the proxy's ref goes dead and sends to
    /// it report a gone peer rather than hanging.
    #[tokio::test]
    async fn dropping_the_node_kills_the_ref() {
        let (node, proxy) = test_node();
        assert!(gluon::Liveness::alive(&proxy));
        drop(node);
        gluon::Liveness::death_notification(&proxy).await;
        assert!(!gluon::Liveness::alive(&proxy));
    }

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
        use protocol::test::proxied::Color;
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
        use protocol::types::proxied::Vec3;
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
