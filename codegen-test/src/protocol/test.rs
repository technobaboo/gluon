#![allow(unused, clippy::all, private_bounds, private_interfaces)]
use gluon::Convertable as _;
use tracing::Instrument as _;
pub const EXTERNAL_PROTOCOL: gluon::ExternalProtocol = gluon::ExternalProtocol {
    protocol_name: "org.gluon.Test",
    types: &[
        gluon::ExternalGluonType {
            name: "TestStruct",
            supported_derives: gluon::Derives::from_bits_truncate(10u32),
            proxy: None,
        },
        gluon::ExternalGluonType {
            name: "Palette",
            supported_derives: gluon::Derives::from_bits_truncate(31u32),
            proxy: None,
        },
        gluon::ExternalGluonType {
            name: "MaybeColor",
            supported_derives: gluon::Derives::from_bits_truncate(31u32),
            proxy: None,
        },
        gluon::ExternalGluonType {
            name: "TestEnum",
            supported_derives: gluon::Derives::from_bits_truncate(0u32),
            proxy: Some("proxies::MyTestEnum"),
        },
        gluon::ExternalGluonType {
            name: "Color",
            supported_derives: gluon::Derives::from_bits_truncate(127u32),
            proxy: Some("proxies::MyColor"),
        },
    ],
};
pub mod proxies {
    use super::*;
    pub use crate::MyTestEnum;
    pub use crate::MyColor;
}
///test struct
#[derive(Debug, Clone, PartialEq)]
pub struct TestStruct {
    pub string: String,
    pub id: u64,
    pub binder_ref: Test,
    pub position: crate::MyVec3,
}
impl gluon::Convertable for TestStruct {
    fn write<'a, 'b: 'a>(
        &'b self,
        gluon_data: &mut gluon::DataBuilder<'a>,
    ) -> Result<(), gluon::WriteError> {
        self.string.write(gluon_data)?;
        self.id.write(gluon_data)?;
        self.binder_ref.write(gluon_data)?;
        {
            let __w: super::types::proxied::Vec3 = self.position.clone().into();
            __w.write_owned(gluon_data)?;
        }
        Ok(())
    }
    fn read(gluon_data: &mut gluon::DataReader) -> Result<Self, gluon::ReadError> {
        let string = gluon::Convertable::read(gluon_data)?;
        let id = gluon::Convertable::read(gluon_data)?;
        let binder_ref = gluon::Convertable::read(gluon_data)?;
        let position: crate::MyVec3 = {
            let __w: super::types::proxied::Vec3 = gluon::Convertable::read(gluon_data)?;
            __w.into()
        };
        Ok(TestStruct {
            string,
            id,
            binder_ref,
            position,
        })
    }
    fn write_owned(
        self,
        gluon_data: &mut gluon::DataBuilder<'_>,
    ) -> Result<(), gluon::WriteError> {
        self.string.write_owned(gluon_data)?;
        self.id.write_owned(gluon_data)?;
        self.binder_ref.write_owned(gluon_data)?;
        {
            let __w: super::types::proxied::Vec3 = self.position.into();
            __w.write_owned(gluon_data)?;
        }
        Ok(())
    }
}
///Struct whose fields use Color — exercises the proxy-in-struct-field path
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Palette {
    pub primary: crate::MyColor,
    pub secondary: crate::MyColor,
}
impl gluon::Convertable for Palette {
    fn write<'a, 'b: 'a>(
        &'b self,
        gluon_data: &mut gluon::DataBuilder<'a>,
    ) -> Result<(), gluon::WriteError> {
        {
            let __w: proxied::Color = self.primary.clone().into();
            __w.write_owned(gluon_data)?;
        }
        {
            let __w: proxied::Color = self.secondary.clone().into();
            __w.write_owned(gluon_data)?;
        }
        Ok(())
    }
    fn read(gluon_data: &mut gluon::DataReader) -> Result<Self, gluon::ReadError> {
        let primary: crate::MyColor = {
            let __w: proxied::Color = gluon::Convertable::read(gluon_data)?;
            __w.into()
        };
        let secondary: crate::MyColor = {
            let __w: proxied::Color = gluon::Convertable::read(gluon_data)?;
            __w.into()
        };
        Ok(Palette { primary, secondary })
    }
    fn write_owned(
        self,
        gluon_data: &mut gluon::DataBuilder<'_>,
    ) -> Result<(), gluon::WriteError> {
        {
            let __w: proxied::Color = self.primary.into();
            __w.write_owned(gluon_data)?;
        }
        {
            let __w: proxied::Color = self.secondary.into();
            __w.write_owned(gluon_data)?;
        }
        Ok(())
    }
}
///Struct with an optional Color — exercises proxy inside Option
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct MaybeColor {
    pub color: Option<crate::MyColor>,
}
impl gluon::Convertable for MaybeColor {
    fn write<'a, 'b: 'a>(
        &'b self,
        gluon_data: &mut gluon::DataBuilder<'a>,
    ) -> Result<(), gluon::WriteError> {
        {
            let __w: Option<proxied::Color> = self.color.clone().map(|__v| __v.into());
            __w.write_owned(gluon_data)?;
        }
        Ok(())
    }
    fn read(gluon_data: &mut gluon::DataReader) -> Result<Self, gluon::ReadError> {
        let color: Option<crate::MyColor> = {
            let __w: Option<proxied::Color> = gluon::Convertable::read(gluon_data)?;
            __w.map(|__v| __v.into())
        };
        Ok(MaybeColor { color })
    }
    fn write_owned(
        self,
        gluon_data: &mut gluon::DataBuilder<'_>,
    ) -> Result<(), gluon::WriteError> {
        {
            let __w: Option<proxied::Color> = self.color.map(|__v| __v.into());
            __w.write_owned(gluon_data)?;
        }
        Ok(())
    }
}
#[derive(Debug, Clone)]
pub struct Test {
    obj: gluon::ObjectOrRef,
}
impl gluon::Convertable for Test {
    fn write<'a, 'b: 'a>(
        &'b self,
        gluon_data: &mut gluon::DataBuilder<'a>,
    ) -> Result<(), gluon::WriteError> {
        self.obj.write(gluon_data)
    }
    fn read(gluon_data: &mut gluon::DataReader) -> Result<Self, gluon::ReadError> {
        let obj = gluon::ObjectOrRef::read(gluon_data)?;
        Ok(Test::from_object_or_ref(obj))
    }
    fn write_owned(
        self,
        gluon_data: &mut gluon::DataBuilder<'_>,
    ) -> Result<(), gluon::WriteError> {
        self.obj.write_owned(gluon_data)
    }
}
impl Test {
    pub fn quit(&self) -> Result<(), gluon::SendError> {
        tracing::trace!(interface = "Test", method = "quit", "→");
        let mut gluon_builder = gluon::DataBuilder::new();
        self.obj.device().transact_one_way(&self.obj, 8u32, gluon_builder.to_payload())?;
        Ok(())
    }
    pub async fn ping(&self) -> Result<(), gluon::SendError> {
        tracing::trace!(interface = "Test", method = "ping", "→");
        let mut gluon_builder = gluon::DataBuilder::new();
        let (gluon_ret_handler, mut gluon_recv) = gluon::ReturnHandler::new();
        let gluon_ret = self.obj.device().register_object(gluon_ret_handler);
        gluon_builder.write_binder(&gluon_ret)?;
        self.obj.device().transact_one_way(&self.obj, 9u32, gluon_builder.to_payload())?;
        let transaction = gluon_recv.recv().await.unwrap();
        let mut reader = gluon::DataReader::from_payload(transaction.payload);
        tracing::trace!(interface = "Test", method = "ping", "←");
        Ok(())
    }
    pub async fn echo(
        &self,
        input: crate::MyTestEnum,
    ) -> Result<crate::MyTestEnum, gluon::SendError> {
        let input: proxied::TestEnum = input.into();
        tracing::trace!(interface = "Test", method = "echo", ? input, "→");
        let mut gluon_builder = gluon::DataBuilder::new();
        let (gluon_ret_handler, mut gluon_recv) = gluon::ReturnHandler::new();
        let gluon_ret = self.obj.device().register_object(gluon_ret_handler);
        gluon_builder.write_binder(&gluon_ret)?;
        input.write(&mut gluon_builder)?;
        self.obj
            .device()
            .transact_one_way(&self.obj, 10u32, gluon_builder.to_payload())?;
        let transaction = gluon_recv.recv().await.unwrap();
        let mut reader = gluon::DataReader::from_payload(transaction.payload);
        let __ret_output = {
            let __w: proxied::TestEnum = gluon::Convertable::read(&mut reader)?;
            __w.into()
        };
        tracing::trace!(
            interface = "Test", method = "echo", __ret_output = "crate::MyTestEnum",
            "←"
        );
        Ok(__ret_output)
    }
    pub async fn echo_ref(
        &self,
        input: impl Into<Test>,
    ) -> Result<Test, gluon::SendError> {
        let input: Test = input.into();
        tracing::trace!(interface = "Test", method = "echo_ref", ? input, "→");
        let mut gluon_builder = gluon::DataBuilder::new();
        let (gluon_ret_handler, mut gluon_recv) = gluon::ReturnHandler::new();
        let gluon_ret = self.obj.device().register_object(gluon_ret_handler);
        gluon_builder.write_binder(&gluon_ret)?;
        input.write(&mut gluon_builder)?;
        self.obj
            .device()
            .transact_one_way(&self.obj, 11u32, gluon_builder.to_payload())?;
        let transaction = gluon_recv.recv().await.unwrap();
        let mut reader = gluon::DataReader::from_payload(transaction.payload);
        let __ret_output = gluon::Convertable::read(&mut reader)?;
        tracing::trace!(interface = "Test", method = "echo_ref", ? __ret_output, "←");
        Ok(__ret_output)
    }
    pub async fn echo_untyped_ref(
        &self,
        input: &impl gluon::ToObjectOrRef,
    ) -> Result<gluon::ObjectOrRef, gluon::SendError> {
        let input: gluon::ObjectOrRef = gluon::ToObjectOrRef::to_binder_object_or_ref(
            input,
        );
        tracing::trace!(interface = "Test", method = "echo_untyped_ref", ? input, "→");
        let mut gluon_builder = gluon::DataBuilder::new();
        let (gluon_ret_handler, mut gluon_recv) = gluon::ReturnHandler::new();
        let gluon_ret = self.obj.device().register_object(gluon_ret_handler);
        gluon_builder.write_binder(&gluon_ret)?;
        input.write(&mut gluon_builder)?;
        self.obj
            .device()
            .transact_one_way(&self.obj, 12u32, gluon_builder.to_payload())?;
        let transaction = gluon_recv.recv().await.unwrap();
        let mut reader = gluon::DataReader::from_payload(transaction.payload);
        let __ret_output = gluon::Convertable::read(&mut reader)?;
        tracing::trace!(
            interface = "Test", method = "echo_untyped_ref", ? __ret_output, "←"
        );
        Ok(__ret_output)
    }
    pub async fn get_position(&self) -> Result<crate::MyVec3, gluon::SendError> {
        tracing::trace!(interface = "Test", method = "get_position", "→");
        let mut gluon_builder = gluon::DataBuilder::new();
        let (gluon_ret_handler, mut gluon_recv) = gluon::ReturnHandler::new();
        let gluon_ret = self.obj.device().register_object(gluon_ret_handler);
        gluon_builder.write_binder(&gluon_ret)?;
        self.obj
            .device()
            .transact_one_way(&self.obj, 13u32, gluon_builder.to_payload())?;
        let transaction = gluon_recv.recv().await.unwrap();
        let mut reader = gluon::DataReader::from_payload(transaction.payload);
        let __ret_position = {
            let __w: super::types::proxied::Vec3 = gluon::Convertable::read(
                &mut reader,
            )?;
            __w.into()
        };
        tracing::trace!(
            interface = "Test", method = "get_position", __ret_position =
            "crate::MyVec3", "←"
        );
        Ok(__ret_position)
    }
    pub fn from_handler<H: TestHandler>(obj: &impl gluon::OwnedObjectRef<H>) -> Test {
        Test::from_object_or_ref(gluon::OwnedObjectRef::to_object_or_ref(obj))
    }
    ///only use this when you know the binder ref implements this interface, else the consquences are for you to find out
    pub fn from_object_or_ref(obj: gluon::ObjectOrRef) -> Test {
        Test { obj }
    }
}
impl From<Test> for gluon::ObjectOrRef {
    fn from(value: Test) -> Self {
        value.obj
    }
}
impl gluon::ToObjectOrRef for Test {
    fn to_binder_object_or_ref(&self) -> gluon::ObjectOrRef {
        self.obj.clone()
    }
}
impl gluon::Liveness for Test {
    fn alive(&self) -> bool {
        gluon::Liveness::alive(&self.obj)
    }
    fn death_notification(
        &self,
    ) -> std::pin::Pin<Box<dyn std::future::Future<Output = ()> + Send>> {
        gluon::Liveness::death_notification(&self.obj)
    }
}
impl std::hash::Hash for Test {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.obj.hash(state);
    }
}
impl PartialEq for Test {
    fn eq(&self, other: &Self) -> bool {
        self.obj == other.obj
    }
}
impl Eq for Test {}
pub trait TestHandler: gluon::Handler + Send + Sync + 'static {
    fn quit(&self, _ctx: gluon::Context) -> impl Future<Output = ()> + Send + Sync;
    fn ping(&self, _ctx: gluon::Context) -> impl Future<Output = ()> + Send + Sync;
    ///Dispatched instead of [`Self::ping`] so a slow reply doesn't hold up dispatch of the next transaction. The default implementation just awaits `ping` and sends the result through `reply`. Override this method instead of `ping` to defer the reply: stash `reply` (it's `Send + Sync + 'static`) somewhere else — a channel, a queue, another task — and return as soon as this method's future is done, without waiting for the reply to actually be sent.
    fn ping_oneway(
        &self,
        _ctx: gluon::Context,
        reply: gluon::ReplySender<()>,
    ) -> impl Future<Output = Result<(), gluon::SendError>> + Send + Sync {
        async move {
            let () = self.ping(_ctx).await;
            reply.send(())
        }
    }
    fn echo(
        &self,
        _ctx: gluon::Context,
        input: crate::MyTestEnum,
    ) -> impl Future<Output = crate::MyTestEnum> + Send + Sync;
    ///Dispatched instead of [`Self::echo`] so a slow reply doesn't hold up dispatch of the next transaction. The default implementation just awaits `echo` and sends the result through `reply`. Override this method instead of `echo` to defer the reply: stash `reply` (it's `Send + Sync + 'static`) somewhere else — a channel, a queue, another task — and return as soon as this method's future is done, without waiting for the reply to actually be sent.
    fn echo_oneway(
        &self,
        _ctx: gluon::Context,
        input: crate::MyTestEnum,
        reply: gluon::ReplySender<crate::MyTestEnum>,
    ) -> impl Future<Output = Result<(), gluon::SendError>> + Send + Sync {
        async move {
            let output = self.echo(_ctx, input).await;
            reply.send(output)
        }
    }
    fn echo_ref(
        &self,
        _ctx: gluon::Context,
        input: Test,
    ) -> impl Future<Output = Test> + Send + Sync;
    ///Dispatched instead of [`Self::echo_ref`] so a slow reply doesn't hold up dispatch of the next transaction. The default implementation just awaits `echo_ref` and sends the result through `reply`. Override this method instead of `echo_ref` to defer the reply: stash `reply` (it's `Send + Sync + 'static`) somewhere else — a channel, a queue, another task — and return as soon as this method's future is done, without waiting for the reply to actually be sent.
    fn echo_ref_oneway(
        &self,
        _ctx: gluon::Context,
        input: Test,
        reply: gluon::ReplySender<Test>,
    ) -> impl Future<Output = Result<(), gluon::SendError>> + Send + Sync {
        async move {
            let output = self.echo_ref(_ctx, input).await;
            reply.send(output)
        }
    }
    fn echo_untyped_ref(
        &self,
        _ctx: gluon::Context,
        input: gluon::ObjectOrRef,
    ) -> impl Future<Output = gluon::ObjectOrRef> + Send + Sync;
    ///Dispatched instead of [`Self::echo_untyped_ref`] so a slow reply doesn't hold up dispatch of the next transaction. The default implementation just awaits `echo_untyped_ref` and sends the result through `reply`. Override this method instead of `echo_untyped_ref` to defer the reply: stash `reply` (it's `Send + Sync + 'static`) somewhere else — a channel, a queue, another task — and return as soon as this method's future is done, without waiting for the reply to actually be sent.
    fn echo_untyped_ref_oneway(
        &self,
        _ctx: gluon::Context,
        input: gluon::ObjectOrRef,
        reply: gluon::ReplySender<gluon::ObjectOrRef>,
    ) -> impl Future<Output = Result<(), gluon::SendError>> + Send + Sync {
        async move {
            let output = self.echo_untyped_ref(_ctx, input).await;
            reply.send(output)
        }
    }
    fn get_position(
        &self,
        _ctx: gluon::Context,
    ) -> impl Future<Output = crate::MyVec3> + Send + Sync;
    ///Dispatched instead of [`Self::get_position`] so a slow reply doesn't hold up dispatch of the next transaction. The default implementation just awaits `get_position` and sends the result through `reply`. Override this method instead of `get_position` to defer the reply: stash `reply` (it's `Send + Sync + 'static`) somewhere else — a channel, a queue, another task — and return as soon as this method's future is done, without waiting for the reply to actually be sent.
    fn get_position_oneway(
        &self,
        _ctx: gluon::Context,
        reply: gluon::ReplySender<crate::MyVec3>,
    ) -> impl Future<Output = Result<(), gluon::SendError>> + Send + Sync {
        async move {
            let position = self.get_position(_ctx).await;
            reply.send(position)
        }
    }
    fn dispatch_one_way(
        &self,
        transaction_code: u32,
        mut gluon_data: gluon::DataReader,
        ctx: gluon::Context,
    ) -> impl Future<Output = Result<(), gluon::SendError>> + Send + Sync {
        async move {
            match transaction_code {
                8u32 => {
                    tracing::trace!(interface = "Test", method = "quit", "dispatching");
                    drop(gluon_data);
                    self.quit(ctx)
                        .instrument(
                            tracing::trace_span!(
                                "dispatching", interface = "Test", method = "quit",
                                method_id = 8u32
                            ),
                        )
                        .await;
                }
                9u32 => {
                    let return_callback = gluon_data.read_binder()?;
                    tracing::trace!(interface = "Test", method = "ping", "dispatching");
                    drop(gluon_data);
                    let reply: gluon::ReplySender<()> = gluon::ReplySender::new(
                        return_callback,
                        |(), gluon_out| {
                            tracing::trace!(interface = "Test", method = "ping", "←");
                            Ok(())
                        },
                    );
                    self.ping_oneway(ctx, reply)
                        .instrument(
                            tracing::trace_span!(
                                "dispatching", interface = "Test", method = "ping",
                                method_id = 9u32
                            ),
                        )
                        .await?;
                }
                10u32 => {
                    let return_callback = gluon_data.read_binder()?;
                    let __wire_param_input: proxied::TestEnum = gluon::Convertable::read(
                        &mut gluon_data,
                    )?;
                    tracing::trace!(
                        interface = "Test", method = "echo", param_input = ?
                        __wire_param_input, "dispatching"
                    );
                    let param_input: crate::MyTestEnum = {
                        let __w = __wire_param_input;
                        __w.into()
                    };
                    drop(gluon_data);
                    let reply: gluon::ReplySender<crate::MyTestEnum> = gluon::ReplySender::new(
                        return_callback,
                        |output, gluon_out| {
                            tracing::trace!(
                                interface = "Test", method = "echo", output =
                                "crate::MyTestEnum", "←"
                            );
                            let __w: proxied::TestEnum = output.into();
                            __w.write_owned(gluon_out)?;
                            Ok(())
                        },
                    );
                    self.echo_oneway(ctx, param_input, reply)
                        .instrument(
                            tracing::trace_span!(
                                "dispatching", interface = "Test", method = "echo",
                                method_id = 10u32
                            ),
                        )
                        .await?;
                }
                11u32 => {
                    let return_callback = gluon_data.read_binder()?;
                    let param_input = gluon::Convertable::read(&mut gluon_data)?;
                    tracing::trace!(
                        interface = "Test", method = "echo_ref", ? param_input,
                        "dispatching"
                    );
                    drop(gluon_data);
                    let reply: gluon::ReplySender<Test> = gluon::ReplySender::new(
                        return_callback,
                        |output, gluon_out| {
                            tracing::trace!(
                                interface = "Test", method = "echo_ref", ? output, "←"
                            );
                            output.write_owned(gluon_out)?;
                            Ok(())
                        },
                    );
                    self.echo_ref_oneway(ctx, param_input, reply)
                        .instrument(
                            tracing::trace_span!(
                                "dispatching", interface = "Test", method = "echo_ref",
                                method_id = 11u32
                            ),
                        )
                        .await?;
                }
                12u32 => {
                    let return_callback = gluon_data.read_binder()?;
                    let param_input = gluon::Convertable::read(&mut gluon_data)?;
                    tracing::trace!(
                        interface = "Test", method = "echo_untyped_ref", ? param_input,
                        "dispatching"
                    );
                    drop(gluon_data);
                    let reply: gluon::ReplySender<gluon::ObjectOrRef> = gluon::ReplySender::new(
                        return_callback,
                        |output, gluon_out| {
                            tracing::trace!(
                                interface = "Test", method = "echo_untyped_ref", ? output,
                                "←"
                            );
                            output.write_owned(gluon_out)?;
                            Ok(())
                        },
                    );
                    self.echo_untyped_ref_oneway(ctx, param_input, reply)
                        .instrument(
                            tracing::trace_span!(
                                "dispatching", interface = "Test", method =
                                "echo_untyped_ref", method_id = 12u32
                            ),
                        )
                        .await?;
                }
                13u32 => {
                    let return_callback = gluon_data.read_binder()?;
                    tracing::trace!(
                        interface = "Test", method = "get_position", "dispatching"
                    );
                    drop(gluon_data);
                    let reply: gluon::ReplySender<crate::MyVec3> = gluon::ReplySender::new(
                        return_callback,
                        |position, gluon_out| {
                            tracing::trace!(
                                interface = "Test", method = "get_position", position =
                                "crate::MyVec3", "←"
                            );
                            let __w: super::types::proxied::Vec3 = position.into();
                            __w.write_owned(gluon_out)?;
                            Ok(())
                        },
                    );
                    self.get_position_oneway(ctx, reply)
                        .instrument(
                            tracing::trace_span!(
                                "dispatching", interface = "Test", method = "get_position",
                                method_id = 13u32
                            ),
                        )
                        .await?;
                }
                _ => {}
            }
            Ok(())
        }
    }
}
pub mod proxied {
    use super::*;
    ///Test enum
    #[derive(Debug)]
    pub enum TestEnum {
        TestStruct { test_struct: TestStruct },
        Fd { fd: std::os::fd::OwnedFd },
        EmptyVariant,
    }
    impl gluon::Convertable for TestEnum {
        fn write<'a, 'b: 'a>(
            &'b self,
            gluon_data: &mut gluon::DataBuilder<'a>,
        ) -> Result<(), gluon::WriteError> {
            match self {
                TestEnum::TestStruct { test_struct } => {
                    gluon_data.write_u16(0u16)?;
                    test_struct.write(gluon_data)?;
                }
                TestEnum::Fd { fd } => {
                    gluon_data.write_u16(1u16)?;
                    fd.write(gluon_data)?;
                }
                TestEnum::EmptyVariant => {
                    gluon_data.write_u16(2u16)?;
                }
            };
            Ok(())
        }
        fn read(gluon_data: &mut gluon::DataReader) -> Result<Self, gluon::ReadError> {
            Ok(
                match gluon_data.read_u16()? {
                    0u16 => {
                        let test_struct = gluon::Convertable::read(gluon_data)?;
                        TestEnum::TestStruct {
                            test_struct,
                        }
                    }
                    1u16 => {
                        let fd = gluon::Convertable::read(gluon_data)?;
                        TestEnum::Fd { fd }
                    }
                    2u16 => TestEnum::EmptyVariant,
                    v => return Err(gluon::ReadError::UnknownEnumVariant(v)),
                },
            )
        }
        fn write_owned(
            self,
            gluon_data: &mut gluon::DataBuilder<'_>,
        ) -> Result<(), gluon::WriteError> {
            match self {
                TestEnum::TestStruct { test_struct } => {
                    gluon_data.write_u16(0u16)?;
                    test_struct.write_owned(gluon_data)?;
                }
                TestEnum::Fd { fd } => {
                    gluon_data.write_u16(1u16)?;
                    fd.write_owned(gluon_data)?;
                }
                TestEnum::EmptyVariant => {
                    gluon_data.write_u16(2u16)?;
                }
            };
            Ok(())
        }
    }
    ///Simple unit-variant enum used to test proxy propagation into struct fields
    #[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
    pub enum Color {
        Red,
        Green,
        Blue,
    }
    impl gluon::Convertable for Color {
        fn write<'a, 'b: 'a>(
            &'b self,
            gluon_data: &mut gluon::DataBuilder<'a>,
        ) -> Result<(), gluon::WriteError> {
            match self {
                Color::Red => {
                    gluon_data.write_u16(0u16)?;
                }
                Color::Green => {
                    gluon_data.write_u16(1u16)?;
                }
                Color::Blue => {
                    gluon_data.write_u16(2u16)?;
                }
            };
            Ok(())
        }
        fn read(gluon_data: &mut gluon::DataReader) -> Result<Self, gluon::ReadError> {
            Ok(
                match gluon_data.read_u16()? {
                    0u16 => Color::Red,
                    1u16 => Color::Green,
                    2u16 => Color::Blue,
                    v => return Err(gluon::ReadError::UnknownEnumVariant(v)),
                },
            )
        }
        fn write_owned(
            self,
            gluon_data: &mut gluon::DataBuilder<'_>,
        ) -> Result<(), gluon::WriteError> {
            match self {
                Color::Red => {
                    gluon_data.write_u16(0u16)?;
                }
                Color::Green => {
                    gluon_data.write_u16(1u16)?;
                }
                Color::Blue => {
                    gluon_data.write_u16(2u16)?;
                }
            };
            Ok(())
        }
    }
}
