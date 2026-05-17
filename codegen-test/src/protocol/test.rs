#![allow(unused, clippy::all, private_bounds, private_interfaces)]
use gluon::Convertable;
pub const EXTERNAL_PROTOCOL: gluon::ExternalProtocol = gluon::ExternalProtocol {
    protocol_name: "org.gluon.Test",
    types: &[
        gluon::ExternalGluonType {
            name: "TestStruct",
            supported_derives: gluon::Derives::from_bits_truncate(2u32),
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
#[derive(Debug, Clone)]
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
        let mut gluon_builder = gluon::DataBuilder::new();
        self.obj.device().transact_one_way(&self.obj, 8u32, gluon_builder.to_payload())?;
        Ok(())
    }
    pub async fn ping(&self) -> Result<(), gluon::SendError> {
        let mut gluon_builder = gluon::DataBuilder::new();
        let (gluon_ret_handler, mut gluon_recv) = gluon::ReturnHandler::new();
        let gluon_ret = self.obj.device().register_object(gluon_ret_handler);
        gluon_builder.write_binder(&gluon_ret)?;
        self.obj.device().transact_one_way(&self.obj, 9u32, gluon_builder.to_payload())?;
        let transaction = gluon_recv.recv().await.unwrap();
        let mut reader = gluon::DataReader::from_payload(transaction.payload);
        Ok(())
    }
    pub async fn echo(
        &self,
        input: crate::MyTestEnum,
    ) -> Result<crate::MyTestEnum, gluon::SendError> {
        let input: proxied::TestEnum = input.into();
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
        Ok({
            let __w: proxied::TestEnum = gluon::Convertable::read(&mut reader)?;
            __w.into()
        })
    }
    pub async fn echo_ref(
        &self,
        input: impl Into<Test>,
    ) -> Result<Test, gluon::SendError> {
        let input: Test = input.into();
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
        Ok(gluon::Convertable::read(&mut reader)?)
    }
    pub async fn echo_untyped_ref(
        &self,
        input: &impl gluon::ToObjectOrRef,
    ) -> Result<gluon::ObjectOrRef, gluon::SendError> {
        let input: gluon::ObjectOrRef = gluon::ToObjectOrRef::to_binder_object_or_ref(
            input,
        );
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
        Ok(gluon::Convertable::read(&mut reader)?)
    }
    pub async fn get_position(&self) -> Result<crate::MyVec3, gluon::SendError> {
        let mut gluon_builder = gluon::DataBuilder::new();
        let (gluon_ret_handler, mut gluon_recv) = gluon::ReturnHandler::new();
        let gluon_ret = self.obj.device().register_object(gluon_ret_handler);
        gluon_builder.write_binder(&gluon_ret)?;
        self.obj
            .device()
            .transact_one_way(&self.obj, 13u32, gluon_builder.to_payload())?;
        let transaction = gluon_recv.recv().await.unwrap();
        let mut reader = gluon::DataReader::from_payload(transaction.payload);
        Ok({
            let __w: super::types::proxied::Vec3 = gluon::Convertable::read(
                &mut reader,
            )?;
            __w.into()
        })
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
    fn echo(
        &self,
        _ctx: gluon::Context,
        input: crate::MyTestEnum,
    ) -> impl Future<Output = crate::MyTestEnum> + Send + Sync;
    fn echo_ref(
        &self,
        _ctx: gluon::Context,
        input: Test,
    ) -> impl Future<Output = Test> + Send + Sync;
    fn echo_untyped_ref(
        &self,
        _ctx: gluon::Context,
        input: gluon::ObjectOrRef,
    ) -> impl Future<Output = gluon::ObjectOrRef> + Send + Sync;
    fn get_position(
        &self,
        _ctx: gluon::Context,
    ) -> impl Future<Output = crate::MyVec3> + Send + Sync;
    fn dispatch_one_way(
        &self,
        transaction_code: u32,
        mut gluon_data: gluon::DataReader,
        ctx: gluon::Context,
    ) -> impl Future<Output = Result<(), gluon::SendError>> + Send + Sync {
        async move {
            match transaction_code {
                8u32 => {
                    drop(gluon_data);
                    self.quit(ctx).await;
                }
                9u32 => {
                    let return_callback = gluon_data.read_binder()?;
                    let mut gluon_out = gluon::DataBuilder::new();
                    let () = self.ping(ctx).await;
                    drop(gluon_data);
                    return_callback
                        .device()
                        .transact_one_way(&return_callback, 0, gluon_out.to_payload())?;
                }
                10u32 => {
                    let return_callback = gluon_data.read_binder()?;
                    let mut gluon_out = gluon::DataBuilder::new();
                    let param_input: crate::MyTestEnum = {
                        let __w: proxied::TestEnum = gluon::Convertable::read(
                            &mut gluon_data,
                        )?;
                        __w.into()
                    };
                    let (output) = self.echo(ctx, param_input).await;
                    drop(gluon_data);
                    let __w: proxied::TestEnum = output.into();
                    __w.write_owned(&mut gluon_out)?;
                    return_callback
                        .device()
                        .transact_one_way(&return_callback, 0, gluon_out.to_payload())?;
                }
                11u32 => {
                    let return_callback = gluon_data.read_binder()?;
                    let mut gluon_out = gluon::DataBuilder::new();
                    let param_input = gluon::Convertable::read(&mut gluon_data)?;
                    let (output) = self.echo_ref(ctx, param_input).await;
                    drop(gluon_data);
                    output.write_owned(&mut gluon_out)?;
                    return_callback
                        .device()
                        .transact_one_way(&return_callback, 0, gluon_out.to_payload())?;
                }
                12u32 => {
                    let return_callback = gluon_data.read_binder()?;
                    let mut gluon_out = gluon::DataBuilder::new();
                    let param_input = gluon::Convertable::read(&mut gluon_data)?;
                    let (output) = self.echo_untyped_ref(ctx, param_input).await;
                    drop(gluon_data);
                    output.write_owned(&mut gluon_out)?;
                    return_callback
                        .device()
                        .transact_one_way(&return_callback, 0, gluon_out.to_payload())?;
                }
                13u32 => {
                    let return_callback = gluon_data.read_binder()?;
                    let mut gluon_out = gluon::DataBuilder::new();
                    let (position) = self.get_position(ctx).await;
                    drop(gluon_data);
                    let __w: super::types::proxied::Vec3 = position.into();
                    __w.write_owned(&mut gluon_out)?;
                    return_callback
                        .device()
                        .transact_one_way(&return_callback, 0, gluon_out.to_payload())?;
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
