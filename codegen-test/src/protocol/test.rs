#![allow(
    unused,
    clippy::single_match,
    clippy::match_single_binding,
    clippy::large_enum_variant
)]
use gluon_wire::GluonConvertable;
pub const EXTERNAL_PROTOCOL: gluon_wire::ExternalGluonProtocol = gluon_wire::ExternalGluonProtocol {
    protocol_name: "org.gluon.Test",
    types: &[
        gluon_wire::ExternalGluonType {
            name: "TestStruct",
            supported_derives: gluon_wire::Derives::from_bits_truncate(2u32),
        },
        gluon_wire::ExternalGluonType {
            name: "TestEnum",
            supported_derives: gluon_wire::Derives::from_bits_truncate(0u32),
        },
    ],
};
///test struct
#[derive(Debug, Clone)]
pub struct TestStruct {
    pub string: String,
    pub id: u64,
    pub binder_ref: Test,
    pub position: super::types::Vec3,
}
impl gluon_wire::GluonConvertable for TestStruct {
    fn write<'a, 'b: 'a>(
        &'b self,
        gluon_data: &mut gluon_wire::GluonDataBuilder<'a>,
    ) -> Result<(), gluon_wire::GluonWriteError> {
        self.string.write(gluon_data)?;
        self.id.write(gluon_data)?;
        self.binder_ref.write(gluon_data)?;
        self.position.write(gluon_data)?;
        Ok(())
    }
    fn read(
        gluon_data: &mut gluon_wire::GluonDataReader,
    ) -> Result<Self, gluon_wire::GluonReadError> {
        let string = gluon_wire::GluonConvertable::read(gluon_data)?;
        let id = gluon_wire::GluonConvertable::read(gluon_data)?;
        let binder_ref = gluon_wire::GluonConvertable::read(gluon_data)?;
        let position = gluon_wire::GluonConvertable::read(gluon_data)?;
        Ok(TestStruct {
            string,
            id,
            binder_ref,
            position,
        })
    }
    fn write_owned(
        self,
        gluon_data: &mut gluon_wire::GluonDataBuilder<'_>,
    ) -> Result<(), gluon_wire::GluonWriteError> {
        self.string.write_owned(gluon_data)?;
        self.id.write_owned(gluon_data)?;
        self.binder_ref.write_owned(gluon_data)?;
        self.position.write_owned(gluon_data)?;
        Ok(())
    }
}
///Test enum
#[derive(Debug)]
pub enum TestEnum {
    TestStruct { test_struct: TestStruct },
    Fd { fd: std::os::fd::OwnedFd },
    EmptyVariant,
}
impl gluon_wire::GluonConvertable for TestEnum {
    fn write<'a, 'b: 'a>(
        &'b self,
        gluon_data: &mut gluon_wire::GluonDataBuilder<'a>,
    ) -> Result<(), gluon_wire::GluonWriteError> {
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
    fn read(
        gluon_data: &mut gluon_wire::GluonDataReader,
    ) -> Result<Self, gluon_wire::GluonReadError> {
        Ok(
            match gluon_data.read_u16()? {
                0u16 => {
                    let test_struct = gluon_wire::GluonConvertable::read(gluon_data)?;
                    TestEnum::TestStruct {
                        test_struct,
                    }
                }
                1u16 => {
                    let fd = gluon_wire::GluonConvertable::read(gluon_data)?;
                    TestEnum::Fd { fd }
                }
                2u16 => TestEnum::EmptyVariant,
                v => return Err(gluon_wire::GluonReadError::UnknownEnumVariant(v)),
            },
        )
    }
    fn write_owned(
        self,
        gluon_data: &mut gluon_wire::GluonDataBuilder<'_>,
    ) -> Result<(), gluon_wire::GluonWriteError> {
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
#[derive(Debug, Clone)]
pub struct Test {
    obj: binderbinder::binder_object::BinderObjectOrRef,
}
impl gluon_wire::GluonConvertable for Test {
    fn write<'a, 'b: 'a>(
        &'b self,
        gluon_data: &mut gluon_wire::GluonDataBuilder<'a>,
    ) -> Result<(), gluon_wire::GluonWriteError> {
        self.obj.write(gluon_data)
    }
    fn read(
        gluon_data: &mut gluon_wire::GluonDataReader,
    ) -> Result<Self, gluon_wire::GluonReadError> {
        let obj = binderbinder::binder_object::BinderObjectOrRef::read(gluon_data)?;
        Ok(Test::from_object_or_ref(obj))
    }
    fn write_owned(
        self,
        gluon_data: &mut gluon_wire::GluonDataBuilder<'_>,
    ) -> Result<(), gluon_wire::GluonWriteError> {
        self.obj.write_owned(gluon_data)
    }
}
impl Test {
    pub fn quit(&self) -> Result<(), gluon_wire::GluonSendError> {
        let mut gluon_builder = gluon_wire::GluonDataBuilder::new();
        self.obj.device().transact_one_way(&self.obj, 8u32, gluon_builder.to_payload())?;
        Ok(())
    }
    pub async fn ping(&self) -> Result<(), gluon_wire::GluonSendError> {
        let mut gluon_builder = gluon_wire::GluonDataBuilder::new();
        let (gluon_ret_handler, mut gluon_recv) = gluon_wire::ReturnHandler::new();
        let gluon_ret = self.obj.device().register_object(gluon_ret_handler);
        gluon_builder.write_binder(&gluon_ret)?;
        self.obj.device().transact_one_way(&self.obj, 9u32, gluon_builder.to_payload())?;
        let transaction = gluon_recv.recv().await.unwrap();
        let mut reader = gluon_wire::GluonDataReader::from_payload(transaction.payload);
        Ok(())
    }
    pub async fn echo(
        &self,
        input: TestEnum,
    ) -> Result<TestEnum, gluon_wire::GluonSendError> {
        let mut gluon_builder = gluon_wire::GluonDataBuilder::new();
        let (gluon_ret_handler, mut gluon_recv) = gluon_wire::ReturnHandler::new();
        let gluon_ret = self.obj.device().register_object(gluon_ret_handler);
        gluon_builder.write_binder(&gluon_ret)?;
        input.write(&mut gluon_builder)?;
        self.obj
            .device()
            .transact_one_way(&self.obj, 10u32, gluon_builder.to_payload())?;
        let transaction = gluon_recv.recv().await.unwrap();
        let mut reader = gluon_wire::GluonDataReader::from_payload(transaction.payload);
        Ok(gluon_wire::GluonConvertable::read(&mut reader)?)
    }
    pub async fn echo_ref(
        &self,
        input: Test,
    ) -> Result<Test, gluon_wire::GluonSendError> {
        let mut gluon_builder = gluon_wire::GluonDataBuilder::new();
        let (gluon_ret_handler, mut gluon_recv) = gluon_wire::ReturnHandler::new();
        let gluon_ret = self.obj.device().register_object(gluon_ret_handler);
        gluon_builder.write_binder(&gluon_ret)?;
        input.write(&mut gluon_builder)?;
        self.obj
            .device()
            .transact_one_way(&self.obj, 11u32, gluon_builder.to_payload())?;
        let transaction = gluon_recv.recv().await.unwrap();
        let mut reader = gluon_wire::GluonDataReader::from_payload(transaction.payload);
        Ok(gluon_wire::GluonConvertable::read(&mut reader)?)
    }
    pub async fn get_position(
        &self,
    ) -> Result<super::types::Vec3, gluon_wire::GluonSendError> {
        let mut gluon_builder = gluon_wire::GluonDataBuilder::new();
        let (gluon_ret_handler, mut gluon_recv) = gluon_wire::ReturnHandler::new();
        let gluon_ret = self.obj.device().register_object(gluon_ret_handler);
        gluon_builder.write_binder(&gluon_ret)?;
        self.obj
            .device()
            .transact_one_way(&self.obj, 12u32, gluon_builder.to_payload())?;
        let transaction = gluon_recv.recv().await.unwrap();
        let mut reader = gluon_wire::GluonDataReader::from_payload(transaction.payload);
        Ok(gluon_wire::GluonConvertable::read(&mut reader)?)
    }
    pub fn from_handler<H: TestHandler>(
        obj: &binderbinder::binder_object::BinderObject<H>,
    ) -> Test {
        Test::from_object_or_ref(
            binderbinder::binder_object::ToBinderObjectOrRef::to_binder_object_or_ref(
                obj,
            ),
        )
    }
    ///only use this when you know the binder ref implements this interface, else the consquences are for you to find out
    pub fn from_object_or_ref(
        obj: binderbinder::binder_object::BinderObjectOrRef,
    ) -> Test {
        Test { obj }
    }
}
impl binderbinder::binder_object::ToBinderObjectOrRef for Test {
    fn to_binder_object_or_ref(&self) -> binderbinder::binder_object::BinderObjectOrRef {
        self.obj.to_binder_object_or_ref()
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
pub trait TestHandler: binderbinder::device::TransactionHandler + Send + Sync + 'static {
    fn quit(&self, _ctx: gluon_wire::GluonCtx) -> impl Future<Output = ()> + Send + Sync;
    fn ping(&self, _ctx: gluon_wire::GluonCtx) -> impl Future<Output = ()> + Send + Sync;
    fn echo(
        &self,
        _ctx: gluon_wire::GluonCtx,
        input: TestEnum,
    ) -> impl Future<Output = TestEnum> + Send + Sync;
    fn echo_ref(
        &self,
        _ctx: gluon_wire::GluonCtx,
        input: Test,
    ) -> impl Future<Output = Test> + Send + Sync;
    fn get_position(
        &self,
        _ctx: gluon_wire::GluonCtx,
    ) -> impl Future<Output = super::types::Vec3> + Send + Sync;
    fn dispatch_one_way(
        &self,
        transaction_code: u32,
        gluon_data: &mut gluon_wire::GluonDataReader,
        ctx: gluon_wire::GluonCtx,
    ) -> impl Future<Output = Result<(), gluon_wire::GluonSendError>> + Send + Sync {
        async move {
            match transaction_code {
                8u32 => {
                    self.quit(ctx).await;
                }
                9u32 => {
                    let return_callback = gluon_data.read_binder()?;
                    let mut gluon_out = gluon_wire::GluonDataBuilder::new();
                    let () = self.ping(ctx).await;
                    return_callback
                        .device()
                        .transact_one_way(&return_callback, 0, gluon_out.to_payload())?;
                }
                10u32 => {
                    let return_callback = gluon_data.read_binder()?;
                    let mut gluon_out = gluon_wire::GluonDataBuilder::new();
                    let (output) = self
                        .echo(ctx, gluon_wire::GluonConvertable::read(gluon_data)?)
                        .await;
                    output.write_owned(&mut gluon_out)?;
                    return_callback
                        .device()
                        .transact_one_way(&return_callback, 0, gluon_out.to_payload())?;
                }
                11u32 => {
                    let return_callback = gluon_data.read_binder()?;
                    let mut gluon_out = gluon_wire::GluonDataBuilder::new();
                    let (output) = self
                        .echo_ref(ctx, gluon_wire::GluonConvertable::read(gluon_data)?)
                        .await;
                    output.write_owned(&mut gluon_out)?;
                    return_callback
                        .device()
                        .transact_one_way(&return_callback, 0, gluon_out.to_payload())?;
                }
                12u32 => {
                    let return_callback = gluon_data.read_binder()?;
                    let mut gluon_out = gluon_wire::GluonDataBuilder::new();
                    let (position) = self.get_position(ctx).await;
                    position.write_owned(&mut gluon_out)?;
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
