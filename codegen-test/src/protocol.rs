#![allow(unused, clippy::single_match, clippy::match_single_binding)]
use gluon_wire::GluonConvertable;
pub const EXTERNAL_PROTOCOL: gluon_wire::ExternalGluonProtocol = gluon_wire::ExternalGluonProtocol {
    protocol_name: "test",
    types: &[
        gluon_wire::ExternalGluonType {
            name: "TestStruct",
            supported_traits: &["Clone"],
        },
        gluon_wire::ExternalGluonType {
            name: "Vec3",
            supported_traits: &["Copy", "Clone", "Hash", "PartialEq", "Eq"],
        },
        gluon_wire::ExternalGluonType {
            name: "TestEnum",
            supported_traits: &[],
        },
    ],
};
///test struct
#[derive(Debug, Clone)]
pub struct TestStruct {
    pub string: String,
    pub id: u64,
    pub binder_ref: Test,
}
impl gluon_wire::GluonConvertable for TestStruct {
    fn write<'a, 'b: 'a>(
        &'b self,
        data: &mut gluon_wire::GluonDataBuilder<'a>,
    ) -> Result<(), gluon_wire::GluonWriteError> {
        self.string.write(data)?;
        self.id.write(data)?;
        self.binder_ref.write(data)?;
        Ok(())
    }
    fn read(
        data: &mut gluon_wire::GluonDataReader,
    ) -> Result<Self, gluon_wire::GluonReadError> {
        let string = gluon_wire::GluonConvertable::read(data)?;
        let id = gluon_wire::GluonConvertable::read(data)?;
        let binder_ref = gluon_wire::GluonConvertable::read(data)?;
        Ok(TestStruct {
            string,
            id,
            binder_ref,
        })
    }
    fn write_owned(
        self,
        data: &mut gluon_wire::GluonDataBuilder<'_>,
    ) -> Result<(), gluon_wire::GluonWriteError> {
        self.string.write_owned(data)?;
        self.id.write_owned(data)?;
        self.binder_ref.write_owned(data)?;
        Ok(())
    }
}
///test struct
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Vec3 {
    pub x: u32,
    pub y: u32,
    pub z: u32,
}
impl gluon_wire::GluonConvertable for Vec3 {
    fn write<'a, 'b: 'a>(
        &'b self,
        data: &mut gluon_wire::GluonDataBuilder<'a>,
    ) -> Result<(), gluon_wire::GluonWriteError> {
        self.x.write(data)?;
        self.y.write(data)?;
        self.z.write(data)?;
        Ok(())
    }
    fn read(
        data: &mut gluon_wire::GluonDataReader,
    ) -> Result<Self, gluon_wire::GluonReadError> {
        let x = gluon_wire::GluonConvertable::read(data)?;
        let y = gluon_wire::GluonConvertable::read(data)?;
        let z = gluon_wire::GluonConvertable::read(data)?;
        Ok(Vec3 { x, y, z })
    }
    fn write_owned(
        self,
        data: &mut gluon_wire::GluonDataBuilder<'_>,
    ) -> Result<(), gluon_wire::GluonWriteError> {
        self.x.write_owned(data)?;
        self.y.write_owned(data)?;
        self.z.write_owned(data)?;
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
        data: &mut gluon_wire::GluonDataBuilder<'a>,
    ) -> Result<(), gluon_wire::GluonWriteError> {
        match self {
            TestEnum::TestStruct { test_struct } => {
                data.write_u16(0u16)?;
                test_struct.write(data)?;
            }
            TestEnum::Fd { fd } => {
                data.write_u16(1u16)?;
                fd.write(data)?;
            }
            TestEnum::EmptyVariant => {
                data.write_u16(2u16)?;
            }
        };
        Ok(())
    }
    fn read(
        data: &mut gluon_wire::GluonDataReader,
    ) -> Result<Self, gluon_wire::GluonReadError> {
        Ok(
            match data.read_u16()? {
                0u16 => {
                    let test_struct = gluon_wire::GluonConvertable::read(data)?;
                    TestEnum::TestStruct {
                        test_struct,
                    }
                }
                1u16 => {
                    let fd = gluon_wire::GluonConvertable::read(data)?;
                    TestEnum::Fd { fd }
                }
                2u16 => TestEnum::EmptyVariant,
                v => return Err(gluon_wire::GluonReadError::UnknownEnumVariant(v)),
            },
        )
    }
    fn write_owned(
        self,
        data: &mut gluon_wire::GluonDataBuilder<'_>,
    ) -> Result<(), gluon_wire::GluonWriteError> {
        match self {
            TestEnum::TestStruct { test_struct } => {
                data.write_u16(0u16)?;
                test_struct.write_owned(data)?;
            }
            TestEnum::Fd { fd } => {
                data.write_u16(1u16)?;
                fd.write_owned(data)?;
            }
            TestEnum::EmptyVariant => {
                data.write_u16(2u16)?;
            }
        };
        Ok(())
    }
}
#[derive(Debug, Clone)]
pub struct Test {
    obj: binderbinder::binder_object::BinderObjectOrRef,
    drop_notification: std::sync::Arc<
        binderbinder::binder_object::BinderObject<
            gluon_wire::drop_tracking::DropNotifiedHandler,
        >,
    >,
}
impl gluon_wire::GluonConvertable for Test {
    fn write<'a, 'b: 'a>(
        &'b self,
        data: &mut gluon_wire::GluonDataBuilder<'a>,
    ) -> Result<(), gluon_wire::GluonWriteError> {
        self.obj.write(data)
    }
    fn read(
        data: &mut gluon_wire::GluonDataReader,
    ) -> Result<Self, gluon_wire::GluonReadError> {
        let obj = binderbinder::binder_object::BinderObjectOrRef::read(data)?;
        Ok(Test::from_object_or_ref(obj))
    }
    fn write_owned(
        self,
        data: &mut gluon_wire::GluonDataBuilder<'_>,
    ) -> Result<(), gluon_wire::GluonWriteError> {
        self.obj.write_owned(data)
    }
}
impl Test {
    pub fn quit(&self) -> Result<(), gluon_wire::GluonSendError> {
        let mut builder = gluon_wire::GluonDataBuilder::new();
        self.obj.device().transact_one_way(&self.obj, 8u32, builder.to_payload())?;
        Ok(())
    }
    pub async fn ping(&self) -> Result<(), gluon_wire::GluonSendError> {
        let obj = binderbinder::binder_object::ToBinderObjectOrRef::to_binder_object_or_ref(
            &self.obj,
        );
        tokio::task::spawn_blocking(move || {
                let mut builder = gluon_wire::GluonDataBuilder::new();
                let reader = obj
                    .device()
                    .transact_blocking(&obj, 9u32, builder.to_payload())?
                    .1;
                let mut reader = gluon_wire::GluonDataReader::from_payload(reader);
                Ok(())
            })
            .await
            .unwrap()
    }
    pub fn ping_blocking(&self) -> Result<(), gluon_wire::GluonSendError> {
        let mut builder = gluon_wire::GluonDataBuilder::new();
        let reader = self
            .obj
            .device()
            .transact_blocking(&self.obj, 9u32, builder.to_payload())?
            .1;
        let mut reader = gluon_wire::GluonDataReader::from_payload(reader);
        Ok(())
    }
    pub async fn echo(
        &self,
        input: TestEnum,
    ) -> Result<TestEnum, gluon_wire::GluonSendError> {
        let obj = binderbinder::binder_object::ToBinderObjectOrRef::to_binder_object_or_ref(
            &self.obj,
        );
        tokio::task::spawn_blocking(move || {
                let mut builder = gluon_wire::GluonDataBuilder::new();
                input.write(&mut builder)?;
                let reader = obj
                    .device()
                    .transact_blocking(&obj, 10u32, builder.to_payload())?
                    .1;
                let mut reader = gluon_wire::GluonDataReader::from_payload(reader);
                Ok(gluon_wire::GluonConvertable::read(&mut reader)?)
            })
            .await
            .unwrap()
    }
    pub fn echo_blocking(
        &self,
        input: TestEnum,
    ) -> Result<TestEnum, gluon_wire::GluonSendError> {
        let mut builder = gluon_wire::GluonDataBuilder::new();
        input.write(&mut builder)?;
        let reader = self
            .obj
            .device()
            .transact_blocking(&self.obj, 10u32, builder.to_payload())?
            .1;
        let mut reader = gluon_wire::GluonDataReader::from_payload(reader);
        Ok(gluon_wire::GluonConvertable::read(&mut reader)?)
    }
    pub async fn echo_ref(
        &self,
        input: Test,
    ) -> Result<Test, gluon_wire::GluonSendError> {
        let obj = binderbinder::binder_object::ToBinderObjectOrRef::to_binder_object_or_ref(
            &self.obj,
        );
        tokio::task::spawn_blocking(move || {
                let mut builder = gluon_wire::GluonDataBuilder::new();
                input.write(&mut builder)?;
                let reader = obj
                    .device()
                    .transact_blocking(&obj, 11u32, builder.to_payload())?
                    .1;
                let mut reader = gluon_wire::GluonDataReader::from_payload(reader);
                Ok(gluon_wire::GluonConvertable::read(&mut reader)?)
            })
            .await
            .unwrap()
    }
    pub fn echo_ref_blocking(
        &self,
        input: Test,
    ) -> Result<Test, gluon_wire::GluonSendError> {
        let mut builder = gluon_wire::GluonDataBuilder::new();
        input.write(&mut builder)?;
        let reader = self
            .obj
            .device()
            .transact_blocking(&self.obj, 11u32, builder.to_payload())?
            .1;
        let mut reader = gluon_wire::GluonDataReader::from_payload(reader);
        Ok(gluon_wire::GluonConvertable::read(&mut reader)?)
    }
    pub fn from_handler<H: TestHandler>(
        obj: &std::sync::Arc<binderbinder::binder_object::BinderObject<H>>,
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
        let drop_notification = obj
            .device()
            .register_object(gluon_wire::drop_tracking::DropNotifiedHandler::new());
        let mut builder = gluon_wire::GluonDataBuilder::new();
        builder.write_binder(&drop_notification);
        _ = obj.device().transact_one_way(&obj, 4, builder.to_payload());
        Test { obj, drop_notification }
    }
    pub fn death_or_drop(&self) -> impl Future<Output = ()> + Send + Sync + 'static {
        let death_notification_future = match &self.obj {
            binderbinder::binder_object::BinderObjectOrRef::Ref(r) => {
                Some(r.death_notification())
            }
            binderbinder::binder_object::BinderObjectOrRef::WeakRef(r) => {
                Some(r.death_notification())
            }
            _ => None,
        };
        let drop_notification = self.drop_notification.clone();
        async move {
            if let Some(death) = death_notification_future {
                tokio::select! {
                    _ = death => {} _ = drop_notification.wait() => {}
                }
            } else {
                drop_notification.wait().await;
            }
        }
    }
}
impl binderbinder::binder_object::ToBinderObjectOrRef for Test {
    fn to_binder_object_or_ref(&self) -> binderbinder::binder_object::BinderObjectOrRef {
        self.obj.to_binder_object_or_ref()
    }
}
pub trait TestHandler: binderbinder::device::TransactionHandler + Send + Sync + 'static {
    fn quit(&self, _ctx: gluon_wire::GluonCtx);
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
    fn drop_notification_requested(
        &self,
        notifier: gluon_wire::drop_tracking::DropNotifier,
    ) -> impl Future<Output = ()> + Send + Sync;
    fn dispatch_two_way(
        &self,
        transaction_code: u32,
        data: &mut gluon_wire::GluonDataReader,
        ctx: gluon_wire::GluonCtx,
    ) -> impl Future<
        Output = Result<
            gluon_wire::GluonDataBuilder<'static>,
            gluon_wire::GluonSendError,
        >,
    > + Send + Sync {
        async move {
            let mut out = gluon_wire::GluonDataBuilder::new();
            match transaction_code {
                9u32 => {
                    let () = self.ping(ctx).await;
                }
                10u32 => {
                    let (output) = self
                        .echo(ctx, gluon_wire::GluonConvertable::read(data)?)
                        .await;
                    output.write_owned(&mut out)?;
                }
                11u32 => {
                    let (output) = self
                        .echo_ref(ctx, gluon_wire::GluonConvertable::read(data)?)
                        .await;
                    output.write_owned(&mut out)?;
                }
                _ => {}
            }
            Ok(out)
        }
    }
    fn dispatch_one_way(
        &self,
        transaction_code: u32,
        data: &mut gluon_wire::GluonDataReader,
        ctx: gluon_wire::GluonCtx,
    ) -> impl Future<Output = Result<(), gluon_wire::GluonSendError>> + Send + Sync {
        async move {
            match transaction_code {
                4 => {
                    let Ok(obj) = data.read_binder() else {
                        return Ok(());
                    };
                    self.drop_notification_requested(
                            gluon_wire::drop_tracking::DropNotifier::new(&obj),
                        )
                        .await;
                }
                8u32 => {
                    self.quit(ctx);
                }
                _ => {}
            }
            Ok(())
        }
    }
}
