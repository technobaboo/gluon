#![allow(unused)]
use gluon_wire::GluonConvertable;
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
    pub fn quit(&self) {
        let mut builder = gluon_wire::GluonDataBuilder::new();
        self.obj
            .device()
            .transact_one_way(&self.obj, 8u32, builder.to_payload())
            .unwrap();
    }
    pub async fn ping(&self) -> () {
        let obj = binderbinder::binder_object::ToBinderObjectOrRef::to_binder_object_or_ref(
            &self.obj,
        );
        tokio::task::spawn_blocking(move || {
                let mut builder = gluon_wire::GluonDataBuilder::new();
                let reader = obj
                    .device()
                    .transact_blocking(&obj, 9u32, builder.to_payload())
                    .unwrap()
                    .1;
                let mut reader = gluon_wire::GluonDataReader::from_payload(reader);
            })
            .await
            .unwrap()
    }
    pub fn ping_blocking(&self) -> () {
        let mut builder = gluon_wire::GluonDataBuilder::new();
        let reader = self
            .obj
            .device()
            .transact_blocking(&self.obj, 9u32, builder.to_payload())
            .unwrap()
            .1;
        let mut reader = gluon_wire::GluonDataReader::from_payload(reader);
    }
    pub async fn echo(&self, input: String) -> String {
        let obj = binderbinder::binder_object::ToBinderObjectOrRef::to_binder_object_or_ref(
            &self.obj,
        );
        tokio::task::spawn_blocking(move || {
                let mut builder = gluon_wire::GluonDataBuilder::new();
                input.write(&mut builder).unwrap();
                let reader = obj
                    .device()
                    .transact_blocking(&obj, 10u32, builder.to_payload())
                    .unwrap()
                    .1;
                let mut reader = gluon_wire::GluonDataReader::from_payload(reader);
                gluon_wire::GluonConvertable::read(&mut reader).unwrap()
            })
            .await
            .unwrap()
    }
    pub fn echo_blocking(&self, input: String) -> String {
        let mut builder = gluon_wire::GluonDataBuilder::new();
        input.write(&mut builder).unwrap();
        let reader = self
            .obj
            .device()
            .transact_blocking(&self.obj, 10u32, builder.to_payload())
            .unwrap()
            .1;
        let mut reader = gluon_wire::GluonDataReader::from_payload(reader);
        gluon_wire::GluonConvertable::read(&mut reader).unwrap()
    }
    pub async fn echo_ref(&self, input: Test2) -> Test2 {
        let obj = binderbinder::binder_object::ToBinderObjectOrRef::to_binder_object_or_ref(
            &self.obj,
        );
        tokio::task::spawn_blocking(move || {
                let mut builder = gluon_wire::GluonDataBuilder::new();
                input.write(&mut builder).unwrap();
                let reader = obj
                    .device()
                    .transact_blocking(&obj, 11u32, builder.to_payload())
                    .unwrap()
                    .1;
                let mut reader = gluon_wire::GluonDataReader::from_payload(reader);
                gluon_wire::GluonConvertable::read(&mut reader).unwrap()
            })
            .await
            .unwrap()
    }
    pub fn echo_ref_blocking(&self, input: Test2) -> Test2 {
        let mut builder = gluon_wire::GluonDataBuilder::new();
        input.write(&mut builder).unwrap();
        let reader = self
            .obj
            .device()
            .transact_blocking(&self.obj, 11u32, builder.to_payload())
            .unwrap()
            .1;
        let mut reader = gluon_wire::GluonDataReader::from_payload(reader);
        gluon_wire::GluonConvertable::read(&mut reader).unwrap()
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
        obj.device().transact_one_way(&obj, 4, builder.to_payload()).unwrap();
        Test { obj, drop_notification }
    }
    pub async fn death_or_drop(&self) {
        let weak: Option<&binderbinder::binder_object::WeakBinderRef> = match &self.obj {
            binderbinder::binder_object::BinderObjectOrRef::Ref(r) => {
                Some(std::ops::Deref::deref(r))
            }
            binderbinder::binder_object::BinderObjectOrRef::WeakRef(r) => Some(r),
            _ => None,
        };
        if let Some(weak) = weak {
            tokio::select! {
                _ = weak.death_notification() => {} _ = self.drop_notification.wait() =>
                {}
            }
        } else {
            self.drop_notification.wait().await;
        }
    }
}
impl binderbinder::binder_object::ToBinderObjectOrRef for Test {
    fn to_binder_object_or_ref(&self) -> binderbinder::binder_object::BinderObjectOrRef {
        self.obj.to_binder_object_or_ref()
    }
}
pub trait TestHandler: binderbinder::device::TransactionHandler + Send + Sync + 'static {
    fn quit(&self);
    fn ping(&self) -> impl Future<Output = ()> + Send + Sync;
    fn echo(&self, input: String) -> impl Future<Output = String> + Send + Sync;
    fn echo_ref(&self, input: Test2) -> impl Future<Output = Test2> + Send + Sync;
    fn drop_notification_requested(
        &self,
        notifier: gluon_wire::drop_tracking::DropNotifier,
    ) -> impl Future<Output = ()> + Send + Sync;
    fn dispatch_two_way(
        &self,
        transaction_code: u32,
        data: &mut gluon_wire::GluonDataReader,
    ) -> impl Future<Output = gluon_wire::GluonDataBuilder<'static>> + Send + Sync {
        async move {
            let mut out = gluon_wire::GluonDataBuilder::new();
            match transaction_code {
                9u32 => {
                    let () = self.ping().await;
                }
                10u32 => {
                    let (output) = self
                        .echo(gluon_wire::GluonConvertable::read(data).unwrap())
                        .await;
                    output.write_owned(&mut out).unwrap();
                }
                11u32 => {
                    let (output) = self
                        .echo_ref(gluon_wire::GluonConvertable::read(data).unwrap())
                        .await;
                    output.write_owned(&mut out).unwrap();
                }
                _ => {}
            }
            out
        }
    }
    fn dispatch_one_way(
        &self,
        transaction_code: u32,
        data: &mut gluon_wire::GluonDataReader,
    ) -> impl Future<Output = ()> + Send + Sync {
        async move {
            match transaction_code {
                4 => {
                    let obj = data.read_binder().unwrap();
                    self.drop_notification_requested(
                            gluon_wire::drop_tracking::DropNotifier::new(&obj),
                        )
                        .await;
                }
                8u32 => {
                    self.quit();
                }
                _ => {}
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct Test2 {
    obj: binderbinder::binder_object::BinderObjectOrRef,
    drop_notification: std::sync::Arc<
        binderbinder::binder_object::BinderObject<
            gluon_wire::drop_tracking::DropNotifiedHandler,
        >,
    >,
}
impl gluon_wire::GluonConvertable for Test2 {
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
        Ok(Test2::from_object_or_ref(obj))
    }
    fn write_owned(
        self,
        data: &mut gluon_wire::GluonDataBuilder<'_>,
    ) -> Result<(), gluon_wire::GluonWriteError> {
        self.obj.write_owned(data)
    }
}
impl Test2 {
    pub fn quit(&self) {
        let mut builder = gluon_wire::GluonDataBuilder::new();
        self.obj
            .device()
            .transact_one_way(&self.obj, 8u32, builder.to_payload())
            .unwrap();
    }
    pub async fn ping(&self) -> () {
        let obj = binderbinder::binder_object::ToBinderObjectOrRef::to_binder_object_or_ref(
            &self.obj,
        );
        tokio::task::spawn_blocking(move || {
                let mut builder = gluon_wire::GluonDataBuilder::new();
                let reader = obj
                    .device()
                    .transact_blocking(&obj, 9u32, builder.to_payload())
                    .unwrap()
                    .1;
                let mut reader = gluon_wire::GluonDataReader::from_payload(reader);
            })
            .await
            .unwrap()
    }
    pub fn ping_blocking(&self) -> () {
        let mut builder = gluon_wire::GluonDataBuilder::new();
        let reader = self
            .obj
            .device()
            .transact_blocking(&self.obj, 9u32, builder.to_payload())
            .unwrap()
            .1;
        let mut reader = gluon_wire::GluonDataReader::from_payload(reader);
    }
    pub async fn echo(&self, input: String) -> String {
        let obj = binderbinder::binder_object::ToBinderObjectOrRef::to_binder_object_or_ref(
            &self.obj,
        );
        tokio::task::spawn_blocking(move || {
                let mut builder = gluon_wire::GluonDataBuilder::new();
                input.write(&mut builder).unwrap();
                let reader = obj
                    .device()
                    .transact_blocking(&obj, 10u32, builder.to_payload())
                    .unwrap()
                    .1;
                let mut reader = gluon_wire::GluonDataReader::from_payload(reader);
                gluon_wire::GluonConvertable::read(&mut reader).unwrap()
            })
            .await
            .unwrap()
    }
    pub fn echo_blocking(&self, input: String) -> String {
        let mut builder = gluon_wire::GluonDataBuilder::new();
        input.write(&mut builder).unwrap();
        let reader = self
            .obj
            .device()
            .transact_blocking(&self.obj, 10u32, builder.to_payload())
            .unwrap()
            .1;
        let mut reader = gluon_wire::GluonDataReader::from_payload(reader);
        gluon_wire::GluonConvertable::read(&mut reader).unwrap()
    }
    pub async fn echo_ref(
        &self,
        input: binderbinder::binder_object::BinderObjectOrRef,
    ) -> binderbinder::binder_object::BinderObjectOrRef {
        let obj = binderbinder::binder_object::ToBinderObjectOrRef::to_binder_object_or_ref(
            &self.obj,
        );
        tokio::task::spawn_blocking(move || {
                let mut builder = gluon_wire::GluonDataBuilder::new();
                input.write(&mut builder).unwrap();
                let reader = obj
                    .device()
                    .transact_blocking(&obj, 11u32, builder.to_payload())
                    .unwrap()
                    .1;
                let mut reader = gluon_wire::GluonDataReader::from_payload(reader);
                gluon_wire::GluonConvertable::read(&mut reader).unwrap()
            })
            .await
            .unwrap()
    }
    pub fn echo_ref_blocking(
        &self,
        input: binderbinder::binder_object::BinderObjectOrRef,
    ) -> binderbinder::binder_object::BinderObjectOrRef {
        let mut builder = gluon_wire::GluonDataBuilder::new();
        input.write(&mut builder).unwrap();
        let reader = self
            .obj
            .device()
            .transact_blocking(&self.obj, 11u32, builder.to_payload())
            .unwrap()
            .1;
        let mut reader = gluon_wire::GluonDataReader::from_payload(reader);
        gluon_wire::GluonConvertable::read(&mut reader).unwrap()
    }
    pub fn from_handler<H: Test2Handler>(
        obj: &std::sync::Arc<binderbinder::binder_object::BinderObject<H>>,
    ) -> Test2 {
        Test2::from_object_or_ref(
            binderbinder::binder_object::ToBinderObjectOrRef::to_binder_object_or_ref(
                obj,
            ),
        )
    }
    ///only use this when you know the binder ref implements this interface, else the consquences are for you to find out
    pub fn from_object_or_ref(
        obj: binderbinder::binder_object::BinderObjectOrRef,
    ) -> Test2 {
        let drop_notification = obj
            .device()
            .register_object(gluon_wire::drop_tracking::DropNotifiedHandler::new());
        let mut builder = gluon_wire::GluonDataBuilder::new();
        builder.write_binder(&drop_notification);
        obj.device().transact_one_way(&obj, 4, builder.to_payload()).unwrap();
        Test2 { obj, drop_notification }
    }
    pub async fn death_or_drop(&self) {
        let weak: Option<&binderbinder::binder_object::WeakBinderRef> = match &self.obj {
            binderbinder::binder_object::BinderObjectOrRef::Ref(r) => {
                Some(std::ops::Deref::deref(r))
            }
            binderbinder::binder_object::BinderObjectOrRef::WeakRef(r) => Some(r),
            _ => None,
        };
        if let Some(weak) = weak {
            tokio::select! {
                _ = weak.death_notification() => {} _ = self.drop_notification.wait() =>
                {}
            }
        } else {
            self.drop_notification.wait().await;
        }
    }
}
impl binderbinder::binder_object::ToBinderObjectOrRef for Test2 {
    fn to_binder_object_or_ref(&self) -> binderbinder::binder_object::BinderObjectOrRef {
        self.obj.to_binder_object_or_ref()
    }
}
pub trait Test2Handler: binderbinder::device::TransactionHandler + Send + Sync + 'static {
    fn quit(&self);
    fn ping(&self) -> impl Future<Output = ()> + Send + Sync;
    fn echo(&self, input: String) -> impl Future<Output = String> + Send + Sync;
    fn echo_ref(
        &self,
        input: binderbinder::binder_object::BinderObjectOrRef,
    ) -> impl Future<
        Output = binderbinder::binder_object::BinderObjectOrRef,
    > + Send + Sync;
    fn drop_notification_requested(
        &self,
        notifier: gluon_wire::drop_tracking::DropNotifier,
    ) -> impl Future<Output = ()> + Send + Sync;
    fn dispatch_two_way(
        &self,
        transaction_code: u32,
        data: &mut gluon_wire::GluonDataReader,
    ) -> impl Future<Output = gluon_wire::GluonDataBuilder<'static>> + Send + Sync {
        async move {
            let mut out = gluon_wire::GluonDataBuilder::new();
            match transaction_code {
                9u32 => {
                    let () = self.ping().await;
                }
                10u32 => {
                    let (output) = self
                        .echo(gluon_wire::GluonConvertable::read(data).unwrap())
                        .await;
                    output.write_owned(&mut out).unwrap();
                }
                11u32 => {
                    let (output) = self
                        .echo_ref(gluon_wire::GluonConvertable::read(data).unwrap())
                        .await;
                    output.write_owned(&mut out).unwrap();
                }
                _ => {}
            }
            out
        }
    }
    fn dispatch_one_way(
        &self,
        transaction_code: u32,
        data: &mut gluon_wire::GluonDataReader,
    ) -> impl Future<Output = ()> + Send + Sync {
        async move {
            match transaction_code {
                4 => {
                    let obj = data.read_binder().unwrap();
                    self.drop_notification_requested(
                            gluon_wire::drop_tracking::DropNotifier::new(&obj),
                        )
                        .await;
                }
                8u32 => {
                    self.quit();
                }
                _ => {}
            }
        }
    }
}
///test struct
#[derive(Clone, Debug)]
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
///Test enum
#[derive(Debug)]
pub enum TestEnum {
    StringVariant { string: String },
    Fd { fd: std::os::fd::OwnedFd },
    EmptyVairant,
}
impl gluon_wire::GluonConvertable for TestEnum {
    fn write<'a, 'b: 'a>(
        &'b self,
        data: &mut gluon_wire::GluonDataBuilder<'a>,
    ) -> Result<(), gluon_wire::GluonWriteError> {
        match self {
            TestEnum::StringVariant { string } => {
                data.write_u16(0u16)?;
                string.write(data)?;
            }
            TestEnum::Fd { fd } => {
                data.write_u16(1u16)?;
                fd.write(data)?;
            }
            TestEnum::EmptyVairant {} => {
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
                    let string = gluon_wire::GluonConvertable::read(data)?;
                    TestEnum::StringVariant { string }
                }
                1u16 => {
                    let fd = gluon_wire::GluonConvertable::read(data)?;
                    TestEnum::Fd { fd }
                }
                2u16 => TestEnum::EmptyVairant,
                v => return Err(gluon_wire::GluonReadError::UnknownEnumVariant(v)),
            },
        )
    }
    fn write_owned(
        self,
        data: &mut gluon_wire::GluonDataBuilder<'_>,
    ) -> Result<(), gluon_wire::GluonWriteError> {
        match self {
            TestEnum::StringVariant { string } => {
                data.write_u16(0u16)?;
                string.write_owned(data)?;
            }
            TestEnum::Fd { fd } => {
                data.write_u16(1u16)?;
                fd.write_owned(data)?;
            }
            TestEnum::EmptyVairant {} => {
                data.write_u16(2u16)?;
            }
        };
        Ok(())
    }
}
