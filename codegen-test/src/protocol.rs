use gluon_wire::GluonConvertable;
#[derive(Debug, Clone)]
pub struct Test(binderbinder::binder_object::BinderObjectOrRef);
impl gluon_wire::GluonConvertable for Test {
    fn write<'a, 'b: 'a>(
        &'b self,
        data: &mut gluon_wire::GluonDataBuilder<'a>,
    ) -> Result<(), gluon_wire::GluonWriteError> {
        self.0.write(data)
    }
    fn read(
        data: &mut gluon_wire::GluonDataReader,
    ) -> Result<Self, gluon_wire::GluonReadError> {
        Ok(Test(binderbinder::binder_object::BinderObjectOrRef::read(data)?))
    }
    fn write_owned(
        self,
        data: &mut gluon_wire::GluonDataBuilder<'_>,
    ) -> Result<(), gluon_wire::GluonWriteError> {
        self.0.write_owned(data)
    }
}
impl Test {
    pub fn quit(&self) {
        let builder = gluon_wire::GluonDataBuilder::new();
        self.0.device().transact_one_way(&self.0, 0u32, builder.to_payload()).unwrap();
    }
    pub async fn ping(&self) -> () {
        let obj = binderbinder::binder_object::ToBinderObjectOrRef::to_binder_object_or_ref(
            &self.0,
        );
        tokio::task::spawn_blocking(move || {
                let mut builder = gluon_wire::GluonDataBuilder::new();
                let reader = obj
                    .device()
                    .transact_blocking(&obj, 1u32, builder.to_payload())
                    .unwrap()
                    .1;
                let mut reader = gluon_wire::GluonDataReader::from_payload(reader);
            })
            .await
            .unwrap()
    }
    pub async fn echo(&self, input: String) -> String {
        let obj = binderbinder::binder_object::ToBinderObjectOrRef::to_binder_object_or_ref(
            &self.0,
        );
        tokio::task::spawn_blocking(move || {
                let mut builder = gluon_wire::GluonDataBuilder::new();
                input.write(&mut builder).unwrap();
                let reader = obj
                    .device()
                    .transact_blocking(&obj, 2u32, builder.to_payload())
                    .unwrap()
                    .1;
                let mut reader = gluon_wire::GluonDataReader::from_payload(reader);
                gluon_wire::GluonConvertable::read(&mut reader).unwrap()
            })
            .await
            .unwrap()
    }
    pub async fn echo_ref(&self, input: Test2) -> Test2 {
        let obj = binderbinder::binder_object::ToBinderObjectOrRef::to_binder_object_or_ref(
            &self.0,
        );
        tokio::task::spawn_blocking(move || {
                let mut builder = gluon_wire::GluonDataBuilder::new();
                input.write(&mut builder).unwrap();
                let reader = obj
                    .device()
                    .transact_blocking(&obj, 3u32, builder.to_payload())
                    .unwrap()
                    .1;
                let mut reader = gluon_wire::GluonDataReader::from_payload(reader);
                gluon_wire::GluonConvertable::read(&mut reader).unwrap()
            })
            .await
            .unwrap()
    }
    pub fn from_handler<H: TestHandler>(
        obj: &std::sync::Arc<binderbinder::binder_object::BinderObject<H>>,
    ) -> Test {
        Test(
            binderbinder::binder_object::ToBinderObjectOrRef::to_binder_object_or_ref(
                obj,
            ),
        )
    }
    ///only use this when you know the binder ref implements this interface, else the consquences are for you to find out
    pub fn from_object_or_ref(
        obj: binderbinder::binder_object::BinderObjectOrRef,
    ) -> Test {
        Test(obj)
    }
}
impl binderbinder::binder_object::ToBinderObjectOrRef for Test {
    fn to_binder_object_or_ref(&self) -> binderbinder::binder_object::BinderObjectOrRef {
        self.0.to_binder_object_or_ref()
    }
}
pub trait TestHandler: binderbinder::device::TransactionHandler + Send + Sync + 'static {
    fn quit(&self);
    fn ping(&self) -> impl Future<Output = ()> + Send + Sync;
    fn echo(&self, input: String) -> impl Future<Output = String> + Send + Sync;
    fn echo_ref(&self, input: Test2) -> impl Future<Output = Test2> + Send + Sync;
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
                8u32 => {
                    self.quit();
                }
                _ => {}
            }
        }
    }
}
#[derive(Debug, Clone)]
pub struct Test2(binderbinder::binder_object::BinderObjectOrRef);
impl gluon_wire::GluonConvertable for Test2 {
    fn write<'a, 'b: 'a>(
        &'b self,
        data: &mut gluon_wire::GluonDataBuilder<'a>,
    ) -> Result<(), gluon_wire::GluonWriteError> {
        self.0.write(data)
    }
    fn read(
        data: &mut gluon_wire::GluonDataReader,
    ) -> Result<Self, gluon_wire::GluonReadError> {
        Ok(Test2(binderbinder::binder_object::BinderObjectOrRef::read(data)?))
    }
    fn write_owned(
        self,
        data: &mut gluon_wire::GluonDataBuilder<'_>,
    ) -> Result<(), gluon_wire::GluonWriteError> {
        self.0.write_owned(data)
    }
}
impl Test2 {
    pub fn quit(&self) {
        let builder = gluon_wire::GluonDataBuilder::new();
        self.0.device().transact_one_way(&self.0, 0u32, builder.to_payload()).unwrap();
    }
    pub async fn ping(&self) -> () {
        let obj = binderbinder::binder_object::ToBinderObjectOrRef::to_binder_object_or_ref(
            &self.0,
        );
        tokio::task::spawn_blocking(move || {
                let mut builder = gluon_wire::GluonDataBuilder::new();
                let reader = obj
                    .device()
                    .transact_blocking(&obj, 1u32, builder.to_payload())
                    .unwrap()
                    .1;
                let mut reader = gluon_wire::GluonDataReader::from_payload(reader);
            })
            .await
            .unwrap()
    }
    pub async fn echo(&self, input: String) -> String {
        let obj = binderbinder::binder_object::ToBinderObjectOrRef::to_binder_object_or_ref(
            &self.0,
        );
        tokio::task::spawn_blocking(move || {
                let mut builder = gluon_wire::GluonDataBuilder::new();
                input.write(&mut builder).unwrap();
                let reader = obj
                    .device()
                    .transact_blocking(&obj, 2u32, builder.to_payload())
                    .unwrap()
                    .1;
                let mut reader = gluon_wire::GluonDataReader::from_payload(reader);
                gluon_wire::GluonConvertable::read(&mut reader).unwrap()
            })
            .await
            .unwrap()
    }
    pub async fn echo_ref(
        &self,
        input: binderbinder::binder_object::BinderObjectOrRef,
    ) -> binderbinder::binder_object::BinderObjectOrRef {
        let obj = binderbinder::binder_object::ToBinderObjectOrRef::to_binder_object_or_ref(
            &self.0,
        );
        tokio::task::spawn_blocking(move || {
                let mut builder = gluon_wire::GluonDataBuilder::new();
                input.write(&mut builder).unwrap();
                let reader = obj
                    .device()
                    .transact_blocking(&obj, 3u32, builder.to_payload())
                    .unwrap()
                    .1;
                let mut reader = gluon_wire::GluonDataReader::from_payload(reader);
                gluon_wire::GluonConvertable::read(&mut reader).unwrap()
            })
            .await
            .unwrap()
    }
    pub fn from_handler<H: Test2Handler>(
        obj: &std::sync::Arc<binderbinder::binder_object::BinderObject<H>>,
    ) -> Test2 {
        Test2(
            binderbinder::binder_object::ToBinderObjectOrRef::to_binder_object_or_ref(
                obj,
            ),
        )
    }
    ///only use this when you know the binder ref implements this interface, else the consquences are for you to find out
    pub fn from_object_or_ref(
        obj: binderbinder::binder_object::BinderObjectOrRef,
    ) -> Test2 {
        Test2(obj)
    }
}
impl binderbinder::binder_object::ToBinderObjectOrRef for Test2 {
    fn to_binder_object_or_ref(&self) -> binderbinder::binder_object::BinderObjectOrRef {
        self.0.to_binder_object_or_ref()
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
                8u32 => {
                    self.quit();
                }
                _ => {}
            }
        }
    }
}
///test struct
#[derive(Debug)]
pub struct TestStructClone {
    string: String,
    id: u64,
    binder_ref: Test,
}
impl gluon_wire::GluonConvertable for TestStructClone {
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
        Ok(TestStructClone {
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
