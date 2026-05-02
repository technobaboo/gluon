pub mod primitive_impls;

use std::{
    os::fd::{BorrowedFd, OwnedFd},
    string::FromUtf8Error,
    sync::Arc,
};

use binderbinder::{
    TransactionHandler,
    binder_object::{BinderObjectOrRef, ToBinderObjectOrRef},
    device::Transaction,
    payload::{PayloadBinderRefReadError, PayloadBuilder, PayloadObjectReadError, PayloadReader},
};
use rustix::process::{RawPid, RawUid};
use thiserror::Error;
use tokio::sync::mpsc;

pub struct GluonDataBuilder<'a> {
    payload: PayloadBuilder<'a>,
}

pub struct GluonDataReader {
    payload: PayloadReader,
}
impl<'a> Default for GluonDataBuilder<'a> {
    fn default() -> Self {
        Self::new()
    }
}
impl<'a> GluonDataBuilder<'a> {
    pub fn new() -> Self {
        Self {
            payload: PayloadBuilder::new(),
        }
    }
    pub fn to_payload(self) -> PayloadBuilder<'a> {
        self.payload
    }
}
impl GluonDataReader {
    pub fn from_payload(payload: PayloadReader) -> Self {
        Self { payload }
    }
}

pub trait GluonConvertable: 'static + Sized {
    fn write<'a, 'b: 'a>(&'b self, data: &mut GluonDataBuilder<'a>) -> Result<(), GluonWriteError>;
    fn write_owned(self, data: &mut GluonDataBuilder<'_>) -> Result<(), GluonWriteError>;
    fn read(data: &mut GluonDataReader) -> Result<Self, GluonReadError>;
}
impl<'a> GluonDataBuilder<'a> {
    pub fn write_str(&mut self, str: &str) -> Result<(), GluonWriteError> {
        if str.len() > u32::MAX as usize {
            return Err(GluonWriteError::StringToLong);
        }
        self.write_u32(str.len() as u32)?;
        self.payload.push_bytes(str.as_bytes());
        Ok(())
    }
    pub fn write_f64(&mut self, float: f64) -> Result<(), GluonWriteError> {
        self.payload.push_bytes(&float.to_le_bytes());
        Ok(())
    }
    pub fn write_f32(&mut self, float: f32) -> Result<(), GluonWriteError> {
        self.payload.push_bytes(&float.to_le_bytes());
        Ok(())
    }
    pub fn write_bool(&mut self, bool: bool) -> Result<(), GluonWriteError> {
        self.write_u8(bool as u8)?;
        Ok(())
    }
    pub fn write_fd<'fd: 'a>(&mut self, fd: BorrowedFd<'fd>) -> Result<(), GluonWriteError> {
        self.payload.push_fd(fd, 0);
        Ok(())
    }
    pub fn write_owned_fd(&mut self, fd: OwnedFd) -> Result<(), GluonWriteError> {
        self.payload.push_owned_fd(fd, 0);
        Ok(())
    }
    pub fn write_binder(
        &mut self,
        binder_ref: &impl ToBinderObjectOrRef,
    ) -> Result<(), GluonWriteError> {
        self.payload.push_binder_ref(binder_ref);
        Ok(())
    }
}

// the ints
impl GluonDataBuilder<'_> {
    pub fn write_u64(&mut self, int: u64) -> Result<(), GluonWriteError> {
        self.payload.push_bytes(&int.to_le_bytes());
        Ok(())
    }
    pub fn write_i64(&mut self, int: i64) -> Result<(), GluonWriteError> {
        self.payload.push_bytes(&int.to_le_bytes());
        Ok(())
    }
    pub fn write_u32(&mut self, int: u32) -> Result<(), GluonWriteError> {
        self.payload.push_bytes(&int.to_le_bytes());
        Ok(())
    }
    pub fn write_i32(&mut self, int: i32) -> Result<(), GluonWriteError> {
        self.payload.push_bytes(&int.to_le_bytes());
        Ok(())
    }
    pub fn write_u16(&mut self, int: u16) -> Result<(), GluonWriteError> {
        self.payload.push_bytes(&int.to_le_bytes());
        Ok(())
    }
    pub fn write_i16(&mut self, int: i16) -> Result<(), GluonWriteError> {
        self.payload.push_bytes(&int.to_le_bytes());
        Ok(())
    }
    pub fn write_u8(&mut self, int: u8) -> Result<(), GluonWriteError> {
        self.payload.push_bytes(&int.to_le_bytes());
        Ok(())
    }
    pub fn write_i8(&mut self, int: i8) -> Result<(), GluonWriteError> {
        self.payload.push_bytes(&int.to_le_bytes());
        Ok(())
    }
}
#[derive(Debug, Error)]
pub enum GluonWriteError {
    #[error("String is longer than u32::MAX bytes")]
    StringToLong,
    #[error("List is longer than u32::MAX items")]
    ListToLong,
}

impl GluonDataReader {
    pub fn read_string(&mut self) -> Result<String, GluonReadError> {
        let len = self.read_u32()?;
        let data = self
            .payload
            .read_bytes(len as usize)
            .map_err(|_| GluonReadError::NotEnoughBytes)?;
        Ok(String::from_utf8(data.to_vec())?)
    }
    pub fn read_f64(&mut self) -> Result<f64, GluonReadError> {
        let bytes = self
            .payload
            .read_bytes(size_of::<f64>())
            .map_err(|_| GluonReadError::NotEnoughBytes)?;
        Ok(f64::from_le_bytes(
            bytes
                .try_into()
                .map_err(|_| GluonReadError::NotEnoughBytes)?,
        ))
    }
    pub fn read_f32(&mut self) -> Result<f32, GluonReadError> {
        let bytes = self
            .payload
            .read_bytes(size_of::<f32>())
            .map_err(|_| GluonReadError::NotEnoughBytes)?;
        Ok(f32::from_le_bytes(
            bytes
                .try_into()
                .map_err(|_| GluonReadError::NotEnoughBytes)?,
        ))
    }
    pub fn read_bool(&mut self) -> Result<bool, GluonReadError> {
        Ok(self.read_u8()? != 0)
    }
    pub fn read_fd(&mut self) -> Result<OwnedFd, GluonReadError> {
        self.payload
            .read_fd()
            .map_err(|err| match err {
                PayloadObjectReadError::IncorrectObject => GluonReadError::IncorrectPrimitiveType,
                PayloadObjectReadError::Empty => GluonReadError::NotEnoughBytes,
            })
            .map(|v| v.0)
    }
    pub fn read_binder(&mut self) -> Result<BinderObjectOrRef, GluonReadError> {
        self.payload.read_binder_ref().map_err(|err| match err {
            PayloadBinderRefReadError::IncorrectObject => GluonReadError::IncorrectPrimitiveType,
            PayloadBinderRefReadError::UnknownBinderObject => {
                GluonReadError::UnregisteredBinderObject
            }
            PayloadBinderRefReadError::DeadBinderObject => GluonReadError::DeadBinderObject,
            PayloadBinderRefReadError::Empty => GluonReadError::NotEnoughBytes,
        })
    }
}

// the ints
impl GluonDataReader {
    pub fn read_u64(&mut self) -> Result<u64, GluonReadError> {
        let bytes = self
            .payload
            .read_bytes(size_of::<u64>())
            .map_err(|_| GluonReadError::NotEnoughBytes)?;
        Ok(u64::from_le_bytes(
            bytes
                .try_into()
                .map_err(|_| GluonReadError::NotEnoughBytes)?,
        ))
    }
    pub fn read_i64(&mut self) -> Result<i64, GluonReadError> {
        let bytes = self
            .payload
            .read_bytes(size_of::<i64>())
            .map_err(|_| GluonReadError::NotEnoughBytes)?;
        Ok(i64::from_le_bytes(
            bytes
                .try_into()
                .map_err(|_| GluonReadError::NotEnoughBytes)?,
        ))
    }
    pub fn read_u32(&mut self) -> Result<u32, GluonReadError> {
        let bytes = self
            .payload
            .read_bytes(size_of::<u32>())
            .map_err(|_| GluonReadError::NotEnoughBytes)?;
        Ok(u32::from_le_bytes(
            bytes
                .try_into()
                .map_err(|_| GluonReadError::NotEnoughBytes)?,
        ))
    }
    pub fn read_i32(&mut self) -> Result<i32, GluonReadError> {
        let bytes = self
            .payload
            .read_bytes(size_of::<i32>())
            .map_err(|_| GluonReadError::NotEnoughBytes)?;
        Ok(i32::from_le_bytes(
            bytes
                .try_into()
                .map_err(|_| GluonReadError::NotEnoughBytes)?,
        ))
    }
    pub fn read_u16(&mut self) -> Result<u16, GluonReadError> {
        let bytes = self
            .payload
            .read_bytes(size_of::<u16>())
            .map_err(|_| GluonReadError::NotEnoughBytes)?;
        Ok(u16::from_le_bytes(
            bytes
                .try_into()
                .map_err(|_| GluonReadError::NotEnoughBytes)?,
        ))
    }
    pub fn read_i16(&mut self) -> Result<i16, GluonReadError> {
        let bytes = self
            .payload
            .read_bytes(size_of::<i16>())
            .map_err(|_| GluonReadError::NotEnoughBytes)?;
        Ok(i16::from_le_bytes(
            bytes
                .try_into()
                .map_err(|_| GluonReadError::NotEnoughBytes)?,
        ))
    }
    pub fn read_u8(&mut self) -> Result<u8, GluonReadError> {
        let bytes = self
            .payload
            .read_bytes(size_of::<u8>())
            .map_err(|_| GluonReadError::NotEnoughBytes)?;
        Ok(u8::from_le_bytes(
            bytes
                .try_into()
                .map_err(|_| GluonReadError::NotEnoughBytes)?,
        ))
    }
    pub fn read_i8(&mut self) -> Result<i8, GluonReadError> {
        let bytes = self
            .payload
            .read_bytes(size_of::<i8>())
            .map_err(|_| GluonReadError::NotEnoughBytes)?;
        Ok(i8::from_le_bytes(
            bytes
                .try_into()
                .map_err(|_| GluonReadError::NotEnoughBytes)?,
        ))
    }
}

#[derive(Debug, Error)]
pub enum GluonReadError {
    #[error("Not enough bytes for type")]
    NotEnoughBytes,
    #[error("Incorrect binder primitive type found")]
    IncorrectPrimitiveType,
    #[error("BinderObject not Registered")]
    UnregisteredBinderObject,
    #[error("BinderObject dead")]
    DeadBinderObject,
    #[error("String data is not valid utf8: {0}")]
    StringNotUtf8(#[from] FromUtf8Error),
    #[error("Unkown enum variant: {0}")]
    UnknownEnumVariant(u16),
}

#[derive(Debug, Error)]
pub enum GluonSendError {
    #[error("Failed to write Parameters: {0}")]
    ParamWriteError(#[from] GluonWriteError),
    #[error("Failed to read return values: {0}")]
    ReturnReadError(#[from] GluonReadError),
    #[error("Transaction error: {0}")]
    TransactionError(#[from] binderbinder::error::Error),
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct GluonCtx {
    pub sender_pid: RawPid,
    pub sender_euid: RawUid,
}
pub struct ReturnHandler(mpsc::Sender<Transaction>);

impl std::fmt::Debug for ReturnHandler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ReturnHandler").finish()
    }
}
impl TransactionHandler for ReturnHandler {
    async fn handle(self: Arc<Self>, _transaction: Transaction) -> PayloadBuilder<'static> {
        PayloadBuilder::new()
    }

    async fn handle_one_way(self: Arc<Self>, transaction: Transaction) {
        _ = self.0.send(transaction).await;
    }
}
impl ReturnHandler {
    pub fn new() -> (Self, mpsc::Receiver<Transaction>) {
        let (tx, rx) = mpsc::channel(1);
        (Self(tx), rx)
    }
}

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct Derives: u32 {
        const COPY        = 1 << 0;
        const CLONE       = 1 << 1;
        const HASH        = 1 << 2;
        const PARTIAL_EQ  = 1 << 3;
        const EQ          = 1 << 4;
        const PARTIAL_ORD = 1 << 5;
        const ORD         = 1 << 6;
        const DEFAULT     = 1 << 7;

        /// All standard derives that integer types support
        const INTEGERS = Self::COPY.bits() | Self::CLONE.bits() | Self::HASH.bits()
            | Self::PARTIAL_EQ.bits() | Self::EQ.bits()
            | Self::PARTIAL_ORD.bits() | Self::ORD.bits()
            | Self::DEFAULT.bits();
        /// All standard derives that float types support (no Hash, Eq, or Ord)
        const FLOATS = Self::COPY.bits() | Self::CLONE.bits()
            | Self::PARTIAL_EQ.bits() | Self::PARTIAL_ORD.bits()
            | Self::DEFAULT.bits();
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ExternalGluonProtocol {
    pub protocol_name: &'static str,
    pub types: &'static [ExternalGluonType],
}
#[derive(Clone, Copy, Debug)]
pub struct ExternalGluonType {
    pub name: &'static str,
    pub supported_derives: Derives,
}

#[cfg(feature = "tracing")]
#[macro_export]
macro_rules! impl_transaction_handler {
    ($type:ty) => {
        impl binderbinder::TransactionHandler for $type {
            async fn handle(
                self: std::sync::Arc<Self>,
                transaction: binderbinder::device::Transaction,
            ) -> binderbinder::payload::PayloadBuilder<'static> {
                tracing::warn!(concat!(
                    "Received two way transaction for ",
                    stringify!($type)
                ));
                binderbinder::payload::PayloadBuilder::new()
            }

            async fn handle_one_way(
                self: std::sync::Arc<Self>,
                transaction: binderbinder::device::Transaction,
            ) {
                let gluon_data = gluon_wire::GluonDataReader::from_payload(transaction.payload);
                _ = self
                    .dispatch_one_way(
                        transaction.code,
                        gluon_data,
                        gluon_wire::GluonCtx {
                            sender_pid: transaction.sender_pid,
                            sender_euid: transaction.sender_euid,
                        },
                    )
                    .await
                    .inspect_err(|err| {
                        tracing::error!(
                            concat!("failed to dispatch one_way {} for ", stringify!($type)),
                            err
                        )
                    });
            }
        }
    };
}
#[cfg(not(feature = "tracing"))]
#[macro_export]
macro_rules! impl_transaction_handler {
    ($type:ty) => {
        impl binderbinder::TransactionHandler for $type {
            async fn handle(
                self: std::sync::Arc<Self>,
                transaction: binderbinder::device::Transaction,
            ) -> binderbinder::payload::PayloadBuilder<'static> {
                binderbinder::payload::PayloadBuilder::new()
            }

            async fn handle_one_way(
                self: std::sync::Arc<Self>,
                transaction: binderbinder::device::Transaction,
            ) {
                let gluon_data = gluon_wire::GluonDataReader::from_payload(transaction.payload);
                _ = self
                    .dispatch_one_way(
                        transaction.code,
                        gluon_data,
                        gluon_wire::GluonCtx {
                            sender_pid: transaction.sender_pid,
                            sender_euid: transaction.sender_euid,
                        },
                    )
                    .await;
            }
        }
    };
}
