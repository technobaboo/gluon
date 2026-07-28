pub mod primitive_impls;
pub use binderbinder::{
    TransactionHandler as Handler,
    binder_object::{
        BinderObject as Object, BinderObjectOrRef as ObjectOrRef, BinderObjectRef as ObjectRef,
        OwnedBinderObjectRefTrait as OwnedObjectRef, ToBinderObjectOrRef as ToObjectOrRef,
    },
    device::Transaction,
    payload::PayloadBuilder,
};
pub use gluon_derive::Handler;

use binderbinder::{
    TransactionHandler,
    binder_object::{BinderObjectOrRef, ToBinderObjectOrRef},
    payload::{PayloadBinderRefReadError, PayloadObjectReadError, PayloadReader},
};
use rustix::process::{RawPid, RawUid};
use std::{
    future::Future,
    os::fd::{BorrowedFd, OwnedFd},
    pin::Pin,
    string::FromUtf8Error,
    sync::Arc,
};
use thiserror::Error;
use tokio::sync::mpsc;

pub struct DataBuilder<'a> {
    payload: PayloadBuilder<'a>,
}

pub struct DataReader {
    payload: PayloadReader,
}
impl<'a> Default for DataBuilder<'a> {
    fn default() -> Self {
        Self::new()
    }
}
impl<'a> DataBuilder<'a> {
    pub fn new() -> Self {
        Self {
            payload: PayloadBuilder::new(),
        }
    }
    pub fn to_payload(self) -> PayloadBuilder<'a> {
        self.payload
    }
}
impl DataReader {
    pub fn from_payload(payload: PayloadReader) -> Self {
        Self { payload }
    }
}

pub trait Convertable: 'static + Sized {
    fn write<'a, 'b: 'a>(&'b self, data: &mut DataBuilder<'a>) -> Result<(), WriteError>;
    fn write_owned(self, data: &mut DataBuilder<'_>) -> Result<(), WriteError>;
    fn read(data: &mut DataReader) -> Result<Self, ReadError>;
}

/// Liveness of the remote object a binder object/ref points to.
pub trait Liveness {
    /// Whether the remote object is (as far as we know) still alive.
    fn alive(&self) -> bool;
    /// Future that resolves once the remote object has died. If we own the
    /// object locally, it can never die from our own perspective, so the
    /// future never completes.
    fn death_notification(&self) -> Pin<Box<dyn Future<Output = ()> + Send>>;
}
impl Liveness for ObjectOrRef {
    fn alive(&self) -> bool {
        BinderObjectOrRef::alive(self)
    }
    fn death_notification(&self) -> Pin<Box<dyn Future<Output = ()> + Send>> {
        BinderObjectOrRef::death_notification(self)
    }
}
impl<'a> DataBuilder<'a> {
    pub fn write_str(&mut self, str: &str) -> Result<(), WriteError> {
        if str.len() > u32::MAX as usize {
            return Err(WriteError::StringToLong);
        }
        self.write_u32(str.len() as u32)?;
        self.payload.push_bytes(str.as_bytes());
        Ok(())
    }
    pub fn write_f64(&mut self, float: f64) -> Result<(), WriteError> {
        self.payload.push_bytes(&float.to_le_bytes());
        Ok(())
    }
    pub fn write_f32(&mut self, float: f32) -> Result<(), WriteError> {
        self.payload.push_bytes(&float.to_le_bytes());
        Ok(())
    }
    pub fn write_bool(&mut self, bool: bool) -> Result<(), WriteError> {
        self.write_u8(bool as u8)?;
        Ok(())
    }
    pub fn write_fd<'fd: 'a>(&mut self, fd: BorrowedFd<'fd>) -> Result<(), WriteError> {
        self.payload.push_fd(fd, 0);
        Ok(())
    }
    pub fn write_owned_fd(&mut self, fd: OwnedFd) -> Result<(), WriteError> {
        self.payload.push_owned_fd(fd, 0);
        Ok(())
    }
    pub fn write_binder(
        &mut self,
        binder_ref: &impl ToBinderObjectOrRef,
    ) -> Result<(), WriteError> {
        self.payload.push_binder_ref(binder_ref);
        Ok(())
    }
}

// the ints
impl DataBuilder<'_> {
    pub fn write_u64(&mut self, int: u64) -> Result<(), WriteError> {
        self.payload.push_bytes(&int.to_le_bytes());
        Ok(())
    }
    pub fn write_i64(&mut self, int: i64) -> Result<(), WriteError> {
        self.payload.push_bytes(&int.to_le_bytes());
        Ok(())
    }
    pub fn write_u32(&mut self, int: u32) -> Result<(), WriteError> {
        self.payload.push_bytes(&int.to_le_bytes());
        Ok(())
    }
    pub fn write_i32(&mut self, int: i32) -> Result<(), WriteError> {
        self.payload.push_bytes(&int.to_le_bytes());
        Ok(())
    }
    pub fn write_u16(&mut self, int: u16) -> Result<(), WriteError> {
        self.payload.push_bytes(&int.to_le_bytes());
        Ok(())
    }
    pub fn write_i16(&mut self, int: i16) -> Result<(), WriteError> {
        self.payload.push_bytes(&int.to_le_bytes());
        Ok(())
    }
    pub fn write_u8(&mut self, int: u8) -> Result<(), WriteError> {
        self.payload.push_bytes(&int.to_le_bytes());
        Ok(())
    }
    pub fn write_i8(&mut self, int: i8) -> Result<(), WriteError> {
        self.payload.push_bytes(&int.to_le_bytes());
        Ok(())
    }
}
#[derive(Debug, Error)]
pub enum WriteError {
    #[error("String is longer than u32::MAX bytes")]
    StringToLong,
    #[error("List is longer than u32::MAX items")]
    ListToLong,
}

impl DataReader {
    pub fn read_string(&mut self) -> Result<String, ReadError> {
        let len = self.read_u32()?;
        let data = self
            .payload
            .read_bytes(len as usize)
            .map_err(|_| ReadError::NotEnoughBytes)?;
        Ok(String::from_utf8(data.to_vec())?)
    }
    pub fn read_f64(&mut self) -> Result<f64, ReadError> {
        let bytes = self
            .payload
            .read_bytes(size_of::<f64>())
            .map_err(|_| ReadError::NotEnoughBytes)?;
        Ok(f64::from_le_bytes(
            bytes.try_into().map_err(|_| ReadError::NotEnoughBytes)?,
        ))
    }
    pub fn read_f32(&mut self) -> Result<f32, ReadError> {
        let bytes = self
            .payload
            .read_bytes(size_of::<f32>())
            .map_err(|_| ReadError::NotEnoughBytes)?;
        Ok(f32::from_le_bytes(
            bytes.try_into().map_err(|_| ReadError::NotEnoughBytes)?,
        ))
    }
    pub fn read_bool(&mut self) -> Result<bool, ReadError> {
        Ok(self.read_u8()? != 0)
    }
    pub fn read_fd(&mut self) -> Result<OwnedFd, ReadError> {
        self.payload
            .read_fd()
            .map_err(|err| match err {
                PayloadObjectReadError::IncorrectObject => ReadError::IncorrectPrimitiveType,
                PayloadObjectReadError::Empty => ReadError::NotEnoughBytes,
            })
            .map(|v| v.0)
    }
    pub fn read_binder(&mut self) -> Result<BinderObjectOrRef, ReadError> {
        self.payload.read_binder_ref().map_err(|err| match err {
            PayloadBinderRefReadError::IncorrectObject => ReadError::IncorrectPrimitiveType,
            PayloadBinderRefReadError::UnknownBinderObject => ReadError::UnregisteredBinderObject,
            PayloadBinderRefReadError::DeadBinderObject => ReadError::DeadBinderObject,
            PayloadBinderRefReadError::Empty => ReadError::NotEnoughBytes,
        })
    }
}

// the ints
impl DataReader {
    pub fn read_u64(&mut self) -> Result<u64, ReadError> {
        let bytes = self
            .payload
            .read_bytes(size_of::<u64>())
            .map_err(|_| ReadError::NotEnoughBytes)?;
        Ok(u64::from_le_bytes(
            bytes.try_into().map_err(|_| ReadError::NotEnoughBytes)?,
        ))
    }
    pub fn read_i64(&mut self) -> Result<i64, ReadError> {
        let bytes = self
            .payload
            .read_bytes(size_of::<i64>())
            .map_err(|_| ReadError::NotEnoughBytes)?;
        Ok(i64::from_le_bytes(
            bytes.try_into().map_err(|_| ReadError::NotEnoughBytes)?,
        ))
    }
    pub fn read_u32(&mut self) -> Result<u32, ReadError> {
        let bytes = self
            .payload
            .read_bytes(size_of::<u32>())
            .map_err(|_| ReadError::NotEnoughBytes)?;
        Ok(u32::from_le_bytes(
            bytes.try_into().map_err(|_| ReadError::NotEnoughBytes)?,
        ))
    }
    pub fn read_i32(&mut self) -> Result<i32, ReadError> {
        let bytes = self
            .payload
            .read_bytes(size_of::<i32>())
            .map_err(|_| ReadError::NotEnoughBytes)?;
        Ok(i32::from_le_bytes(
            bytes.try_into().map_err(|_| ReadError::NotEnoughBytes)?,
        ))
    }
    pub fn read_u16(&mut self) -> Result<u16, ReadError> {
        let bytes = self
            .payload
            .read_bytes(size_of::<u16>())
            .map_err(|_| ReadError::NotEnoughBytes)?;
        Ok(u16::from_le_bytes(
            bytes.try_into().map_err(|_| ReadError::NotEnoughBytes)?,
        ))
    }
    pub fn read_i16(&mut self) -> Result<i16, ReadError> {
        let bytes = self
            .payload
            .read_bytes(size_of::<i16>())
            .map_err(|_| ReadError::NotEnoughBytes)?;
        Ok(i16::from_le_bytes(
            bytes.try_into().map_err(|_| ReadError::NotEnoughBytes)?,
        ))
    }
    pub fn read_u8(&mut self) -> Result<u8, ReadError> {
        let bytes = self
            .payload
            .read_bytes(size_of::<u8>())
            .map_err(|_| ReadError::NotEnoughBytes)?;
        Ok(u8::from_le_bytes(
            bytes.try_into().map_err(|_| ReadError::NotEnoughBytes)?,
        ))
    }
    pub fn read_i8(&mut self) -> Result<i8, ReadError> {
        let bytes = self
            .payload
            .read_bytes(size_of::<i8>())
            .map_err(|_| ReadError::NotEnoughBytes)?;
        Ok(i8::from_le_bytes(
            bytes.try_into().map_err(|_| ReadError::NotEnoughBytes)?,
        ))
    }
}

#[derive(Debug, Error)]
pub enum ReadError {
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
pub enum SendError {
    #[error("Failed to write Parameters: {0}")]
    ParamWriteError(#[from] WriteError),
    #[error("Failed to read return values: {0}")]
    ReturnReadError(#[from] ReadError),
    #[error("Transaction error: {0}")]
    TransactionError(#[from] binderbinder::error::Error),
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Context {
    pub sender_pid: RawPid,
    pub sender_euid: RawUid,
}

/// Handle to reply to a call whose return value is being sent back asynchronously,
/// separately from the `_oneway` dispatch future completing. Call `send(value)` with
/// the same value the corresponding non-`_oneway` method would have returned; `encode`
/// (supplied by codegen when the sender is constructed) knows how to convert and write
/// that value onto the wire, so callers never touch a `DataBuilder` directly.
pub struct ReplySender<T> {
    callback: ObjectOrRef,
    encode: fn(T, &mut DataBuilder<'_>) -> Result<(), WriteError>,
}

impl<T> std::fmt::Debug for ReplySender<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ReplySender")
            .field("callback", &self.callback)
            .finish_non_exhaustive()
    }
}

impl<T> ReplySender<T> {
    pub fn new(
        callback: ObjectOrRef,
        encode: fn(T, &mut DataBuilder<'_>) -> Result<(), WriteError>,
    ) -> Self {
        Self { callback, encode }
    }

    pub fn send(self, value: T) -> Result<(), SendError> {
        let mut payload = DataBuilder::new();
        (self.encode)(value, &mut payload)?;
        self.callback
            .device()
            .transact_one_way(&self.callback, 0, payload.to_payload())?;
        Ok(())
    }
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

#[cfg(test)]
mod tests {
    use std::marker::PhantomData;

    use binderbinder::TransactionHandler;

    // The derive emits `gluon::` paths; alias this crate so those paths
    // resolve when the tests are compiled as part of `gluon-wire` itself.
    extern crate self as gluon;

    use super::*;

    fn assert_handler<T: TransactionHandler>() {}

    // --- plain struct ---

    #[derive(Debug, Handler)]
    struct PlainHandler;

    impl PlainHandler {
        async fn dispatch_one_way(
            &self,
            _code: u32,
            _data: DataReader,
            _ctx: Context,
        ) -> Result<(), SendError> {
            Ok(())
        }
    }

    // --- generic struct (bounds on type param) ---

    #[derive(Debug, Handler)]
    struct GenericHandler<T: std::fmt::Debug + Send + Sync + 'static>(PhantomData<T>);

    impl<T: std::fmt::Debug + Send + Sync + 'static> GenericHandler<T> {
        async fn dispatch_one_way(
            &self,
            _code: u32,
            _data: DataReader,
            _ctx: Context,
        ) -> Result<(), SendError> {
            Ok(())
        }
    }

    // --- generic struct (bounds in where clause) ---

    #[derive(Debug, Handler)]
    struct WhereHandler<T>(PhantomData<T>)
    where
        T: std::fmt::Debug + Send + Sync + 'static;

    impl<T> WhereHandler<T>
    where
        T: std::fmt::Debug + Send + Sync + 'static,
    {
        async fn dispatch_one_way(
            &self,
            _code: u32,
            _data: DataReader,
            _ctx: Context,
        ) -> Result<(), SendError> {
            Ok(())
        }
    }

    #[test]
    fn plain_handler_is_transaction_handler() {
        assert_handler::<PlainHandler>();
    }

    #[test]
    fn generic_handler_is_transaction_handler() {
        assert_handler::<GenericHandler<u32>>();
    }

    #[test]
    fn where_clause_handler_is_transaction_handler() {
        assert_handler::<WhereHandler<String>>();
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
        const SERDE_SER   = 1 << 8;
        const SERDE_DE    = 1 << 9;

        /// All serde derives
        const SERDE = Self::SERDE_SER.bits() | Self::SERDE_DE.bits();
        /// All standard derives that integer types support
        const INTEGERS = Self::COPY.bits() | Self::CLONE.bits() | Self::HASH.bits()
            | Self::PARTIAL_EQ.bits() | Self::EQ.bits()
            | Self::PARTIAL_ORD.bits() | Self::ORD.bits()
            | Self::DEFAULT.bits() | Self::SERDE_SER.bits() | Self::SERDE_DE.bits();
        /// All standard derives that float types support (no Hash, Eq, or Ord)
        const FLOATS = Self::COPY.bits() | Self::CLONE.bits()
            | Self::PARTIAL_EQ.bits() | Self::PARTIAL_ORD.bits()
            | Self::DEFAULT.bits() | Self::SERDE_SER.bits() | Self::SERDE_DE.bits();
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ExternalProtocol {
    pub protocol_name: &'static str,
    pub types: &'static [ExternalGluonType],
}
#[derive(Clone, Copy, Debug)]
pub struct ExternalGluonType {
    pub name: &'static str,
    pub proxy: Option<&'static str>,
    pub supported_derives: Derives,
}
