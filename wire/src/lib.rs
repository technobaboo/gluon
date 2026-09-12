//! Wire types for gluon over [`strong_ipc`].
//!
//! # Message layout
//!
//! strong-ipc messages are a byte payload plus an ordered list of descriptors, with no
//! notion of a transaction code and no typed objects embedded in the bytes. gluon adds
//! both on top:
//!
//! ```text
//!   [u32 LE transaction code][payload bytes...]   + descriptors, in write order
//! ```
//!
//! Descriptors are matched to their slots positionally, exactly as binder did: the
//! schema decides how many are written and in what order, so the reader pops them in
//! the same order rather than tagging them in the byte stream. A [`Ref`] and a plain
//! `OwnedFd` are indistinguishable on the wire — both are just descriptors — so reading
//! a payload against the wrong schema yields the wrong Rust type rather than an error.
//!
//! Code `0` is the reply code used by [`ReplySender`]; generated method codes start at 8.
//!
//! # Refs and nodes
//!
//! binder had objects you host and refs to someone else's; strong-ipc's [`Ref`] and
//! [`Node`] are used here directly, unwrapped. A `Ref` is a capability to send to a node,
//! and it carries identity — strong-ipc dedupes descriptors for the same socket through a
//! registry — so two refs received in separate messages that lead to the same node
//! compare equal and hash alike, as binder's objects did.
//!
//! A `Node` deliberately holds no `Ref` to itself, so [`Node::new`] hands both back and
//! generated [`RefExt`] impls pass the ref straight into a proxy. Keep the node:
//! dropping it hangs its socket up and every ref to it goes dead.
//!
//! One thing binder had that a bare `Ref` cannot say is which side of an object you are
//! on. A generated proxy is a `Ref` and nothing else, so a node this process built and one
//! a peer handed us are the same type — right for the wire, lossy for the builder, who
//! knew the handler and then had nowhere to put it. [`LocalRef`] is that place: the proxy
//! plus the `Arc<H>` behind it, handed back by [`RefExt::new_node`] and
//! [`RefExt::new_service`], and recovered from a ref that comes back in by
//! [`RefExt::local_from_ref`]. It is an ergonomic pairing only — nothing about it reaches
//! the wire, where a `Ref` is still just a `Ref`.

pub mod primitive_impls;
pub use gluon_ipc_derive::Handler;
pub use strong_ipc::{
	DeathNotifier, FdVec, Handler, MAX_MESSAGE_SIZE, Message, Node, NodeError, Ref, RefFsBinding,
	UCred,
};

use rustix::process::{RawGid, RawPid, RawUid};
use std::{
	future::Future,
	ops::Deref,
	os::fd::{BorrowedFd, OwnedFd},
	pin::Pin,
	string::FromUtf8Error,
	sync::Arc,
};
use strong_ipc::TrySendError;
use thiserror::Error;
use tokio::sync::mpsc;

/// The reply-carrying transaction code. Generated method codes start at 8, so this can
/// never collide with one.
pub const REPLY_CODE: u32 = 0;

/// A descriptor waiting to go out on a message.
///
/// [`Message`] only accepts an `OwnedFd` or a `Ref`, so a borrowed fd is duplicated when
/// it is written. The kernel dups again out of `SCM_RIGHTS` at send time either way, so
/// this costs one extra `dup` on the borrowed path and nothing on the owned one.
enum Attachment {
	Fd(OwnedFd),
	Ref(Ref),
}

pub struct DataBuilder {
	/// starts with four zero bytes reserved for the transaction code, patched in by
	/// [`DataBuilder::finish`] so the code never costs a second buffer or a copy
	data: Vec<u8>,
	fds: Vec<Attachment>,
}

pub struct DataReader {
	/// owned rather than borrowed from the receive buffer: a [`DataReader`] outlives the
	/// handler call when it travels through [`ReturnHandler`]'s channel to a waiting
	/// proxy method
	data: Vec<u8>,
	cursor: usize,
	fds: std::vec::IntoIter<OwnedFd>,
}

impl Default for DataBuilder {
	fn default() -> Self {
		Self::new()
	}
}
impl DataBuilder {
	pub fn new() -> Self {
		Self {
			data: vec![0; size_of::<u32>()],
			fds: Vec::new(),
		}
	}
	/// Seals this payload into a message carrying `code`.
	pub fn finish(self, code: u32) -> Message {
		let Self { mut data, fds } = self;
		data[..size_of::<u32>()].copy_from_slice(&code.to_le_bytes());
		let mut message = Message::from_data(data);
		for fd in fds {
			match fd {
				Attachment::Fd(fd) => message.add_fd(fd),
				Attachment::Ref(node_ref) => message.add_ref(&node_ref),
			}
		}
		message
	}
}
impl DataReader {
	/// Splits a received message into its transaction code and a reader over the rest.
	pub fn from_wire(data: &[u8], fds: FdVec) -> Result<(u32, Self), ReadError> {
		let code = data
			.get(..size_of::<u32>())
			.and_then(|b| b.try_into().ok())
			.map(u32::from_le_bytes)
			.ok_or(ReadError::NotEnoughBytes)?;
		Ok((
			code,
			Self {
				data: data.to_vec(),
				cursor: size_of::<u32>(),
				fds: fds.into_vec().into_iter(),
			},
		))
	}

	fn read_bytes(&mut self, len: usize) -> Result<&[u8], ReadError> {
		let end = self
			.cursor
			.checked_add(len)
			.ok_or(ReadError::NotEnoughBytes)?;
		let bytes = self
			.data
			.get(self.cursor..end)
			.ok_or(ReadError::NotEnoughBytes)?;
		self.cursor = end;
		Ok(bytes)
	}
}

pub trait Convertable: 'static + Sized {
	fn write(&self, data: &mut DataBuilder) -> Result<(), WriteError>;
	fn write_owned(self, data: &mut DataBuilder) -> Result<(), WriteError>;
	fn read(data: &mut DataReader) -> Result<Self, ReadError>;
}

/// Anything that can produce the ref that reaches it.
pub trait ToRef: Send + Sync + 'static {
	fn to_ref(&self) -> Ref;
}
impl ToRef for Ref {
	fn to_ref(&self) -> Ref {
		self.clone()
	}
}

pub trait Interface: ToRef {
	const ID: &'static str;
}

/// A handler, or a share of one already in an `Arc`.
///
/// Deliberately not `Into<Arc<H>>`, which is ambiguous for the case that matters:
/// `Arc<H>: Into<Arc<?H>>` matches both `From<T> for T` and `From<T> for Arc<T>`, so
/// passing an `Arc` leaves `?H` unpinned and the call needs a turbofish. The two impls
/// here differ in the trait's own parameter rather than only in `Self`, and the reflexive
/// reading of the `Arc` case (`H = Arc<H>`) fails its `Handler` bound, so exactly one
/// candidate survives and `H` falls out of the argument on its own.
pub trait IntoHandler<H: Handler> {
	fn into_handler(self) -> Arc<H>;
}
impl<H: Handler> IntoHandler<H> for H {
	fn into_handler(self) -> Arc<H> {
		Arc::new(self)
	}
}
impl<H: Handler> IntoHandler<H> for Arc<H> {
	fn into_handler(self) -> Arc<H> {
		self
	}
}

/// An interface `H` can answer the methods of.
///
/// Generated per interface as `impl<H: TestHandler> HandledBy<H> for Test {}`, which is
/// what carries the per-interface bound now that [`RefExt`] is not generic over the
/// handler. That matters because [`RefExt::connect`] mentions no handler at all — with the
/// bound on the trait, `Test::connect(path)` would have had nothing to infer it from.
///
/// The interface is `Self` and the handler is the parameter, not the other way around,
/// because the orphan rule needs the local type first: an `impl<H: TestHandler>
/// HandlerFor<Test> for H` leaves `H` uncovered ahead of any local type and is refused
/// outright.
pub trait HandledBy<H: Handler>: Interface {}

/// A proxy paired with the handler behind it, for a node made in *this* process.
///
/// A plain proxy is a [`Ref`] and nothing else, which is the whole truth about one a peer
/// handed us but throws away what we knew about one we built ourselves. This keeps both:
/// the proxy to send through, and the `Arc<H>` the node is already feeding. No lookup, no
/// `Option` — the pairing is a fact of construction rather than something to go and check.
///
/// The [`HandledBy<H>`] bound is what makes that pairing honest. It is the same bound that
/// decides what may be *put* behind an interface, so a `LocalRef<Test, H>` can only exist
/// for an `H` answering `Test`'s methods — checked when it is built, not downcast when it
/// is read.
///
/// [`Deref`]s to the `Arc<H>`, so the handler's own methods and fields are reached bare and
/// the interface's go through [`LocalRef::proxy`]. Unlike [`Node`], which refuses the same
/// deref precisely because `node.clone()` would silently hand back a share of its handler,
/// that is safe here: `LocalRef` has its own `Clone`, and an inherent impl wins over a
/// deref, so `local.clone()` is another `LocalRef`.
///
/// **Do not store one of these in the handler it points at.** `Arc<H>` → `LocalRef` →
/// `Arc<H>` is a cycle and the handler never drops. Keep the bare proxy for a
/// self-reference — [`LocalRef::proxy`] hands it over.
pub struct LocalRef<I, H> {
	proxy: I,
	handler: Arc<H>,
}

impl<I: RefExt + HandledBy<H>, H: Handler> LocalRef<I, H> {
	/// Pairs a proxy with the handler behind it.
	///
	/// Nothing here checks that `proxy` actually leads to `handler` — the constructors on
	/// [`RefExt`] are the ones that know, and this is how they say so.
	pub fn new(proxy: I, handler: Arc<H>) -> Self {
		Self { proxy, handler }
	}

	/// The handler behind this proxy.
	///
	/// No lookup and no `Option`, unlike [`RefExt::local_handler`]: this is the `Arc` the
	/// node is running, carried here since it was built.
	pub fn handler(&self) -> &Arc<H> {
		&self.handler
	}

	/// The proxy, for calling this interface's methods and for anywhere a peer's ref goes.
	pub fn proxy(&self) -> &I {
		&self.proxy
	}

	/// Drops the handler share and keeps the proxy.
	pub fn into_proxy(self) -> I {
		self.proxy
	}
}

/// To the handler, as binderbinder's `BinderObjectRef` did — see the type's own docs for
/// why this is safe here and not on [`Node`].
impl<I: RefExt + HandledBy<H>, H: Handler> Deref for LocalRef<I, H> {
	type Target = Arc<H>;
	fn deref(&self) -> &Arc<H> {
		&self.handler
	}
}

/// Hand-written rather than derived, and rebuilding the proxy from its ref rather than
/// cloning it: that costs the same `Arc` bump and spares [`Interface`] a `Clone` supertrait
/// it would otherwise need for no other reason.
impl<I: RefExt + HandledBy<H>, H: Handler> Clone for LocalRef<I, H> {
	fn clone(&self) -> Self {
		Self {
			proxy: I::from_ref(self.proxy.to_ref()),
			handler: self.handler.clone(),
		}
	}
}

/// Prints the proxy, since the handler is not obliged to be `Debug` and the ref is the
/// identity anyway.
impl<I: RefExt + HandledBy<H> + std::fmt::Debug, H: Handler> std::fmt::Debug for LocalRef<I, H> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_struct("LocalRef")
			.field("proxy", &self.proxy)
			.finish_non_exhaustive()
	}
}

/// So a `LocalRef` goes straight into an untyped-ref parameter or [`DataBuilder::write_ref`].
impl<I: RefExt + HandledBy<H>, H: Handler> ToRef for LocalRef<I, H> {
	fn to_ref(&self) -> Ref {
		self.proxy.to_ref()
	}
}

/// The generated `From<LocalRef<Proxy, H>> for Proxy` is the one that feeds the
/// `impl Into<Proxy>` parameters; this is the untyped end of the same road.
impl<I: RefExt + HandledBy<H>, H: Handler> From<LocalRef<I, H>> for Ref {
	fn from(value: LocalRef<I, H>) -> Ref {
		value.proxy.to_ref()
	}
}

impl<I: RefExt + HandledBy<H>, H: Handler> Liveness for LocalRef<I, H> {
	fn alive(&self) -> bool {
		Liveness::alive(&self.proxy.to_ref())
	}
	fn death_notifier(&self) -> DeathNotifier {
		Liveness::death_notifier(&self.proxy.to_ref())
	}
}

/// Identity is the proxy's, which is the ref's — the handler share says nothing about
/// which node this is.
impl<I: RefExt + HandledBy<H> + PartialEq, H: Handler> PartialEq for LocalRef<I, H> {
	fn eq(&self, other: &Self) -> bool {
		self.proxy == other.proxy
	}
}
impl<I: RefExt + HandledBy<H> + Eq, H: Handler> Eq for LocalRef<I, H> {}
impl<I: RefExt + HandledBy<H> + std::hash::Hash, H: Handler> std::hash::Hash for LocalRef<I, H> {
	fn hash<S: std::hash::Hasher>(&self, state: &mut S) {
		self.proxy.hash(state);
	}
}

/// Everything you can do with an interface's proxy besides call its methods: reach a node
/// that already exists, put a handler behind a new one, or publish one at a path.
///
/// The handler constructors take [`IntoHandler`], so a handler you already share elsewhere
/// goes in as the `Arc` and one you don't goes in bare, and they bound `H` by
/// [`HandledBy<H>`] so only a handler that actually answers this interface's methods
/// is accepted. A handler for some other interface doesn't fail a check — the call simply
/// doesn't resolve for it.
pub trait RefExt: Interface + Sized {
	/// Wraps a ref you already have.
	///
	/// Only use this when you know the ref leads to something implementing this interface,
	/// else the consequences are for you to find out.
	fn from_ref(obj: Ref) -> Self;

	/// Connects to the [`RefFsBinding`] listening at `path`.
	///
	/// The other side of the bootstrap problem: a path is the one name that isn't itself a
	/// capability, so this is how you get a first ref without anyone handing you one.
	/// Nothing checks that whatever is listening speaks this interface.
	fn connect(
		path: impl AsRef<std::path::Path> + Send,
	) -> impl Future<Output = Result<Self, NodeError>> + Send {
		async move { Ok(Self::from_ref(Ref::connect(path).await?)) }
	}

	/// Publishes this proxy at `path`, so anyone who can open it gets a ref by
	/// [`RefExt::connect`]ing.
	///
	/// The other half of the bootstrap problem, and the only door into a process that
	/// isn't already a capability: every connection served here hands out a ref to the
	/// same node, and nothing about the path is checked beyond the filesystem's own
	/// permissions.
	///
	/// Keep the binding — dropping it stops the accept loop, and refs already handed out
	/// stay live. It does not unlink `path`, so a stale socket file left by a killed
	/// process reports [`std::io::ErrorKind::AddrInUse`] until something removes it.
	fn bind(&self, path: impl AsRef<std::path::Path>) -> Result<RefFsBinding, NodeError> {
		Ok(RefFsBinding::new(self.to_ref(), path)?)
	}

	/// Runs `handler` on a new node reached through the returned [`LocalRef`].
	///
	/// A [`LocalRef`] rather than a bare proxy because this is the one call that *knows*
	/// what is behind the ref it hands back — throwing that away here is what forced every
	/// caller to either carry the `Arc<H>` in a second variable or go and look it up again
	/// with [`Self::local_handler`].
	///
	/// Keep the node. It *is* the node, and dropping it hangs its socket up, so the
	/// `LocalRef` returned beside it goes dead — it holds a share of the handler, but that
	/// keeps the handler alive, not the node. Use [`Self::new_service`] when you would
	/// rather the refs decided that.
	fn new_node<H: Handler>(
		handler: impl IntoHandler<H>,
	) -> Result<(Node<H>, LocalRef<Self, H>), NodeError>
	where
		Self: HandledBy<H>,
	{
		let (node, node_ref) = Node::new_raw(handler.into_handler())?;
		let local = LocalRef::new(Self::from_ref(node_ref), node.handler().clone());
		Ok((node, local))
	}

	/// [`Self::new_node`] for a handler nothing is going to hold onto.
	///
	/// Hands the node's lifetime straight to its refs through [`Node::to_service`], so
	/// there is no node to keep and the [`LocalRef`] is the whole result. It lives until
	/// the last ref to it goes, and there is no getting it back to stop it earlier.
	fn new_service<H: Handler>(handler: impl IntoHandler<H>) -> Result<LocalRef<Self, H>, NodeError>
	where
		Self: HandledBy<H>,
	{
		let (node, local) = Self::new_node(handler)?;
		node.to_service();
		Ok(local)
	}

	/// The handler behind this proxy, if it leads to a node in *this* process.
	///
	/// A proxy a peer handed you is normally opaque — you call its methods and the wire
	/// decides what happens. But a process that is both ends of an interface handed out
	/// the node in the first place, and a ref it gets back is recognised on arrival, so
	/// the handler is a hash lookup away rather than a round trip through its own wire
	/// format. That is the whole shortcut, and why it is behind a feature.
	///
	/// The [`HandledBy<H>`] bound is doing real work here: it is what stops this being a
	/// blind downcast. Asking a `Spatial` for a handler that only answers `Field`'s
	/// methods doesn't return `None`, it doesn't compile — the same bound that decides
	/// what may be *put* behind this interface decides what may be recovered from it.
	///
	/// `None` means every way this can fail to be a handler you can have: the proxy leads
	/// to another process, its node is gone, or it is some other `H` entirely.
	#[cfg(feature = "local-handlers")]
	fn local_handler<H: Handler>(&self) -> Option<Arc<H>>
	where
		Self: HandledBy<H>,
	{
		self.to_ref().local_handler::<H>()
	}

	/// [`Self::local_handler`], but the ref you hand in becomes the proxy instead of being
	/// dropped on the floor.
	///
	/// The lookup needs a ref and so does the proxy, so this is the same one call with
	/// nothing thrown away: `obj` moves into the [`LocalRef`] rather than being cloned back
	/// out of it afterwards. What comes back can be *called* as well as read from, which is
	/// the difference from a bare `Arc<H>`.
	///
	/// `None` means what it always did — the ref leads to another process, its node is
	/// gone, or it is some other `H` entirely.
	#[cfg(feature = "local-handlers")]
	fn local_from_ref<H: Handler>(obj: Ref) -> Option<LocalRef<Self, H>>
	where
		Self: HandledBy<H>,
	{
		let handler = obj.local_handler::<H>()?;
		Some(LocalRef::new(Self::from_ref(obj), handler))
	}

	/// [`Self::local_from_ref`] for a proxy you are already holding — the typed answer to
	/// "is this one of mine?" for a ref that came back in off the wire.
	#[cfg(feature = "local-handlers")]
	fn as_local<H: Handler>(&self) -> Option<LocalRef<Self, H>>
	where
		Self: HandledBy<H>,
	{
		Self::local_from_ref(self.to_ref())
	}
}

/// Liveness of the node a ref points to.
pub trait Liveness {
	/// Whether the node is (as far as we know) still alive.
	fn alive(&self) -> bool {
		!self.death_notifier().is_dead()
	}
	/// This death as an owned handle, to hold and wait on somewhere else.
	///
	/// The one method worth implementing: [`Self::death_notification`] falls out of it,
	/// and unlike that one-shot future this can be cloned, waited on more than once, and
	/// asked with [`DeathNotifier::is_dead`] without awaiting at all.
	///
	/// What holding one says about the lifetime of what it watches depends on which
	/// variant comes back — see [`DeathNotifier`].
	fn death_notifier(&self) -> DeathNotifier;
	/// Future that resolves once the node has died.
	fn death_notification(&self) -> Pin<Box<dyn Future<Output = ()> + Send>> {
		let notifier = self.death_notifier();
		Box::pin(async move { notifier.wait().await })
	}
}
impl Liveness for Ref {
	fn alive(&self) -> bool {
		!self.is_dead()
	}
	fn death_notifier(&self) -> DeathNotifier {
		Ref::death_notifier(self)
	}
}

/// A node in this process, which is dead once nothing can reach it any more.
impl<H: Handler> Liveness for Node<H> {
	fn alive(&self) -> bool {
		!self.is_dead()
	}
	fn death_notifier(&self) -> DeathNotifier {
		// Spelled as a path: the inherent method and this one share a name, and the
		// inherent one is what should answer here.
		Node::death_notifier(self)
	}
}

/// A death already held apart from whatever it belongs to.
///
/// The identity impl, so anything handing out a notifier can be waited on the same way as
/// the notifier itself.
impl Liveness for DeathNotifier {
	fn alive(&self) -> bool {
		!self.is_dead()
	}
	fn death_notifier(&self) -> DeathNotifier {
		self.clone()
	}
}

/// Sends `data` to `target` under `code`.
///
/// Never blocks: this is [`Ref::try_send`], so a peer that is merely behind reports
/// [`SendError::Full`] rather than parking the caller.
pub fn transact(target: &Ref, code: u32, data: DataBuilder) -> Result<(), SendError> {
	target.try_send(data.finish(code)).map_err(SendError::from)
}

impl DataBuilder {
	pub fn write_str(&mut self, str: &str) -> Result<(), WriteError> {
		if str.len() > u32::MAX as usize {
			return Err(WriteError::StringToLong);
		}
		self.write_u32(str.len() as u32)?;
		self.data.extend_from_slice(str.as_bytes());
		Ok(())
	}
	pub fn write_f64(&mut self, float: f64) -> Result<(), WriteError> {
		self.data.extend_from_slice(&float.to_le_bytes());
		Ok(())
	}
	pub fn write_f32(&mut self, float: f32) -> Result<(), WriteError> {
		self.data.extend_from_slice(&float.to_le_bytes());
		Ok(())
	}
	pub fn write_bool(&mut self, bool: bool) -> Result<(), WriteError> {
		self.write_u8(bool as u8)?;
		Ok(())
	}
	/// Duplicates `fd` — see [`Attachment`]. Prefer [`DataBuilder::write_owned_fd`].
	pub fn write_fd(&mut self, fd: BorrowedFd<'_>) -> Result<(), WriteError> {
		let fd = fd.try_clone_to_owned().map_err(WriteError::DupFd)?;
		self.fds.push(Attachment::Fd(fd));
		Ok(())
	}
	pub fn write_owned_fd(&mut self, fd: OwnedFd) -> Result<(), WriteError> {
		self.fds.push(Attachment::Fd(fd));
		Ok(())
	}
	pub fn write_ref(&mut self, node_ref: &impl ToRef) -> Result<(), WriteError> {
		self.fds.push(Attachment::Ref(node_ref.to_ref()));
		Ok(())
	}
}

// the ints
impl DataBuilder {
	pub fn write_u64(&mut self, int: u64) -> Result<(), WriteError> {
		self.data.extend_from_slice(&int.to_le_bytes());
		Ok(())
	}
	pub fn write_i64(&mut self, int: i64) -> Result<(), WriteError> {
		self.data.extend_from_slice(&int.to_le_bytes());
		Ok(())
	}
	pub fn write_u32(&mut self, int: u32) -> Result<(), WriteError> {
		self.data.extend_from_slice(&int.to_le_bytes());
		Ok(())
	}
	pub fn write_i32(&mut self, int: i32) -> Result<(), WriteError> {
		self.data.extend_from_slice(&int.to_le_bytes());
		Ok(())
	}
	pub fn write_u16(&mut self, int: u16) -> Result<(), WriteError> {
		self.data.extend_from_slice(&int.to_le_bytes());
		Ok(())
	}
	pub fn write_i16(&mut self, int: i16) -> Result<(), WriteError> {
		self.data.extend_from_slice(&int.to_le_bytes());
		Ok(())
	}
	pub fn write_u8(&mut self, int: u8) -> Result<(), WriteError> {
		self.data.extend_from_slice(&int.to_le_bytes());
		Ok(())
	}
	pub fn write_i8(&mut self, int: i8) -> Result<(), WriteError> {
		self.data.extend_from_slice(&int.to_le_bytes());
		Ok(())
	}
}
#[derive(Debug, Error)]
pub enum WriteError {
	#[error("String is longer than u32::MAX bytes")]
	StringToLong,
	#[error("List is longer than u32::MAX items")]
	ListToLong,
	#[error("Could not duplicate borrowed fd: {0}")]
	DupFd(#[source] std::io::Error),
}

impl DataReader {
	pub fn read_string(&mut self) -> Result<String, ReadError> {
		let len = self.read_u32()?;
		let data = self.read_bytes(len as usize)?;
		Ok(String::from_utf8(data.to_vec())?)
	}
	pub fn read_f64(&mut self) -> Result<f64, ReadError> {
		let bytes = self.read_bytes(size_of::<f64>())?;
		Ok(f64::from_le_bytes(
			bytes.try_into().map_err(|_| ReadError::NotEnoughBytes)?,
		))
	}
	pub fn read_f32(&mut self) -> Result<f32, ReadError> {
		let bytes = self.read_bytes(size_of::<f32>())?;
		Ok(f32::from_le_bytes(
			bytes.try_into().map_err(|_| ReadError::NotEnoughBytes)?,
		))
	}
	pub fn read_bool(&mut self) -> Result<bool, ReadError> {
		Ok(self.read_u8()? != 0)
	}
	pub fn read_fd(&mut self) -> Result<OwnedFd, ReadError> {
		self.fds.next().ok_or(ReadError::MissingDescriptor)
	}
	pub fn read_ref(&mut self) -> Result<Ref, ReadError> {
		self.read_fd().map(Ref::from_owned_fd)
	}
}

// the ints
impl DataReader {
	pub fn read_u64(&mut self) -> Result<u64, ReadError> {
		let bytes = self.read_bytes(size_of::<u64>())?;
		Ok(u64::from_le_bytes(
			bytes.try_into().map_err(|_| ReadError::NotEnoughBytes)?,
		))
	}
	pub fn read_i64(&mut self) -> Result<i64, ReadError> {
		let bytes = self.read_bytes(size_of::<i64>())?;
		Ok(i64::from_le_bytes(
			bytes.try_into().map_err(|_| ReadError::NotEnoughBytes)?,
		))
	}
	pub fn read_u32(&mut self) -> Result<u32, ReadError> {
		let bytes = self.read_bytes(size_of::<u32>())?;
		Ok(u32::from_le_bytes(
			bytes.try_into().map_err(|_| ReadError::NotEnoughBytes)?,
		))
	}
	pub fn read_i32(&mut self) -> Result<i32, ReadError> {
		let bytes = self.read_bytes(size_of::<i32>())?;
		Ok(i32::from_le_bytes(
			bytes.try_into().map_err(|_| ReadError::NotEnoughBytes)?,
		))
	}
	pub fn read_u16(&mut self) -> Result<u16, ReadError> {
		let bytes = self.read_bytes(size_of::<u16>())?;
		Ok(u16::from_le_bytes(
			bytes.try_into().map_err(|_| ReadError::NotEnoughBytes)?,
		))
	}
	pub fn read_i16(&mut self) -> Result<i16, ReadError> {
		let bytes = self.read_bytes(size_of::<i16>())?;
		Ok(i16::from_le_bytes(
			bytes.try_into().map_err(|_| ReadError::NotEnoughBytes)?,
		))
	}
	pub fn read_u8(&mut self) -> Result<u8, ReadError> {
		let bytes = self.read_bytes(size_of::<u8>())?;
		Ok(u8::from_le_bytes(
			bytes.try_into().map_err(|_| ReadError::NotEnoughBytes)?,
		))
	}
	pub fn read_i8(&mut self) -> Result<i8, ReadError> {
		let bytes = self.read_bytes(size_of::<i8>())?;
		Ok(i8::from_le_bytes(
			bytes.try_into().map_err(|_| ReadError::NotEnoughBytes)?,
		))
	}
}

#[derive(Debug, Error)]
pub enum ReadError {
	#[error("Not enough bytes for type")]
	NotEnoughBytes,
	#[error("Message carried fewer descriptors than the schema expects")]
	MissingDescriptor,
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
	/// The peer is alive but behind: its socket buffer and outbound queue are both full.
	///
	/// binder had no equivalent — it blocked instead. gluon's one-way sends never block,
	/// so backpressure surfaces here and the message was **not** delivered.
	#[error("The peer's outbound queue is full")]
	Full,
	#[error("The peer is gone")]
	Closed,
	#[error("Payload is {size} bytes, over the {MAX_MESSAGE_SIZE} byte limit")]
	TooLarge { size: usize },
	#[error("Could not create the reply object: {0}")]
	Node(#[from] NodeError),
}
impl From<TrySendError> for SendError {
	fn from(err: TrySendError) -> Self {
		match err {
			TrySendError::Full(_) => SendError::Full,
			TrySendError::TooLarge(m) => SendError::TooLarge {
				size: m.data().len(),
			},
			TrySendError::Closed(_) => SendError::Closed,
		}
	}
}

/// Who sent the transaction being handled, as reported by the kernel.
///
/// The credentials are an `Option` because `SCM_CREDENTIALS` is what supplies them.
/// strong-ipc sets `SO_PASSCRED` on every socket it receives on, so in practice they are
/// always present — but a peer not going through this crate is not obliged to cooperate,
/// and a handler that gates on identity should treat `None` as "unknown", never as
/// "trusted".
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Context {
	creds: Option<UCred>,
}

impl Context {
	pub fn new(creds: Option<UCred>) -> Self {
		Self { creds }
	}
	pub fn creds(&self) -> Option<UCred> {
		self.creds
	}
	pub fn sender_pid(&self) -> Option<RawPid> {
		self.creds.map(|c| c.pid.as_raw_nonzero().get())
	}
	pub fn sender_uid(&self) -> Option<RawUid> {
		self.creds.map(|c| c.uid.as_raw())
	}
	pub fn sender_gid(&self) -> Option<RawGid> {
		self.creds.map(|c| c.gid.as_raw())
	}
}

/// Handle to reply to a call whose return value is being sent back asynchronously,
/// separately from the `_oneway` dispatch future completing. Call `send(value)` with
/// the same value the corresponding non-`_oneway` method would have returned; `encode`
/// (supplied by codegen when the sender is constructed) knows how to convert and write
/// that value onto the wire, so callers never touch a `DataBuilder` directly.
pub struct ReplySender<T> {
	callback: Ref,
	encode: fn(T, &mut DataBuilder) -> Result<(), WriteError>,
}

impl<T> std::fmt::Debug for ReplySender<T> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_struct("ReplySender").finish_non_exhaustive()
	}
}

impl<T> ReplySender<T> {
	pub fn new(callback: Ref, encode: fn(T, &mut DataBuilder) -> Result<(), WriteError>) -> Self {
		Self { callback, encode }
	}

	pub fn send(self, value: T) -> Result<(), SendError> {
		let mut payload = DataBuilder::new();
		(self.encode)(value, &mut payload)?;
		transact(&self.callback, REPLY_CODE, payload)
	}
}
/// A wrapper for the throwaway node a proxy hands out to receive one reply.
pub struct ReturnReceiver(Node<ReturnHandler>, mpsc::Receiver<DataReader>);
impl ReturnReceiver {
	pub fn new() -> Result<(Self, Ref), SendError> {
		let (handler, recv) = ReturnHandler::new();
		let (node, node_ref) = Node::new(handler)?;
		Ok((Self(node, recv), node_ref))
	}
	pub async fn recv(&mut self) -> Result<DataReader, SendError> {
		tokio::select! {
			// this unwrap should be fine since we
			v = self.1.recv() => { Ok(v.unwrap()) }
			_ = self.0.death_notification() => { Err(SendError::Closed) }
		}
	}
}

/// The handler behind the throwaway node a proxy hands out to receive one reply.
struct ReturnHandler(mpsc::Sender<DataReader>);

impl std::fmt::Debug for ReturnHandler {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_struct("ReturnHandler").finish()
	}
}
impl Handler for ReturnHandler {
	async fn handle(&self, data: &mut [u8], fds: FdVec, _creds: Option<UCred>) {
		let Ok((_code, reader)) = DataReader::from_wire(data, fds) else {
			return;
		};
		_ = self.0.send(reader).await;
	}
}
impl ReturnHandler {
	pub fn new() -> (Self, mpsc::Receiver<DataReader>) {
		let (tx, rx) = mpsc::channel(1);
		(Self(tx), rx)
	}
}

#[cfg(test)]
mod tests {
	use std::marker::PhantomData;

	// The derive emits `gluon_ipc::` paths; alias this crate so those paths
	// resolve when the tests are compiled as part of `gluon-wire` itself.
	extern crate self as gluon_ipc;

	use super::*;

	fn assert_handler<T: Handler>() {}

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
	fn plain_handler_is_handler() {
		assert_handler::<PlainHandler>();
	}

	#[test]
	fn generic_handler_is_handler() {
		assert_handler::<GenericHandler<u32>>();
	}

	#[test]
	fn where_clause_handler_is_handler() {
		assert_handler::<WhereHandler<String>>();
	}

	#[test]
	fn code_and_payload_round_trip() {
		let mut builder = DataBuilder::new();
		builder.write_u32(7).unwrap();
		builder.write_str("hello").unwrap();
		let message = builder.finish(12);

		let (code, mut reader) = DataReader::from_wire(message.data(), FdVec::new()).unwrap();
		assert_eq!(code, 12);
		assert_eq!(reader.read_u32().unwrap(), 7);
		assert_eq!(reader.read_string().unwrap(), "hello");
	}

	#[test]
	fn short_message_has_no_code() {
		assert!(matches!(
			DataReader::from_wire(&[0, 1], FdVec::new()),
			Err(ReadError::NotEnoughBytes)
		));
	}

	#[test]
	fn missing_descriptor_is_an_error() {
		let message = DataBuilder::new().finish(0);
		let (_, mut reader) = DataReader::from_wire(message.data(), FdVec::new()).unwrap();
		assert!(matches!(
			reader.read_fd(),
			Err(ReadError::MissingDescriptor)
		));
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
