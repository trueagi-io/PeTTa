
/// Wrapper around `std::alloc::Allocator` trait, shims to the `allocator_api` on nightly, does nothing on `stable`
#[cfg(not(feature = "nightly"))]
pub trait Allocator: Clone + Send + Sync {}

#[cfg(not(feature = "nightly"))]
impl Allocator for () {}

/// Wrapper around `std::alloc::Global`, shims to the `allocator_api` on nightly, does nothing on `stable`
#[cfg(not(feature = "nightly"))]
pub type GlobalAlloc = ();

/// Instantiates the GlobalAlloc type, to work around the "type alias can't use a type alias as a constructor" error
#[cfg(not(feature = "nightly"))]
pub const fn global_alloc() -> GlobalAlloc {()}

/// Wrapper around `std::alloc::Allocator` trait, shims to the `allocator_api` on nightly, does nothing on `stable`
#[cfg(feature = "nightly")]
pub trait Allocator: std::alloc::Allocator + Clone + Send + Sync {}

#[cfg(feature = "nightly")]
impl<T> Allocator for T where T: std::alloc::Allocator + Clone + Send + Sync {}

/// Wrapper around `std::alloc::Global`, shims to the `allocator_api` on nightly, does nothing on `stable`
#[cfg(feature = "nightly")]
pub type GlobalAlloc = std::alloc::Global;

/// Instantiates the GlobalAlloc type, to work around the "type alias can't use a type alias as a constructor" error
#[cfg(feature = "nightly")]
pub const fn global_alloc() -> GlobalAlloc {std::alloc::Global}
