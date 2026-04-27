//! Debug utilities for catamorphisms and other morphisms

use super::super::super::PathMap;
use super::super::super::alloc::Allocator;
use super::super::super::morphisms::{DoCache, into_cata_cached_body};
use super::super::super::utils::ByteMask;
use super::super::super::zipper::*;

/// Debug extension trait for catamorphisms
///
/// This trait provides debug-only catamorphism methods that may expose additional
/// information useful for debugging and development.
pub trait CatamorphismDebug<V> {
    /// A version of [`into_cata_jumping_cached`](super::super::super::morphisms::Catamorphism::into_cata_jumping_cached) where
    /// the full path is available to the closure; **For debugging purposes only**
    ///
    /// Using data from the full path for your algorithm **will** lead to incorrect behavior.
    /// You must either adapt your algorithm not to require full path data or use the one of
    /// the `_side_effect` methods.
    fn into_cata_jumping_cached_fallible_debug<W, E, AlgF>(self, alg_f: AlgF) -> Result<W, E>
    where
        W: Clone,
        AlgF: Fn(&ByteMask, &mut [W], Option<&V>, &[u8], &[u8]) -> Result<W, E>;
}

impl<'a, Z, V: 'a> CatamorphismDebug<V> for Z
where
    Z: Zipper
        + ZipperReadOnlyConditionalValues<'a, V>
        + ZipperConcrete
        + ZipperAbsolutePath
        + ZipperPathBuffer,
{
    fn into_cata_jumping_cached_fallible_debug<W, E, AlgF>(self, alg_f: AlgF) -> Result<W, E>
    where
        W: Clone,
        AlgF: Fn(&ByteMask, &mut [W], Option<&V>, &[u8], &[u8]) -> Result<W, E>,
    {
        into_cata_cached_body::<Self, V, W, E, _, DoCache, true, true>(
            self,
            |mask, children, val, sub_path, debug_path, _z| {
                alg_f(mask, children, val, sub_path, debug_path)
            },
        )
    }
}

impl<V: 'static + Clone + Send + Sync + Unpin, A: Allocator + 'static> CatamorphismDebug<V>
    for PathMap<V, A>
{
    fn into_cata_jumping_cached_fallible_debug<W, E, AlgF>(self, alg_f: AlgF) -> Result<W, E>
    where
        W: Clone,
        AlgF: Fn(&ByteMask, &mut [W], Option<&V>, &[u8], &[u8]) -> Result<W, E>,
    {
        let rz = self.into_read_zipper([]);
        rz.into_cata_jumping_cached_fallible_debug(alg_f)
    }
}
