#[cfg(doc)]
use super::zipper::*;

/// Derive macro to implement *most* zipper traits on an enum designed to act as a polymorphic zipper
///
/// A polymorphic zipper is a zipper that can represent different underlying zipper kinds, and dispatch
/// to the appropriate type at runtime.  Similar in concept to an `&dyn` reference.
///
/// The `PolyZipper` macro implements the following traits, provided they are implemented on each of the enum variants:
/// * [`Zipper`]
/// * [`ZipperAbsolutePath`]
/// * [`ZipperConcrete`]
/// * [`ZipperIteration`]
/// * [`ZipperMoving`]
/// * [`ZipperPathBuffer`]
/// * [`ZipperReadOnlyConditionalIteration`]
/// * [`ZipperReadOnlyConditionalValues`]
/// * [`ZipperReadOnlyIteration`]
/// * [`ZipperReadOnlyValues`]
/// * [`ZipperValues`]
///
/// NOTE: This macro does not derive an impl for [`ZipperForking`]
/// because the mapping between child zipper types and the output type is not always straightforward.
/// Therefore it is recommended to implement `ZipperForking` yourself.
///
/// [`ZipperWriting`] and other write zipper trait are also not supported currently. That decision is
/// not fundamental and additional impls could be added in the future.
///
/// ## USAGE:
/// The generic parameter names: `'trie`, `'path`, `V`, and `A` have special meaning to
/// the traits that require them.  `V` must be specified as a generic type paremeter, even if
/// you intend to specify a default type.
///
/// ```ignore
/// use petta::pathmap::zipper::{PolyZipper, ReadZipperTracked, ReadZipperUntracked};
///
/// #[derive(PolyZipper)]
/// enum MyPolyZipper<'trie, 'path, V: Clone + Send + Sync + Unpin = ()> {
///     Tracked(ReadZipperTracked<'trie, 'path, V>),
///     Untracked(ReadZipperUntracked<'trie, 'path, V>),
/// }
/// ```ignore
pub use pathmap_derive::PolyZipper;

/// Variant of [`PolyZipper`] that allows selecting which traits are derived.
///
/// This macro was created to work around the inability of the Rust trait solver to handle fixed point
/// recursion.  It will be deprecated in the future when Rust is improved.
///
/// Usage:
/// ```ignore
/// use petta::pathmap::zipper::{PolyZipperExplicit, ReadZipperTracked, ReadZipperUntracked};
///
/// #[derive(PolyZipperExplicit)]
/// #[poly_zipper_explicit(traits(Zipper, ZipperMoving, ZipperIteration))]
/// enum MyPolyZipper<'trie, 'path, V: Clone + Send + Sync + Unpin = ()> {
///     Tracked(ReadZipperTracked<'trie, 'path, V>),
///     Untracked(ReadZipperUntracked<'trie, 'path, V>),
/// }
/// ```ignore
pub use pathmap_derive::PolyZipperExplicit;

#[cfg(all(test, feature = "pathmap-internal-tests"))]
mod tests {
    use super::super::PathMap;
    use super::zipper::*;
    use crate as pathmap;

    #[cfg(feature = "arena_compact")]
    use super::arena_compact::{ACTMmapZipper, ACTVec, ACTVecZipper};

    #[cfg(not(feature = "arena_compact"))]
    #[derive(PolyZipper)]
    pub enum TestPolyZipper<'trie, V: Clone + Send + Sync + Unpin = ()> {
        /// Tracked PathMap read zipper
        PathMap(ReadZipperTracked<'trie, 'trie, V>),
        /// Unracked PathMap read zipper.
        /// The reason this exists is to allow forking the zipper.
        /// Forking a Tracked read zipper return an Untracked read zipper.
        PathMapU(ReadZipperUntracked<'trie, 'trie, V>),
    }

    #[cfg(feature = "arena_compact")]
    #[derive(PolyZipper)]
    pub enum TestPolyZipper<'trie, V: Clone + Send + Sync + Unpin = ()> {
        /// Tracked PathMap read zipper
        PathMap(ReadZipperTracked<'trie, 'trie, V>),
        /// Unracked PathMap read zipper.
        /// The reason this exists is to allow forking the zipper.
        /// Forking a Tracked read zipper return an Untracked read zipper.
        PathMapU(ReadZipperUntracked<'trie, 'trie, V>),
        /// Memory-mapped ACT.
        /// Prefix is necessary here such that MORK can see ACT under a specific path
        ACTMmapPrefix(PrefixZipper<'trie, ACTMmapZipper<'trie, V>>),
        /// `Vec<u8>` based ACT
        /// Prefix is necessary here such that MORK can see ACT under a specific path
        ACTVecPrefix(PrefixZipper<'trie, ACTVecZipper<'trie, V>>),
    }

    super::zipper::zipper_moving_tests::zipper_moving_tests!(
        poly_zipper_pm,
        |keys: &[&[u8]]| { keys.iter().map(|k| (k, ())).collect::<PathMap<()>>() },
        |btm: &mut PathMap<()>, path: &[u8]| -> _ {
            TestPolyZipper::PathMapU(btm.read_zipper_at_path(path))
        }
    );

    super::zipper::zipper_iteration_tests::zipper_iteration_tests!(
        poly_zipper_pm,
        |keys: &[&[u8]]| { keys.iter().map(|k| (k, ())).collect::<PathMap<()>>() },
        |btm: &mut PathMap<()>, path: &[u8]| -> _ {
            TestPolyZipper::PathMapU(btm.read_zipper_at_path(path))
        }
    );

    #[cfg(feature = "arena_compact")]
    super::zipper::zipper_moving_tests::zipper_moving_tests!(
        poly_zipper_act,
        |keys: &[&[u8]]| {
            let btm = keys.iter().map(|k| (k, ())).collect::<PathMap<()>>();
            ACTVec::from_zipper(btm.read_zipper(), |()| 0)
        },
        |act: &mut ACTVec, path: &[u8]| -> _ {
            TestPolyZipper::ACTVecPrefix(PrefixZipper::new(&[], act.read_zipper_at_path(path)))
        }
    );

    #[cfg(feature = "arena_compact")]
    super::zipper::zipper_iteration_tests::zipper_iteration_tests!(
        poly_zipper_act,
        |keys: &[&[u8]]| {
            let btm = keys.iter().map(|k| (k, ())).collect::<PathMap<()>>();
            ACTVec::from_zipper(btm.read_zipper(), |()| 0)
        },
        |act: &mut ACTVec, path: &[u8]| -> _ {
            TestPolyZipper::ACTVecPrefix(PrefixZipper::new(&[], act.read_zipper_at_path(path)))
        }
    );

    // ======================================================================================
    // Cocktail of recursive zipper madness
    #[derive(PolyZipperExplicit)]
    #[poly_zipper_explicit(traits(Zipper, ZipperMoving, ZipperIteration))]
    pub enum ExprFactor<'trie, V: Clone + Send + Sync + Unpin + 'static = ()> {
        Specific(ReadZipperOwned<V>),
        Generic(
            PrefixZipper<
                'trie,
                DependentProductZipperG<
                    'trie,
                    Box<ExprFactor<'trie, V>>,
                    ExprFactor<'trie, V>,
                    V,
                    (),
                    for<'a> fn((), &'a [u8], usize) -> ((), Option<ExprFactor<'trie, V>>),
                >,
            >,
        ),
    }

    super::zipper::zipper_moving_tests::zipper_moving_tests!(
        recursive_zipper_madness,
        |keys: &[&[u8]]| { keys.iter().map(|k| (k, ())).collect::<PathMap<()>>() },
        |btm: &mut PathMap<()>, path: &[u8]| -> _ {
            ExprFactor::Specific(btm.clone().into_read_zipper(path))
        }
    );
}
