
use maybe_dangling::MaybeDangling;
use core::ptr::NonNull;

use super::alloc::{Allocator, GlobalAlloc};
use super::utils::{ByteMask, BitMask};
use super::trie_core::node::*;
use super::trie_core::r#ref::{TrieRef, TrieRefOwned};
use super::PathMap;
use super::zipper::*;
use super::zipper::zipper_priv::*;
use super::zipper_tracking::*;
use super::ring::{AlgebraicResult, AlgebraicStatus, DistributiveLattice, Lattice, COUNTER_IDENT, SELF_IDENT};

/// Implemented on [Zipper] types that allow modification of the trie
//GOAT: Long term, the ZipperMoving bound doesn't belong here.  But we'll want to break ZipperWriting into a
// separate trait that allows writing at a specific path that we can also implement in PathMap
pub trait ZipperWriting<V: Clone + Send + Sync, A: Allocator = GlobalAlloc>: WriteZipperPriv<V, A> + ZipperMoving {
    /// A [ZipperHead] that can be created from this zipper
    type ZipperHead<'z> where Self: 'z;

    /// Returns a mutable reference to a value at the zipper's focus, or None if no value exists
    fn get_val_mut(&mut self) -> Option<&mut V>;

    /// Deprecated alias for [ZipperWriting::get_val_mut]
    #[deprecated] //GOAT-old-names
    fn get_value_mut(&mut self) -> Option<&mut V> {
        self.get_val_mut()
    }

    /// Returns a mutable reference to the value at the zipper's focus, inserting `default` if no
    /// value exists
    fn get_val_or_set_mut(&mut self, default: V) -> &mut V;

    /// Deprecated alias for [ZipperWriting::get_val_or_set_mut]
    #[deprecated] //GOAT-old-names
    fn get_value_or_insert(&mut self, default: V) -> &mut V {
        self.get_val_or_set_mut(default)
    }

    /// Returns a mutable reference to the value at the zipper's focus, inserting the result of `func`
    /// if no value exists
    fn get_val_or_set_mut_with<F>(&mut self, func: F) -> &mut V where F: FnOnce() -> V;

    /// Deprecated alias for [ZipperWriting::get_val_or_set_mut_with]
    #[deprecated] //GOAT-old-names
    fn get_value_or_insert_with<F>(&mut self, func: F) -> &mut V where F: FnOnce() -> V {
        self.get_val_or_set_mut_with(func)
    }

    /// Sets the value at the zipper's focus
    ///
    /// Returns `Some(replaced_val)` if an existing value was replaced, otherwise returns `None` if
    /// the value was added without replacing anything.
    fn set_val(&mut self, val: V) -> Option<V>;

    /// Deprecated alias for [ZipperWriting::set_val]
    #[deprecated] //GOAT-old-names
    fn set_value(&mut self, val: V) -> Option<V> {
        self.set_val(val)
    }

    /// Removes the value at the zipper's focus.  Does not affect any onward branches.  Returns `Some(val)`
    /// with the value that was removed, otherwise returns `None`
    ///
    /// Pass `true` to the `prune` argument to automatically remove any dangling path created by this operation.
    fn remove_val(&mut self, prune: bool) -> Option<V>;

    /// Deprecated alias for [ZipperWriting::remove_val]
    #[deprecated] //GOAT-old-names
    fn remove_value(&mut self) -> Option<V> {
        self.remove_val(true)
    }

    /// Creates a [ZipperHead] at the zipper's current focus
    fn zipper_head<'z>(&'z mut self) -> Self::ZipperHead<'z>;

    /// Replaces the trie below the zipper's focus with the subtrie downstream from the focus of `read_zipper`
    ///
    /// If there is a value at the zipper's focus, it will not be affected.
    /// GOAT: This method's behavior is affected by the `graft_root_vals` feature
    ///
    /// NOTE: If the `read_zipper` is not on an existing path (according to [Zipper::path_exists]) then the
    /// effect will be the same as [remove_branches](ZipperWriting::remove_branches)
    fn graft<Z: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &Z);

    /// Replaces the trie below the zipper's focus with the contents of a [PathMap], consuming the map
    ///
    /// If there is a value at the zipper's focus, it will not be affected.
    /// GOAT: This method's behavior is affected by the `graft_root_vals` feature
    ///
    /// NOTE: If the `map` is empty then the effect will be the same as [remove_branches](ZipperWriting::remove_branches)
    fn graft_map(&mut self, map: PathMap<V, A>);

    /// Grafts each [PathMap] returned by the `maps` iterator at the corresponding child byte indicated by a 
    /// set bit in `child_mask`.
    ///
    /// If `remove_unset` is `true` then [Zipper::child_mask] will be equal to `child_mask` when this operations
    /// completes.  If it is `false`, then the branches corresponding to set bits will be grafted, but branches
    /// corresponding to unset bits will be left alone.
    ///
    /// Panics if the `maps` iterator returns fewer maps than the number of set bits in `child_mask`
    fn graft_child_maps<I: IntoIterator<Item=PathMap<V, A>>>(&mut self, child_mask: ByteMask, maps: I, remove_unset: bool) {
        if remove_unset {
            self.remove_unmasked_branches(child_mask, false);
        }

        let mut maps_iter = maps.into_iter();
        for child_byte in child_mask.iter() {
            let map = maps_iter.next().expect("maps iterator returned fewer items than the number of set bits in child_mask");
            self.descend_to_byte(child_byte);
            self.graft_map(map);
            self.ascend_byte();
        }
    }

    /// Grafts each child of `read_zipper` masked by `child_mask`
    fn graft_children<Z: ZipperInfallibleSubtries<V, A> + ZipperMoving>(&mut self, read_zipper: &mut Z, child_mask: ByteMask) {
        let rz_mask = read_zipper.child_mask();
        let actual_mask = child_mask & rz_mask;
        for child_byte in actual_mask.iter() {
            self.descend_to_byte(child_byte);
            read_zipper.descend_to_byte(child_byte);

            self.graft(read_zipper);

            read_zipper.ascend_byte();
            self.ascend_byte();
        }
    }

    /// Joins (union of) the subtrie below the focus of `read_zipper` into the subtrie downstream from the
    /// focus of `self`
    ///
    /// GOAT: Should the ordinary zipper alg ops also be affected by `graft_root_vals` behavior?
    /// In other words, should we join, meet, subtract, etc. the values at the zipper focus as well??
    /// It actually makes sense that the answer should be yes.  If this is the decision, the `join_map_into`
    /// method has an implementation that could likely be factord out and shared among all the ops.
    ///
    /// If the `self` zipper is at a path that does not exist, this method behaves like [graft](ZipperWriting::graft).
    fn join_into<Z: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &Z) -> AlgebraicStatus where V: Lattice;

    /// Depracated alias for [ZipperWriting::join_into].  Likely to be removed in the future to make way
    /// for a method that interacts with two read-only arguments and returns a newly constructed subtrie or map.
    #[deprecated] //GOAT-old-names
    fn join<Z: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &Z) -> AlgebraicStatus where V: Lattice {
        self.join_into(read_zipper)
    }

    /// Joins (union of) the contents of a [PathMap] into the trie below the zipper's focus,
    /// consuming the map
    ///
    /// GOAT: This method's behavior is affected by the `graft_root_vals` feature
    /// GOAT QUESTION!!!!! Should this method join the map's root value into the value at the zipper's
    /// focus?  The argument for `yes` is that a root value is part of a map.  The argument for `no` is
    /// an analogy to `graft` and `graft_map` that currently don't bother the values.  Personally, I
    /// believe `yes` is more conceptually correct, and that the behavior of `graft` and `graft_map`
    /// should probably be revisited.  **HOWEVER** the currently implemented behavior is **NO**!
    /// This is related to a question in [ZipperInfallibleSubtries::make_map]
    fn join_map_into(&mut self, map: PathMap<V, A>) -> AlgebraicStatus where V: Lattice;

    /// Depracated alias for [ZipperWriting::join_map_into]
    #[deprecated] //GOAT-old-names
    fn join_map(&mut self, map: PathMap<V, A>) -> AlgebraicStatus where V: Lattice {
        self.join_map_into(map)
    }

    /// Joins the subtrie below the focus of `src_zipper` into the subtrie below the focus of `self`,
    /// consuming the subtrie from the `src_zipper`
    ///
    /// Pass `true` to the `prune` argument to automatically remove any dangling path created in `src_zipper`.
    fn join_into_take<Z: ZipperInfallibleSubtries<V, A> + ZipperWriting<V, A>>(&mut self, src_zipper: &mut Z, prune: bool) -> AlgebraicStatus where V: Lattice;

    /// Collapses all the paths below the zipper's focus by removing the leading `byte_cnt` bytes from
    /// each path and joins together all of the downstream subtries
    ///
    /// Returns `true` if the focus has at least one downstream continuation, otherwise returns `false`.
    ///
    /// NOTE: for legacy reasons, this operation is sometimes called `drop_head`
    fn join_k_path_into(&mut self, byte_cnt: usize, prune: bool) -> bool where V: Lattice;

    /// Collapses all the paths below the zipper's focus by removing the leading `byte_cnt` bytes from
    /// each path and meets together all of the downstream subtries
    ///
    /// Returns `true` if the focus has at least one downstream continuation, otherwise returns `false`.
    fn meet_k_path_into(&mut self, byte_cnt: usize, prune: bool) -> bool where V: Lattice;

    /// Deprecated alias for [ZipperWriting::join_k_path_into]
    #[deprecated] //GOAT-old-names
    fn drop_head(&mut self, byte_cnt: usize) -> bool where V: Lattice {
        self.join_k_path_into(byte_cnt, true)
    }

// GOAT QUESTION: Do we want to change the behavior to move the value as well?  Or do we want a variant
//  of this method that moves the value?  The main guiding idea behind not shifting the value was the desire
//  to preserve the property of being the inverse of drop_head.
    /// Inserts `prefix` in front of every downstream path at the focus
    ///
    /// This method does not affect a value at the focus, nor does it move the zipper's focus. Returns false
    /// when at a none-existent place in the trie.
    ///
    /// NOTE: This is the inverse of [Self::drop_head], although it cannot perfectly undo `drop_head` because
    /// `drop_head` loses information about the prior nested structure.  However, `drop_head` will undo this
    /// operation.
    fn insert_prefix<K: AsRef<[u8]>>(&mut self, prefix: K) -> bool;

    /// Deleted the `n` bytes from the path above the zipper's focus, including any subtries that descend
    /// from the deleted branches
    ///
    /// Returns `true` if n upstream bytes were removed from the path, otherwise returns `false`.
    //
    // GOAT: TODO, make a diagram illustrating the behavior
    fn remove_prefix(&mut self, n: usize) -> bool;

    /// Meets (retains the intersection of) the subtrie below the zipper's focus with the subtrie downstream
    /// from the focus of `read_zipper`
    fn meet_into<Z: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &Z, prune: bool) -> AlgebraicStatus where V: Lattice;

    /// Deprecated alias for [ZipperWriting::meet_into].  May be replaced in the future with a different method
    #[deprecated] //GOAT-old-names
    fn meet<Z: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &Z) -> AlgebraicStatus where V: Lattice {
        self.meet_into(read_zipper, true)
    }

    /// Experiment.  GOAT, document this
    fn meet_2<'z, ZA: ZipperInfallibleSubtries<V, A>, ZB: ZipperInfallibleSubtries<V, A>>(&mut self, rz_a: &ZA, rz_b: &ZB) -> AlgebraicStatus where V: Lattice;

    /// Subtracts the subtrie downstream of the focus of `read_zipper` from the subtrie below the `self` zipper's
    /// focus
    fn subtract_into<Z: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &Z, prune: bool) -> AlgebraicStatus where V: DistributiveLattice;

    /// Deprecated alias for [ZipperWriting::subtract_into]
    #[deprecated] //GOAT-old-names
    fn subtract<Z: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &Z) -> AlgebraicStatus where V: DistributiveLattice {
        self.subtract_into(read_zipper, true)
    }

    /// Restricts paths in the subtrie downstream of the `self` focus to paths prefixed by a path to a value in
    /// `read_zipper`
    ///
    /// NOTE: In the future this method is likely to be replaced by a "restrict" policy which may
    /// be passed as an argument to [ZipperWriting::meet_into]
    fn restrict<Z: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &Z) -> AlgebraicStatus;

    /// Populates the "stem" paths in `self` with the corresponding subtries in `read_zipper`
    ///
    /// NOTE: Any stem path without a corresponding path in `read_zipper` will be removed from `self`.
    /// Returns false if the focus was at a non-existent path in the trie.
    ///
    /// GOAT, I feel like `restricting` might not be a very evocative name here.  The way I think of this
    /// operation is as a bunch of "stems" in the WriteZipper, that get their downstream contents populated
    /// by the corresponding paths in the ReadZipper.  Ideas for names are: "blossom", "fill_in", "expound",
    /// "populate", etc.  I avoided "bloom" and "expand" because those both have other connotations.
    //GOAT, gotta document this much better and decide if a return of AlgebraicStatus is called for.  Probably.
    fn restricting<Z: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &Z) -> bool;

    /// Creates a new [PathMap] from the zipper's focus, removing all downstream branches from the zipper
    ///
    /// Pass `true` to the `prune` argument to automatically remove any dangling path created by this operation.
    ///
    /// GOAT: This method's behavior is affected by the `graft_root_vals` feature
    /// A value at the zipper's focus will not be affected, and will not be included in the resulting map.
    /// GOAT: See discussion in [ZipperInfallibleSubtries::make_map] about whether this behavior should be changed
    fn take_map(&mut self, prune: bool) -> Option<PathMap<V, A>>;

    /// Removes all branches below the zipper's focus.  Does not affect the value if there is one.  Returns `true`
    /// if a branch was removed, otherwise returns `false`
    ///
    /// Pass `true` to the `prune` argument to automatically remove any dangling path created by this operation.
    fn remove_branches(&mut self, prune: bool) -> bool;

    /// Removes multiple branches below the zipper's focus based on the supplied 256-bit `mask`
    ///
    /// Key bytes for which the corresponding `mask` bit is `0` will be removed.
    ///
    /// Pass `true` to the `prune` argument to automatically remove any dangling path created by this operation.
    fn remove_unmasked_branches(&mut self, mask: ByteMask, prune: bool);

    /// Creates a dangling path to the current zipper focus.  Returns `true` if new path bytes were created, or
    /// `false` if the path already existed
    ///
    /// Calling this method will guarantee that [Zipper::path_exists] subsequently returns `true` for this location.
    fn create_path(&mut self) -> bool;

    /// Prunes a dangling path above the zipper's focus.  Returns the number of path bytes removed
    ///
    /// Calling this method may result in [Zipper::path_exists] subsequently returning `false`, where it previously returned `true`
    ///
    /// This method cannot prune the trie above the zipper's root.
    /// This method has no effect if there is a value at the focus, or any downstream paths from the focus.
    fn prune_path(&mut self) -> usize;

    /// Convenience to prune and ascend the zipper to the first non-dangling path
    ///
    /// Equivalent to:
    /// ```ignoreignore
    /// let bytes = z.prune_path();
    /// z.ascend(bytes);
    /// return bytes
    /// ```ignore
    //NOTE: This ought to go in a trait that's a supertrait on both `ZipperWriting` and `ZipperMoving`,
    // however, there currently isn't such a thing; hence the lack of default impl.  If we implement
    // non-moving `ZipperWriting` types in the furure, we may want to move this method
    fn prune_ascend(&mut self) -> usize;
}

pub(crate) mod write_zipper_priv {
    use super::*;

    pub trait WriteZipperPriv<V: Clone + Send + Sync, A: Allocator> {
        fn take_focus(&mut self, prune: bool) -> Option<TrieNodeODRc<V, A>>;
        /// Takes the root_prefix_path from a zipper, and leaves its buffers in an "unprepared" state
        fn take_root_prefix_path(&mut self) -> Vec<u8>;
        /// Returns the allocator belonging to the WZ
        fn alloc(&self) -> A;
    }
}
use write_zipper_priv::*;

impl<V: Clone + Send + Sync, Z, A: Allocator> ZipperWriting<V, A> for &mut Z where Z: ZipperWriting<V, A> {
    type ZipperHead<'z> = Z::ZipperHead<'z> where Self: 'z;
    fn get_val_mut(&mut self) -> Option<&mut V> { (**self).get_val_mut() }
    fn get_val_or_set_mut(&mut self, default: V) -> &mut V { (**self).get_val_or_set_mut(default) }
    fn get_val_or_set_mut_with<F>(&mut self, func: F) -> &mut V where F: FnOnce() -> V { (**self).get_val_or_set_mut_with(func) }
    fn set_val(&mut self, val: V) -> Option<V> { (**self).set_val(val) }
    fn remove_val(&mut self, prune: bool) -> Option<V> { (**self).remove_val(prune) }
    fn zipper_head<'z>(&'z mut self) -> Self::ZipperHead<'z> { (**self).zipper_head() }
    fn graft<RZ: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &RZ) { (**self).graft(read_zipper) }
    fn graft_map(&mut self, map: PathMap<V, A>) { (**self).graft_map(map) }
    fn graft_child_maps<I: IntoIterator<Item=PathMap<V, A>>>(&mut self, child_mask: ByteMask, maps: I, remove_unset: bool) { (**self).graft_child_maps(child_mask, maps, remove_unset) }
    fn join_into<RZ: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &RZ) -> AlgebraicStatus where V: Lattice { (**self).join_into(read_zipper) }
    fn join_map_into(&mut self, map: PathMap<V, A>) -> AlgebraicStatus where V: Lattice { (**self).join_map_into(map) }
    fn join_into_take<RZ: ZipperInfallibleSubtries<V, A> + ZipperWriting<V, A>>(&mut self, src_zipper: &mut RZ, prune: bool) -> AlgebraicStatus where V: Lattice { (**self).join_into_take(src_zipper, prune) }
    fn join_k_path_into(&mut self, byte_cnt: usize, prune: bool) -> bool where V: Lattice { (**self).join_k_path_into(byte_cnt, prune) }
    fn meet_k_path_into(&mut self, byte_cnt: usize, prune: bool) -> bool where V: Lattice { (**self).meet_k_path_into(byte_cnt, prune) }
    fn insert_prefix<K: AsRef<[u8]>>(&mut self, prefix: K) -> bool { (**self).insert_prefix(prefix) }
    fn remove_prefix(&mut self, n: usize) -> bool { (**self).remove_prefix(n) }
    fn meet_into<RZ: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &RZ, prune: bool) -> AlgebraicStatus where V: Lattice { (**self).meet_into(read_zipper, prune) }
    fn meet_2<RZA: ZipperInfallibleSubtries<V, A>, RZB: ZipperInfallibleSubtries<V, A>>(&mut self, rz_a: &RZA, rz_b: &RZB) -> AlgebraicStatus where V: Lattice { (**self).meet_2(rz_a, rz_b) }
    fn subtract_into<RZ: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &RZ, prune: bool) -> AlgebraicStatus where V: DistributiveLattice { (**self).subtract_into(read_zipper, prune) }
    fn restrict<RZ: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &RZ) -> AlgebraicStatus { (**self).restrict(read_zipper) }
    fn restricting<RZ: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &RZ) -> bool { (**self).restricting(read_zipper) }
    fn take_map(&mut self, prune: bool) -> Option<PathMap<V, A>> { (**self).take_map(prune) }
    fn remove_branches(&mut self, prune: bool) -> bool { (**self).remove_branches(prune) }
    fn remove_unmasked_branches(&mut self, mask: ByteMask, prune: bool) { (**self).remove_unmasked_branches(mask, prune) }
    fn create_path(&mut self) -> bool { (**self).create_path() }
    fn prune_path(&mut self) -> usize { (**self).prune_path() }
    fn prune_ascend(&mut self) -> usize { (**self).prune_ascend() }
}

impl<V: Clone + Send + Sync, Z, A: Allocator> WriteZipperPriv<V, A> for &mut Z where Z: WriteZipperPriv<V, A> {
    fn take_focus(&mut self, prune: bool) -> Option<TrieNodeODRc<V, A>> { (**self).take_focus(prune) }
    fn take_root_prefix_path(&mut self) -> Vec<u8> { (**self).take_root_prefix_path() }
    fn alloc(&self) -> A { (**self).alloc() }
}

// ***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---
// WriteZipperTracked
// ***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---

/// A [write zipper](ZipperWriting) type for editing and adding paths and values in the trie
pub struct WriteZipperTracked<'a, 'path, V: Clone + Send + Sync, A: Allocator = GlobalAlloc> {
    z: WriteZipperCore<'a, 'path, V, A>,
    _tracker: Option<ZipperTracker<TrackingWrite>>,
}

//The Drop impl ensures the tracker gets dropped at the right time
impl<V: Clone + Send + Sync, A: Allocator> Drop for WriteZipperTracked<'_, '_, V, A> {
    fn drop(&mut self) { }
}

impl<'a, V: Clone + Send + Sync + Unpin, A: Allocator + 'a> Zipper for WriteZipperTracked<'a, '_, V, A>{
    fn path_exists(&self) -> bool { self.z.path_exists() }
    fn is_val(&self) -> bool { self.z.is_val() }
    fn child_count(&self) -> usize { self.z.child_count() }
    fn child_mask(&self) -> ByteMask { self.z.child_mask() }
}

impl<'a, V: Clone + Send + Sync + Unpin, A: Allocator + 'a> ZipperValues<V> for WriteZipperTracked<'a, '_, V, A>{
    fn val(&self) -> Option<&V> { self.z.val() }
}

impl<'trie, V: Clone + Send + Sync + Unpin, A: Allocator + 'trie> ZipperForking<V> for WriteZipperTracked<'trie, '_, V, A>{
    type ReadZipperT<'a> = ReadZipperUntracked<'a, 'a, V, A> where Self: 'a;
    fn fork_read_zipper<'a>(&'a self) -> Self::ReadZipperT<'a> {
        let rz_core = self.z.fork_read_zipper();
        Self::ReadZipperT::new_forked_with_inner_zipper(rz_core)
    }
}

impl<'a, V: Clone + Send + Sync + Unpin, A: Allocator + 'a> ZipperSubtries<V, A> for WriteZipperTracked<'a, '_, V, A>{
    fn native_subtries(&self) -> bool { true }
    fn try_make_map(&self) -> Option<PathMap<V, A>> { Some(self.z.make_map()) }
    fn trie_ref(&self) -> Option<TrieRef<'_, V, A>> { Some(self.z.get_trie_ref()) }
    fn alloc(&self) -> A { self.z.alloc.clone() }
}

impl<'a, V: Clone + Send + Sync + Unpin, A: Allocator + 'a> ZipperInfallibleSubtries<V, A> for WriteZipperTracked<'a, '_, V, A>{
    fn make_map(&self) -> PathMap<V, A> { self.z.make_map() }
    fn get_trie_ref(&self) -> TrieRef<'_, V, A> { self.z.get_trie_ref() }
}

impl<'a, 'path, V: Clone + Send + Sync + Unpin, A: Allocator + 'a> ZipperMoving for WriteZipperTracked<'a, 'path, V, A> {
    fn at_root(&self) -> bool { self.z.at_root() }
    fn reset(&mut self) { self.z.reset() }
    fn path(&self) -> &[u8] { self.z.path() }
    fn val_count(&self) -> usize { self.z.val_count() }
    fn descend_to<K: AsRef<[u8]>>(&mut self, k: K) { self.z.descend_to(k) }
    fn descend_to_byte(&mut self, k: u8) { self.z.descend_to_byte(k) }
    fn descend_indexed_byte(&mut self, child_idx: usize) -> bool { self.z.descend_indexed_byte(child_idx) }
    fn descend_first_byte(&mut self) -> bool { self.z.descend_first_byte() }
    fn descend_until(&mut self) -> bool { self.z.descend_until() }
    fn to_next_sibling_byte(&mut self) -> bool { self.z.to_next_sibling_byte() }
    fn to_prev_sibling_byte(&mut self) -> bool { self.z.to_prev_sibling_byte() }
    fn ascend(&mut self, steps: usize) -> bool { self.z.ascend(steps) }
    fn ascend_byte(&mut self) -> bool { self.z.ascend_byte() }
    fn ascend_until(&mut self) -> bool { self.z.ascend_until() }
    fn ascend_until_branch(&mut self) -> bool { self.z.ascend_until_branch() }
}

impl<'a, 'path, V: Clone + Send + Sync + Unpin, A: Allocator + 'a> zipper_priv::ZipperPriv for WriteZipperTracked<'a, 'path, V, A> {
    type V = V;
    type A = A;
    fn get_focus(&self) -> AbstractNodeRef<'_, Self::V, Self::A> { self.z.get_focus() }
    fn try_borrow_focus(&self) -> Option<&TrieNodeODRc<Self::V, Self::A>> { self.z.try_borrow_focus() }
}

impl<'a, 'path, V: Clone + Send + Sync + Unpin, A: Allocator + 'a> ZipperPathBuffer for WriteZipperTracked<'a, 'path, V, A> {
    unsafe fn origin_path_assert_len(&self, len: usize) -> &[u8] { unsafe{ self.z.origin_path_assert_len(len) } }
    fn prepare_buffers(&mut self) { self.z.prepare_buffers() }
    fn reserve_buffers(&mut self, path_len: usize, stack_depth: usize) { self.z.reserve_buffers(path_len, stack_depth) }
}

impl<'a, 'path, V: Clone + Send + Sync + Unpin, A: Allocator + 'a> ZipperAbsolutePath for WriteZipperTracked<'a, 'path, V, A> {
    fn origin_path(&self) -> &[u8] { self.z.origin_path() }
    fn root_prefix_path(&self) -> &[u8] { self.z.root_prefix_path() }
}

impl<'a, 'path, V: Clone + Send + Sync + Unpin, A: Allocator + 'a> WriteZipperTracked<'a, 'path, V, A> {
    //GOAT, this method currently isn't called
    // /// Creates a new zipper, with a path relative to a node
    // pub(crate) fn new_with_node_and_path(root_node: &'a mut TrieNodeODRc<V>, path: &'k [u8], tracker: ZipperTracker) -> Self {
    //     let core = WriteZipperCore::<'a, 'k, V>::new_with_node_and_path(root_node, path);
    //     Self { z: core, tracker, }
    // }
    //GOAT, currently unused
    // /// Creates a new zipper, with a path relative to a node, assuming the path is fully-contained within
    // /// the node
    // ///
    // /// NOTE: This method currently doesn't descend subnodes.  Use [Self::new_with_node_and_path] if you can't
    // /// guarantee the path is within the supplied node.
    // pub(crate) fn new_with_node_and_path_internal(root_node: &'a mut TrieNodeODRc<V>, root_val: Option<&'a mut Option<V>>, path: &'path [u8], root_key_start: usize, tracker: ZipperTracker<TrackingWrite>) -> Self {
    //     let core = WriteZipperCore::<'a, 'path, V>::new_with_node_and_path_internal(root_node, root_val, path, root_key_start);
    //     Self { z: core, _tracker: Some(tracker), }
    // }

    /// Consumes the `WriteZipperTracked`, and returns a [ReadZipperTracked] in its place
    ///
    /// The returned read zipper will have the same root and focus as the the consumed write zipper.
    pub fn into_read_zipper(mut self) -> ReadZipperTracked<'a, 'static, V, A> {
        let tracker = self._tracker.take().unwrap().into_reader();
        let root_node = self.z.focus_stack.take_root().unwrap();
        let root_path = &self.z.key.prefix_buf[..self.z.key.origin_path.len()];
        let descended_path = &self.z.key.prefix_buf[self.z.key.origin_path.len()..];
        let root_val = core::mem::take(&mut self.z.root_val);
        let root_val = root_val.and_then(|root_val| unsafe{ (&*root_val).as_ref() });

        let mut new_zipper = ReadZipperTracked::new_with_node_and_cloned_path_in(root_node, false, root_path, root_path.len(), self.z.key.root_key_start, root_val, self.z.alloc.clone(), Some(tracker));
        new_zipper.descend_to(descended_path);
        new_zipper
    }
}

impl<'a, V: Clone + Send + Sync + Unpin, A: Allocator> WriteZipperTracked<'a, 'static, V, A> {
    /// Same as [WriteZipperUntracked::new_with_node_and_path_internal], but clones the path so the `'path` lifetime isn't needed
    pub(crate) fn new_with_node_and_cloned_path_internal_in(root_node: &'a mut TrieNodeODRc<V, A>, root_val: Option<&'a mut Option<V>>, path: &[u8], root_key_start: usize, alloc: A, tracker: Option<ZipperTracker<TrackingWrite>>) -> Self {
        let core = WriteZipperCore::<'a, 'static, V, A>::new_with_node_and_cloned_path_internal_in(root_node, root_val, path, root_key_start, alloc);
        Self { z: core, _tracker: tracker }
    }
}

impl<'a, 'path, V: Clone + Send + Sync + Unpin, A: Allocator + 'a> ZipperWriting<V, A> for WriteZipperTracked<'a, 'path, V, A> {
    type ZipperHead<'z> = ZipperHead<'z, 'a, V, A> where Self: 'z;
    fn get_val_mut(&mut self) -> Option<&mut V> { self.z.get_val_mut() }
    fn get_val_or_set_mut(&mut self, default: V) -> &mut V { self.z.get_val_or_set_mut(default) }
    fn get_val_or_set_mut_with<F>(&mut self, func: F) -> &mut V where F: FnOnce() -> V { self.z.get_val_or_set_mut_with(func) }
    fn set_val(&mut self, val: V) -> Option<V> { self.z.set_val(val) }
    fn remove_val(&mut self, prune: bool) -> Option<V> { self.z.remove_val(prune) }
    fn zipper_head<'z>(&'z mut self) -> Self::ZipperHead<'z> { self.z.zipper_head() }
    fn graft<Z: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &Z) { self.z.graft(read_zipper) }
    fn graft_map(&mut self, map: PathMap<V, A>) { self.z.graft_map(map) }
    fn graft_child_maps<I: IntoIterator<Item=PathMap<V, A>>>(&mut self, child_mask: ByteMask, maps: I, remove_unset: bool) { self.z.graft_child_maps(child_mask, maps, remove_unset) }
    fn join_into<Z: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &Z) -> AlgebraicStatus where V: Lattice { self.z.join_into(read_zipper) }
    fn join_map_into(&mut self, map: PathMap<V, A>) -> AlgebraicStatus where V: Lattice { self.z.join_map_into(map) }
    fn join_into_take<Z: ZipperInfallibleSubtries<V, A> + ZipperWriting<V, A>>(&mut self, src_zipper: &mut Z, prune: bool) -> AlgebraicStatus where V: Lattice { self.z.join_into_take(src_zipper, prune) }
    fn join_k_path_into(&mut self, byte_cnt: usize, prune: bool) -> bool where V: Lattice { self.z.join_k_path_into(byte_cnt, prune) }
    fn meet_k_path_into(&mut self, byte_cnt: usize, prune: bool) -> bool where V: Lattice { self.z.meet_k_path_into(byte_cnt, prune) }
    fn insert_prefix<K: AsRef<[u8]>>(&mut self, prefix: K) -> bool { self.z.insert_prefix(prefix) }
    fn remove_prefix(&mut self, n: usize) -> bool { self.z.remove_prefix(n) }
    fn meet_into<Z: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &Z, prune: bool) -> AlgebraicStatus where V: Lattice { self.z.meet_into(read_zipper, prune) }
    fn meet_2<ZA: ZipperInfallibleSubtries<V, A>, ZB: ZipperInfallibleSubtries<V, A>>(&mut self, rz_a: &ZA, rz_b: &ZB) -> AlgebraicStatus where V: Lattice { self.z.meet_2(rz_a, rz_b) }
    fn subtract_into<Z: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &Z, prune: bool) -> AlgebraicStatus where V: DistributiveLattice { self.z.subtract_into(read_zipper, prune) }
    fn restrict<Z: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &Z) -> AlgebraicStatus { self.z.restrict(read_zipper) }
    fn restricting<Z: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &Z) -> bool { self.z.restricting(read_zipper) }
    fn take_map(&mut self, prune: bool) -> Option<PathMap<V, A>> { self.z.take_map(prune) }
    fn remove_branches(&mut self, prune: bool) -> bool { self.z.remove_branches(prune) }
    fn remove_unmasked_branches(&mut self, mask: ByteMask, prune: bool) { self.z.remove_unmasked_branches(mask, prune) }
    fn create_path(&mut self) -> bool { self.z.create_path() }
    fn prune_path(&mut self) -> usize { self.z.prune_path() }
    fn prune_ascend(&mut self) -> usize { self.z.prune_ascend() }
}

impl<'a, 'path, V: Clone + Send + Sync + Unpin, A: Allocator + 'a> WriteZipperPriv<V, A> for WriteZipperTracked<'a, 'path, V, A> {
    fn take_focus(&mut self, prune: bool) -> Option<TrieNodeODRc<V, A>> { self.z.take_focus(prune) }
    fn take_root_prefix_path(&mut self) -> Vec<u8> { self.z.take_root_prefix_path() }
    fn alloc(&self) -> A { self.z.alloc.clone() }
}

super::zipper::impl_zipper_debug!(
    impl<'a, 'path, V: Clone + Send + Sync + Unpin + 'a, A: Allocator + 'a> core::fmt::Debug for WriteZipperTracked<'a, 'path, V, A>
);

// ***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---
// WriteZipperUntracked
// ***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---

/// A [write zipper](ZipperWriting) type for editing and adding paths and values in the trie, used when it
/// is possible to statically guarantee non-interference between zippers
pub struct WriteZipperUntracked<'a, 'k, V: Clone + Send + Sync, A: Allocator = GlobalAlloc> {
    z: WriteZipperCore<'a, 'k, V, A>,
}

impl<'a, V: Clone + Send + Sync + Unpin, A: Allocator + 'a> Zipper for WriteZipperUntracked<'a, '_, V, A> {
    fn path_exists(&self) -> bool { self.z.path_exists() }
    fn is_val(&self) -> bool { self.z.is_val() }
    fn child_count(&self) -> usize { self.z.child_count() }
    fn child_mask(&self) -> ByteMask { self.z.child_mask() }
}

impl<'a, V: Clone + Send + Sync + Unpin, A: Allocator + 'a> ZipperValues<V> for WriteZipperUntracked<'a, '_, V, A> {
    fn val(&self) -> Option<&V> { self.z.val() }
}

impl<'trie, V: Clone + Send + Sync + Unpin, A: Allocator + 'trie> ZipperForking<V> for WriteZipperUntracked<'trie, '_, V, A> {
    type ReadZipperT<'a> = ReadZipperUntracked<'a, 'a, V, A> where Self: 'a;
    fn fork_read_zipper<'a>(&'a self) -> Self::ReadZipperT<'a> {
        let rz_core = self.z.fork_read_zipper();
        Self::ReadZipperT::new_forked_with_inner_zipper(rz_core)
    }
}

impl<'a, V: Clone + Send + Sync + Unpin, A: Allocator + 'a> ZipperSubtries<V, A> for WriteZipperUntracked<'a, '_, V, A> {
    fn native_subtries(&self) -> bool { true }
    fn try_make_map(&self) -> Option<PathMap<V, A>> { Some(self.z.make_map()) }
    fn trie_ref(&self) -> Option<TrieRef<'_, V, A>> { Some(self.z.get_trie_ref()) }
    fn alloc(&self) -> A { self.z.alloc.clone() }
}

impl<'a, V: Clone + Send + Sync + Unpin, A: Allocator + 'a> ZipperInfallibleSubtries<V, A> for WriteZipperUntracked<'a, '_, V, A> {
    fn make_map(&self) -> PathMap<V, A> { self.z.make_map() }
    fn get_trie_ref(&self) -> TrieRef<'_, V, A> { self.z.get_trie_ref() }
}

impl<'a, 'path, V: Clone + Send + Sync + Unpin, A: Allocator + 'a> ZipperMoving for WriteZipperUntracked<'a, 'path, V, A> {
    fn at_root(&self) -> bool { self.z.at_root() }
    fn reset(&mut self) { self.z.reset() }
    fn path(&self) -> &[u8] { self.z.path() }
    fn val_count(&self) -> usize { self.z.val_count() }
    fn descend_to<K: AsRef<[u8]>>(&mut self, k: K) { self.z.descend_to(k) }
    fn descend_to_byte(&mut self, k: u8) { self.z.descend_to_byte(k) }
    fn descend_indexed_byte(&mut self, child_idx: usize) -> bool { self.z.descend_indexed_byte(child_idx) }
    fn descend_first_byte(&mut self) -> bool { self.z.descend_first_byte() }
    fn descend_until(&mut self) -> bool { self.z.descend_until() }
    fn to_next_sibling_byte(&mut self) -> bool { self.z.to_next_sibling_byte() }
    fn to_prev_sibling_byte(&mut self) -> bool { self.z.to_prev_sibling_byte() }
    fn ascend(&mut self, steps: usize) -> bool { self.z.ascend(steps) }
    fn ascend_byte(&mut self) -> bool { self.z.ascend_byte() }
    fn ascend_until(&mut self) -> bool { self.z.ascend_until() }
    fn ascend_until_branch(&mut self) -> bool { self.z.ascend_until_branch() }
}

impl<'a, 'k, V: Clone + Send + Sync + Unpin, A: Allocator + 'a> zipper_priv::ZipperPriv for WriteZipperUntracked<'a, 'k, V, A> {
    type V = V;
    type A = A;
    fn get_focus(&self) -> AbstractNodeRef<'_, Self::V, Self::A> { self.z.get_focus() }
    fn try_borrow_focus(&self) -> Option<&TrieNodeODRc<Self::V, Self::A>> { self.z.try_borrow_focus() }
}

impl<'a, 'path, V: Clone + Send + Sync + Unpin, A: Allocator + 'a> ZipperPathBuffer for WriteZipperUntracked<'a, 'path, V, A> {
    unsafe fn origin_path_assert_len(&self, len: usize) -> &[u8] { unsafe{ self.z.origin_path_assert_len(len) } }
    fn prepare_buffers(&mut self) { self.z.prepare_buffers() }
    fn reserve_buffers(&mut self, path_len: usize, stack_depth: usize) { self.z.reserve_buffers(path_len, stack_depth) }
}

impl<'a, 'path, V: Clone + Send + Sync + Unpin, A: Allocator + 'a> ZipperAbsolutePath for WriteZipperUntracked<'a, 'path, V, A> {
    fn origin_path(&self) -> &[u8] { self.z.origin_path() }
    fn root_prefix_path(&self) -> &[u8] { self.z.root_prefix_path() }
}

impl <'a, V: Clone + Send + Sync + Unpin, A: Allocator> WriteZipperUntracked<'a, 'static, V, A> {
    //GOAT, currently unneeded, but we may add the method back that requires this
    // /// See [WriteZipperUntracked::new_with_node_and_path_internal]
    // pub(crate) fn new_with_node_and_cloned_path_internal_in(root_node: &'a mut TrieNodeODRc<V, A>, root_val: Option<&'a mut Option<V>>, path: &[u8], root_key_start: usize, alloc: A) -> Self {
    //     let core = WriteZipperCore::<'a, 'static, V, A>::new_with_node_and_cloned_path_internal_in(root_node, root_val, path, root_key_start, alloc);
    //     Self { z: core }
    // }
}

impl <'a, 'path, V: Clone + Send + Sync + Unpin, A: Allocator + 'a> WriteZipperUntracked<'a, 'path, V, A> {
    /// Creates a new zipper, with a path relative to a node
    pub(crate) fn new_with_node_and_path_in(root_node: &'a mut TrieNodeODRc<V, A>, root_val: Option<&'a mut Option<V>>, path: &'path [u8], root_prefix_len: usize, root_key_start: usize, alloc: A) -> Self {
        let core = WriteZipperCore::<'a, 'path, V, A>::new_with_node_and_path_in(root_node, root_val, path, root_prefix_len, root_key_start, alloc);
        Self { z: core }
    }
    /// Creates a new zipper, with a path relative to a node, assuming the path is fully-contained within
    /// the node
    ///
    /// NOTE: This method doesn't descend subnodes.  Use [WriteZipperUntracked::new_with_node_and_path] if you can't
    /// guarantee the path is within the supplied node.
    pub(crate) fn new_with_node_and_path_internal_in(root_node: &'a mut TrieNodeODRc<V, A>, root_val: Option<&'a mut Option<V>>, path: &'path [u8], root_key_start: usize, alloc: A) -> Self {
        let core = WriteZipperCore::<'a, 'path, V, A>::new_with_node_and_path_internal_in(root_node, root_val, path, root_key_start, alloc);
        Self { z: core }
    }
    /// Consumes the `WriteZipperUntracked`, and returns a [ReadZipperUntracked] in its place
    ///
    /// The returned read zipper will have the same root and focus as the the consumed write zipper.
    pub fn into_read_zipper(mut self) -> ReadZipperUntracked<'a, 'static, V, A> {
        let root_node = self.z.focus_stack.take_root().unwrap();
        let root_path = &self.z.key.prefix_buf[..self.z.key.origin_path.len()];
        let descended_path = &self.z.key.prefix_buf[self.z.key.origin_path.len()..];
        let root_val = core::mem::take(&mut self.z.root_val);
        let root_val = root_val.and_then(|root_val| unsafe{ (&*root_val).as_ref() });

        let mut new_zipper = ReadZipperUntracked::new_with_node_and_cloned_path_in(root_node, root_path, root_path.len(), self.z.key.root_key_start, root_val, self.z.alloc.clone());
        new_zipper.descend_to(descended_path);
        new_zipper
    }
    /// Internal method to access `WriteZipperCore` inside `WriteZipperUntracked`
    #[inline]
    pub(crate) fn core(&mut self) -> &mut WriteZipperCore<'a, 'path, V, A> {
        &mut self.z
    }
}

impl<'a, 'path, V: Clone + Send + Sync + Unpin, A: Allocator + 'a> ZipperWriting<V, A> for WriteZipperUntracked<'a, 'path, V, A> {
    type ZipperHead<'z> = ZipperHead<'z, 'a, V, A> where Self: 'z;
    fn get_val_mut(&mut self) -> Option<&mut V> { self.z.get_val_mut() }
    fn get_val_or_set_mut(&mut self, default: V) -> &mut V { self.z.get_val_or_set_mut(default) }
    fn get_val_or_set_mut_with<F>(&mut self, func: F) -> &mut V where F: FnOnce() -> V { self.z.get_val_or_set_mut_with(func) }
    fn set_val(&mut self, val: V) -> Option<V> { self.z.set_val(val) }
    fn remove_val(&mut self, prune: bool) -> Option<V> { self.z.remove_val(prune) }
    fn zipper_head<'z>(&'z mut self) -> Self::ZipperHead<'z> { self.z.zipper_head() }
    fn graft<Z: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &Z) { self.z.graft(read_zipper) }
    fn graft_map(&mut self, map: PathMap<V, A>) { self.z.graft_map(map) }
    fn graft_child_maps<I: IntoIterator<Item=PathMap<V, A>>>(&mut self, child_mask: ByteMask, maps: I, remove_unset: bool) { self.z.graft_child_maps(child_mask, maps, remove_unset) }
    fn join_into<Z: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &Z) -> AlgebraicStatus where V: Lattice { self.z.join_into(read_zipper) }
    fn join_map_into(&mut self, map: PathMap<V, A>) -> AlgebraicStatus where V: Lattice { self.z.join_map_into(map) }
    fn join_into_take<Z: ZipperInfallibleSubtries<V, A> + ZipperWriting<V, A>>(&mut self, src_zipper: &mut Z, prune: bool) -> AlgebraicStatus where V: Lattice { self.z.join_into_take(src_zipper, prune) }
    fn join_k_path_into(&mut self, byte_cnt: usize, prune: bool) -> bool where V: Lattice { self.z.join_k_path_into(byte_cnt, prune) }
    fn meet_k_path_into(&mut self, byte_cnt: usize, prune: bool) -> bool where V: Lattice { self.z.meet_k_path_into(byte_cnt, prune) }
    fn insert_prefix<K: AsRef<[u8]>>(&mut self, prefix: K) -> bool { self.z.insert_prefix(prefix) }
    fn remove_prefix(&mut self, n: usize) -> bool { self.z.remove_prefix(n) }
    fn meet_into<Z: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &Z, prune: bool) -> AlgebraicStatus where V: Lattice { self.z.meet_into(read_zipper, prune) }
    fn meet_2<ZA: ZipperInfallibleSubtries<V, A>, ZB: ZipperInfallibleSubtries<V, A>>(&mut self, rz_a: &ZA, rz_b: &ZB) -> AlgebraicStatus where V: Lattice { self.z.meet_2(rz_a, rz_b) }
    fn subtract_into<Z: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &Z, prune: bool) -> AlgebraicStatus where V: DistributiveLattice { self.z.subtract_into(read_zipper, prune) }
    fn restrict<Z: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &Z) -> AlgebraicStatus { self.z.restrict(read_zipper) }
    fn restricting<Z: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &Z) -> bool { self.z.restricting(read_zipper) }
    fn take_map(&mut self, prune: bool) -> Option<PathMap<V, A>> { self.z.take_map(prune) }
    fn remove_branches(&mut self, prune: bool) -> bool { self.z.remove_branches(prune) }
    fn remove_unmasked_branches(&mut self, mask: ByteMask, prune: bool) { self.z.remove_unmasked_branches(mask, prune) }
    fn create_path(&mut self) -> bool { self.z.create_path() }
    fn prune_path(&mut self) -> usize { self.z.prune_path() }
    fn prune_ascend(&mut self) -> usize { self.z.prune_ascend() }
}

impl<'a, 'path, V: Clone + Send + Sync + Unpin, A: Allocator + 'a> WriteZipperPriv<V, A> for WriteZipperUntracked<'a, 'path, V, A> {
    fn take_focus(&mut self, prune: bool) -> Option<TrieNodeODRc<V, A>> { self.z.take_focus(prune) }
    fn take_root_prefix_path(&mut self) -> Vec<u8> { self.z.take_root_prefix_path() }
    fn alloc(&self) -> A { self.z.alloc.clone() }
}

super::zipper::impl_zipper_debug!(
    impl<'a, 'path, V: Clone + Send + Sync + Unpin + 'a, A: Allocator + 'a> core::fmt::Debug for WriteZipperUntracked<'a, 'path, V, A>
);

// ***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---
// WriteZipperOwned
// ***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---

/// A [Zipper] for editing a trie for situations where a lifetime is inconvenient
///
/// I am on the fence about whether this object has much value as part of the public API.  The only benefit
/// I see is that it saves the caller from creating a new temporary [write zipper](ZipperWriting) and re-traversing to the
/// zipper root each time, which could be a perf gain.  On the other hand, this object has higher overhead
/// than the ordinary borrowed `WriteZipper`, both at creation time as well as during use.
pub struct WriteZipperOwned<V: Clone + Send + Sync + 'static, A: Allocator + 'static = GlobalAlloc> {
    map: MaybeDangling<Box<PathMap<V, A>>>,
    z: WriteZipperCore<'static, 'static, V, A>,
}

impl<V: 'static + Clone + Send + Sync + Unpin, A: Allocator> Clone for WriteZipperOwned<V, A> {
    fn clone(&self) -> Self {
        let new_map = (**self.map).clone();
        Self::new_with_map(new_map, self.root_prefix_path())
    }
}

impl<V: Clone + Send + Sync + Unpin, A: Allocator> Zipper for WriteZipperOwned<V, A> { zipper_impl_lens!(Zipper self => self.z); }
impl<V: Clone + Send + Sync + Unpin, A: Allocator> ZipperValues<V> for WriteZipperOwned<V, A> { zipper_impl_lens!(ZipperValues self => self.z); }
impl<V: Clone + Send + Sync + Unpin, A: Allocator> ZipperInfallibleSubtries<V, A> for WriteZipperOwned<V, A> { zipper_impl_lens!(ZipperInfallibleSubtries self => self.z); }
impl<V: Clone + Send + Sync + Unpin, A: Allocator> ZipperMoving for WriteZipperOwned<V, A> { zipper_impl_lens!(ZipperMoving self => self.z); }
impl<V: Clone + Send + Sync + Unpin, A: Allocator> ZipperPriv for WriteZipperOwned<V, A> { zipper_impl_lens!(ZipperPriv self => self.z); }
impl<V: Clone + Send + Sync + Unpin, A: Allocator> ZipperPathBuffer for WriteZipperOwned<V, A> { zipper_impl_lens!(ZipperPathBuffer self => self.z); }
impl<V: Clone + Send + Sync + Unpin, A: Allocator> ZipperAbsolutePath for WriteZipperOwned<V, A> { zipper_impl_lens!(ZipperAbsolutePath self => self.z); }


impl<V: Clone + Send + Sync + Unpin, A: Allocator> ZipperForking<V> for WriteZipperOwned<V, A> {
    type ReadZipperT<'a> = ReadZipperUntracked<'a, 'a, V, A> where Self: 'a;
    fn fork_read_zipper<'a>(&'a self) -> Self::ReadZipperT<'a> {
        let rz_core = self.z.fork_read_zipper();
        Self::ReadZipperT::new_forked_with_inner_zipper(rz_core)
    }
}

impl<V: Clone + Send + Sync + Unpin, A: Allocator> ZipperSubtries<V, A> for WriteZipperOwned<V, A> {
    fn native_subtries(&self) -> bool { true } // Why not self.z.native_subtries ?
    fn try_make_map(&self) -> Option<PathMap<V, A>> { Some(self.z.make_map()) }
    fn trie_ref(&self) -> Option<TrieRef<'_, V, A>> { Some(self.z.get_trie_ref()) }
    fn alloc(&self) -> A { self.z.alloc.clone() }
}

impl <V: Clone + Send + Sync + Unpin> WriteZipperOwned<V> {
    /// Create a brand new `WriteZipperOwned` containing no paths nor values
    pub fn new() -> Self {
        PathMap::new().into_write_zipper(&[])
    }
}

impl <V: Clone + Send + Sync + Unpin, A: Allocator> WriteZipperOwned<V, A> {
    /// Create a brand new `WriteZipperOwned` containing no paths nor values
    pub fn new_in(alloc: A) -> Self {
        PathMap::new_in(alloc).into_write_zipper(&[])
    }
    /// Creates a new `WriteZipperOwned`, consuming a `map`
    pub(crate) fn new_with_map<K: AsRef<[u8]>>(map: PathMap<V, A>, path: K) -> Self {
        let path = path.as_ref();
        map.ensure_root();
        let alloc = map.alloc.clone();
        let map = MaybeDangling::new(Box::new(map));
        let root_ref = unsafe{ &mut *map.root.get() }.as_mut().unwrap();
        let root_val = match path.len() == 0 {
            true => Some(unsafe{ &mut *map.root_val.get() }),
            false => None
        };
        let core = WriteZipperCore::new_with_node_and_cloned_path_in(root_ref, root_val, &*path, path.len(), 0, alloc);
        Self { map, z: core }
    }
    /// Consumes the zipper and returns a map contained within the zipper
    pub fn into_map(self) -> PathMap<V, A> {
        drop(self.z);
        let map = MaybeDangling::into_inner(self.map);
        *map
    }
    /// Consumes the `WriteZipperOwned`, and returns a [ReadZipperOwned] in its place
    ///
    /// The returned read zipper will have the same root and focus as the the consumed write zipper.
    pub fn into_read_zipper(mut self) -> ReadZipperOwned<V, A> {
        let descended_path = &self.z.key.prefix_buf[self.z.key.origin_path.len()..].to_vec();
        let root_prefix_len = self.root_prefix_path().len();
        let path = self.take_root_prefix_path();
        let map = self.into_map();
        let mut new_zipper = ReadZipperOwned::new_with_map(map, &path[..root_prefix_len]);
        new_zipper.descend_to(descended_path);
        new_zipper
    }
    /// Internal method to access `WriteZipperCore` inside `WriteZipperOwned`
    pub(crate) fn core(&mut self) -> &mut WriteZipperCore<'static, 'static, V, A> {
        &mut self.z
    }
}

impl<V: Clone + Send + Sync + Unpin, A: Allocator> ZipperWriting<V, A> for WriteZipperOwned<V, A> {
    type ZipperHead<'z> = ZipperHead<'z, 'static, V, A> where Self: 'z;
    fn get_val_mut(&mut self) -> Option<&mut V> { self.z.get_val_mut() }
    fn get_val_or_set_mut(&mut self, default: V) -> &mut V { self.z.get_val_or_set_mut(default) }
    fn get_val_or_set_mut_with<F>(&mut self, func: F) -> &mut V where F: FnOnce() -> V { self.z.get_val_or_set_mut_with(func) }
    fn set_val(&mut self, val: V) -> Option<V> { self.z.set_val(val) }
    fn remove_val(&mut self, prune: bool) -> Option<V> { self.z.remove_val(prune) }
    fn zipper_head<'z>(&'z mut self) -> Self::ZipperHead<'z> { self.z.zipper_head() }
    fn graft<Z: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &Z) { self.z.graft(read_zipper) }
    fn graft_map(&mut self, map: PathMap<V, A>) { self.z.graft_map(map) }
    fn graft_child_maps<I: IntoIterator<Item=PathMap<V, A>>>(&mut self, child_mask: ByteMask, maps: I, remove_unset: bool) { self.z.graft_child_maps(child_mask, maps, remove_unset) }
    fn join_into<Z: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &Z) -> AlgebraicStatus where V: Lattice { self.z.join_into(read_zipper) }
    fn join_map_into(&mut self, map: PathMap<V, A>) -> AlgebraicStatus where V: Lattice { self.z.join_map_into(map) }
    fn join_into_take<Z: ZipperInfallibleSubtries<V, A> + ZipperWriting<V, A>>(&mut self, src_zipper: &mut Z, prune: bool) -> AlgebraicStatus where V: Lattice { self.z.join_into_take(src_zipper, prune) }
    fn join_k_path_into(&mut self, byte_cnt: usize, prune: bool) -> bool where V: Lattice { self.z.join_k_path_into(byte_cnt, prune) }
    fn meet_k_path_into(&mut self, byte_cnt: usize, prune: bool) -> bool where V: Lattice { self.z.meet_k_path_into(byte_cnt, prune) }
    fn insert_prefix<K: AsRef<[u8]>>(&mut self, prefix: K) -> bool { self.z.insert_prefix(prefix) }
    fn remove_prefix(&mut self, n: usize) -> bool { self.z.remove_prefix(n) }
    fn meet_into<Z: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &Z, prune: bool) -> AlgebraicStatus where V: Lattice { self.z.meet_into(read_zipper, prune) }
    fn meet_2<ZA: ZipperInfallibleSubtries<V, A>, ZB: ZipperInfallibleSubtries<V, A>>(&mut self, rz_a: &ZA, rz_b: &ZB) -> AlgebraicStatus where V: Lattice { self.z.meet_2(rz_a, rz_b) }
    fn subtract_into<Z: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &Z, prune: bool) -> AlgebraicStatus where V: DistributiveLattice { self.z.subtract_into(read_zipper, prune) }
    fn restrict<Z: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &Z) -> AlgebraicStatus { self.z.restrict(read_zipper) }
    fn restricting<Z: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &Z) -> bool { self.z.restricting(read_zipper) }
    fn take_map(&mut self, prune: bool) -> Option<PathMap<V, A>> { self.z.take_map(prune) }
    fn remove_branches(&mut self, prune: bool) -> bool { self.z.remove_branches(prune) }
    fn remove_unmasked_branches(&mut self, mask: ByteMask, prune: bool) { self.z.remove_unmasked_branches(mask, prune) }
    fn create_path(&mut self) -> bool { self.z.create_path() }
    fn prune_path(&mut self) -> usize { self.z.prune_path() }
    fn prune_ascend(&mut self) -> usize { self.z.prune_ascend() }
}

impl<V: Clone + Send + Sync + Unpin, A: Allocator> WriteZipperPriv<V, A> for WriteZipperOwned<V, A> {
    fn take_focus(&mut self, prune: bool) -> Option<TrieNodeODRc<V, A>> { self.z.take_focus(prune) }
    fn take_root_prefix_path(&mut self) -> Vec<u8> { self.z.take_root_prefix_path() }
    fn alloc(&self) -> A { self.z.alloc.clone() }
}

impl<V: Clone + Send + Sync + Unpin, A: Allocator> ZipperIteration for WriteZipperOwned<V, A> { } //Use the default impl for all methods

impl<V: Clone + Send + Sync + Unpin, A: Allocator> std::iter::IntoIterator for WriteZipperOwned<V, A> {
    type Item = (Vec<u8>, V);
    type IntoIter = OwnedZipperIter<V, A>;

    fn into_iter(self) -> Self::IntoIter {
        OwnedZipperIter {
            started: false,
            zipper: self,
        }
    }
}

/// An iterator for depth-first traversal of a [ZipperWriting] type or a [PathMap]
///
/// NOTE: This is a convenience to allow access to syntactic sugar like `for` loops, [collect](std::iter::Iterator::collect),
///  etc.  It will always be faster to use the zipper itself for iteration and traversal.
pub struct OwnedZipperIter<V: Clone + Send + Sync + 'static, A: Allocator + 'static = GlobalAlloc>{
    started: bool,
    zipper: WriteZipperOwned<V, A>,
}

impl<V: Clone + Send + Sync + Unpin + 'static, A: Allocator + 'static> Iterator for OwnedZipperIter<V, A> {
    type Item = (Vec<u8>, V);

    fn next(&mut self) -> Option<(Vec<u8>, V)> {
        if !self.started {
            self.started = true;
            if let Some(val) = self.zipper.remove_val(true) {
                return Some((self.zipper.path().to_vec(), val))
            }
        }
        if self.zipper.to_next_val() {
            match self.zipper.remove_val(true) {
                Some(val) => return Some((self.zipper.path().to_vec(), val)),
                None => None
            }
        } else {
            None
        }
    }
}

super::impl_name_only_debug!(
    impl<V: Clone + Send + Sync + Unpin + 'static, A: Allocator + 'static> core::fmt::Debug for OwnedZipperIter<V, A>
);

super::zipper::impl_zipper_debug!(
    impl<V: Clone + Send + Sync + Unpin, A: Allocator> core::fmt::Debug for WriteZipperOwned<V, A>
);

// ***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---
// WriteZipperCore (the actual implementation)
// ***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---

/// The core implementation of WriteZipper
pub(crate) struct WriteZipperCore<'a, 'k, V: Clone + Send + Sync, A: Allocator> {
    pub(crate) key: KeyFields<'k>,

    pub(crate) root_val: Option<*mut Option<V>>,

    /// The stack of node references.  We need a "rooted" Vec in case we need to upgrade the node at the root of the zipper
    pub(crate) focus_stack: MutNodeStack<'a, V, A>,
    pub(crate) alloc: A,
}

unsafe impl<V: Clone + Send + Sync, A: Allocator> Send for WriteZipperCore<'_, '_, V, A> {}
unsafe impl<V: Clone + Send + Sync, A: Allocator> Sync for WriteZipperCore<'_, '_, V, A> {}

/// The part of the zipper that contains the path and key-related fields.  So it can be borrowed separately
///
/// For a more complete description of the meaning of the fields, see [read_zipper_core::ReadZipperCore::new_with_node_and_path]
//
//TODO: We may want to unify this object with the ReadZipper's fields now that they do exactly the same thing, but the ReadZipper
// stores a single Vec for nodes and path indices, where the WriteZipper has them separate.
pub(crate) struct KeyFields<'path> {
    /// The zipper's root prefix path
    pub(crate) origin_path: SliceOrLen<'path>,
    /// The index into `origin_path` of the start of the root node's key
    pub(crate) root_key_start: usize,
    /// Stores the entire path, including the bytes from origin_path
    pub(crate) prefix_buf: Vec<u8>,
    /// Stores the lengths for each successive node's key
    pub(crate) prefix_idx: Vec<usize>,
}

impl<'trie, V: Clone + Send + Sync + Unpin, A: Allocator + 'trie> Zipper for WriteZipperCore<'trie, '_, V, A> {
    fn path_exists(&self) -> bool {
        let key = self.key.node_key();
        if key.len() > 0 {
            self.focus_stack.top().unwrap().node_contains_partial_key(key)
        } else {
            true
        }
    }
    fn is_val(&self) -> bool {
        let key = self.key.node_key();
        if key.len() == 0 {
            debug_assert!(self.at_root());
            match self.root_val {
                Some(root_ptr) => unsafe{ &*root_ptr }.is_some(),
                None => false
            }
        } else {
            self.focus_stack.top().unwrap().node_contains_val(key)
        }
    }
    fn child_count(&self) -> usize {
        match self.focus_stack.top() {
            Some(focus_node) => {
                node_count_branches_recursive(focus_node, self.key.node_key())
            },
            None => 0
        }
    }
    fn child_mask(&self) -> ByteMask {
        let focus_node = match self.focus_stack.top() {
            Some(focus_node) => focus_node,
            None => return ByteMask::EMPTY
        };
        let node_key = self.key.node_key();
        if node_key.len() == 0 {
            return focus_node.node_branches_mask(b"")
        }
        match focus_node.node_get_child(node_key) {
            Some((consumed_bytes, child_node)) => {
                let child_node = child_node.as_tagged();
                if node_key.len() >= consumed_bytes {
                    child_node.node_branches_mask(&node_key[consumed_bytes..])
                } else {
                    ByteMask::EMPTY
                }
            },
            None => focus_node.node_branches_mask(node_key)
        }
    }
}

impl<'trie, V: Clone + Send + Sync + Unpin, A: Allocator + 'trie> ZipperForking<V> for WriteZipperCore<'trie, '_, V, A> {
    type ReadZipperT<'a> = super::zipper::read_zipper_core::ReadZipperCore<'a, 'a, V, A> where Self: 'a;
    fn fork_read_zipper<'a>(&'a self) -> Self::ReadZipperT<'a> {
        let new_root_val = self.val();
        let path = self.origin_path();

        read_zipper_core::ReadZipperCore::new_with_node_and_path_in(self.focus_parent(), false, path, path.len(), self.key.node_key_start(), new_root_val, self.alloc.clone())
    }
}

impl<'a, V: Clone + Send + Sync + Unpin, A: Allocator + 'a> WriteZipperCore<'a, '_, V, A> {
    fn make_map(&self) -> PathMap<V, A> {
        #[cfg(not(feature = "graft_root_vals"))]
        let root_val = None;
        #[cfg(feature = "graft_root_vals")]
        let root_val = self.val().cloned();

        let root_node = self.get_focus().into_option();
        PathMap::new_with_root_in(root_node, root_val, self.alloc.clone())
    }
    fn get_trie_ref(&self) -> TrieRef<'_, V, A> {
        #[cfg(not(feature = "graft_root_vals"))]
        let root_val = None;
        #[cfg(feature = "graft_root_vals")]
        let root_val = self.val().cloned();

        let root_node = self.get_focus().into_option();
        TrieRefOwned::new_with_node_and_val_in(root_node, root_val, self.alloc.clone()).into()
    }
}

impl<'a, 'path, V: Clone + Send + Sync + Unpin, A: Allocator + 'a> ZipperMoving for WriteZipperCore<'a, 'path, V, A> {
    #[inline]
    fn at_root(&self) -> bool {
        self.key.prefix_buf.len() <= self.key.origin_path.len()
    }

    fn reset(&mut self) {
        self.focus_stack.to_root();
        self.key.prefix_buf.truncate(self.key.origin_path.len());
        self.key.prefix_idx.clear();
    }

    fn path(&self) -> &[u8] {
        if self.key.prefix_buf.len() > 0 {
            &self.key.prefix_buf[self.key.origin_path.len()..]
        } else {
            &[]
        }
    }

    fn val_count(&self) -> usize {
        let root_val = self.is_val() as usize;
        let focus = self.get_focus();
        if focus.is_none() {
            root_val
        } else {
            val_count_below_root(focus.as_tagged()) + root_val
        }
    }
    fn descend_to<K: AsRef<[u8]>>(&mut self, k: K) {
        let key = k.as_ref();
        self.key.prepare_buffers();
        self.key.prefix_buf.extend(key);
        self.descend_to_internal();
    }

    fn ascend(&mut self, mut steps: usize) -> bool {
        loop {
            if self.key.node_key().len() == 0 {
                self.ascend_across_nodes();
            }
            if steps == 0 {
                return true
            }
            if self.at_root() {
                return false
            }
            debug_assert!(self.key.node_key().len() > 0);
            let cur_jump = steps.min(self.key.excess_key_len());
            self.key.prefix_buf.truncate(self.key.prefix_buf.len() - cur_jump);
            steps -= cur_jump;
        }
    }

    fn ascend_until(&mut self) -> bool {
        if self.at_root() {
            return false;
        }
        loop {
            self.ascend_within_node();
            if self.at_root() {
                return true;
            }
            if self.key.node_key().len() == 0 {
                self.ascend_across_nodes();
            }
            if self.child_count() > 1 || self.is_val() {
                break;
            }
        }
        debug_assert!(self.key.node_key().len() > 0); //We should never finish with a zero-length node-key
        true
    }

    fn ascend_until_branch(&mut self) -> bool {
        if self.at_root() {
            return false;
        }
        loop {
            self.ascend_within_node();
            if self.at_root() {
                return true;
            }
            if self.key.node_key().len() == 0 {
                self.ascend_across_nodes();
            }
            if self.child_count() > 1 {
                break;
            }
        }
        debug_assert!(self.key.node_key().len() > 0); //We should never finish with a zero-length node-key
        true
    }
}

impl<'a, 'k, V: Clone + Send + Sync + Unpin, A: Allocator + 'a> zipper_priv::ZipperPriv for WriteZipperCore<'a, 'k, V, A> {
    type V = V;
    type A = A;
    fn get_focus(&self) -> AbstractNodeRef<'_, Self::V, Self::A> {
        let node_key = self.key.node_key();
        if node_key.len() > 0 {
            self.focus_stack.top().unwrap().get_node_at_key(node_key)
        } else {
            debug_assert!(self.at_root());
            unsafe{ AbstractNodeRef::BorrowedRc(self.focus_stack.root_unchecked()) }
        }
    }
    fn try_borrow_focus(&self) -> Option<&TrieNodeODRc<Self::V, Self::A>> {
        let node_key = self.key.node_key();
        if node_key.len() == 0 {
            debug_assert!(self.at_root());
            debug_assert_eq!(self.focus_stack.depth(), 1);
            Some(unsafe{ self.focus_stack.root_unchecked() })
        } else {
            match self.focus_stack.top().unwrap().node_get_child(node_key) {
                Some((consumed_bytes, child_node)) => {
                    debug_assert_eq!(consumed_bytes, node_key.len());
                    Some(child_node)
                },
                None => None
            }
        }
    }
}

impl<'a, 'path, V: Clone + Send + Sync + Unpin, A: Allocator + 'a> ZipperAbsolutePath for WriteZipperCore<'a, 'path, V, A> {
    fn origin_path(&self) -> &[u8] {
        self.key.origin_path()
    }
    fn root_prefix_path(&self) -> &[u8] {
        self.key.root_prefix_path()
    }
}

impl<'a, 'path, V: Clone + Send + Sync + Unpin, A: Allocator + 'a> ZipperPathBuffer for WriteZipperCore<'a, 'path, V, A> {
    unsafe fn origin_path_assert_len(&self, len: usize) -> &[u8] {
        if self.key.prefix_buf.capacity() > 0 {
            assert!(len <= self.key.prefix_buf.capacity());
            unsafe{ core::slice::from_raw_parts(self.key.prefix_buf.as_ptr(), len) }
        } else {
            assert!(len <= self.key.origin_path.len());
            unsafe{ &self.key.origin_path.as_slice_unchecked() }
        }
    }
    fn prepare_buffers(&mut self) { self.key.prepare_buffers() }
    fn reserve_buffers(&mut self, path_len: usize, stack_depth: usize) { self.key.reserve_buffers(path_len, stack_depth) }
}

impl <'a, V: Clone + Send + Sync + Unpin, A: Allocator> WriteZipperCore<'a, 'static, V, A> {
    pub(crate) fn new_with_node_and_cloned_path_in(root_node: &'a mut TrieNodeODRc<V, A>, root_val: Option<&'a mut Option<V>>, path: &[u8], root_prefix_len: usize, root_key_start: usize, alloc: A) -> Self {
        let (key, node) = node_along_path_mut(root_node, &path[root_key_start..], true);

        let new_root_key_start = root_prefix_len - key.len();
        Self::new_with_node_and_cloned_path_internal_in(node, root_val, path, new_root_key_start, alloc)
    }
    /// See [WriteZipperUntracked::new_with_node_and_path_internal]
    pub(crate) fn new_with_node_and_cloned_path_internal_in(root_node: &'a mut TrieNodeODRc<V, A>, root_val: Option<&'a mut Option<V>>, path: &[u8], root_key_start: usize, alloc: A) -> Self {
        let focus_stack = MutNodeStack::new(root_node);
        debug_assert!((path.len()-root_key_start == 0) != (root_val.is_none())); //We must have either a node_path or a root_val, but never both
        Self {
            key: KeyFields::new_cloned_path(path, root_key_start),
            root_val: root_val.map(|val| val as *mut Option<V>),
            focus_stack,
            alloc,
        }
    }
}

impl <'a, 'path, V: Clone + Send + Sync + Unpin, A: Allocator + 'a> WriteZipperCore<'a, 'path, V, A> {
    /// Creates a new zipper, with a path relative to a node
    pub(crate) fn new_with_node_and_path_in(root_node: &'a mut TrieNodeODRc<V, A>, root_val: Option<&'a mut Option<V>>, path: &'path [u8], root_prefix_len: usize, root_key_start: usize, alloc: A) -> Self {
        let (key, node) = node_along_path_mut(root_node, &path[root_key_start..], true);

        let new_root_key_start = root_prefix_len - key.len();
        Self::new_with_node_and_path_internal_in(node, root_val, path, new_root_key_start, alloc)
    }
    /// See [WriteZipperUntracked::new_with_node_and_path_internal]
    pub(crate) fn new_with_node_and_path_internal_in(root_node: &'a mut TrieNodeODRc<V, A>, root_val: Option<&'a mut Option<V>>, path: &'path [u8], root_key_start: usize, alloc: A) -> Self {
        let focus_stack = MutNodeStack::new(root_node);
        debug_assert!((path.len()-root_key_start == 0) != (root_val.is_none())); //We must have either a node_path or a root_val, but never both
        Self {
            key: KeyFields::new(path, root_key_start),
            root_val: root_val.map(|val| val as *mut Option<V>),
            focus_stack,
            alloc,
        }
    }

    // GOAT, Temporary (but medium term necessary)
    // / Temporary stand-in for the commented-out implementation below.  This whole function body should
    // / be deleted at the earliest opportunity.
    // /
    // / This temporary impl that treats read zippers the same as write zippers is needed on account of
    // / the fact that a read_zipper holds a reference to the root value, which lives in the parent node.
    // / However that node can be upgraded as part of a ZipperHead operation, to make a rooting site for
    // / another upstream zipper.  This unfortunately means we have no choice but to make the parent node
    // / into a CellNode, until we can change this unfortunate fact, by implementing the proposal in:
    // / [A.0001_map_root_values.md], Alternative 2.
    // /
    // / To head off the question: "Why not just clone ODRc reference the root into the read zipper" (I
    // / went part way down this implementation path myself), the answer is that some ReadZipper methods
    // / return borrows in the `'trie` lifetime, so the zipper can be dropped without invalidating the
    // / reference lifetime.
    // pub(crate) fn splitting_borrow_focus(&mut self) -> (TaggedNodeRef<'_, V, A>, Option<&V>) {
    //     let self_ptr: *mut Self = self;
    //     let node_key = self.key.node_key();
    //     if node_key.len() == 1 {
    //         let parent_node = self.focus_stack.top().unwrap().reborrow();
    //         if parent_node.is_cell_node() {
    //             if let Some(focus_node) = parent_node.node_get_child(node_key) {
    //                 let focus_node = focus_node.1.as_tagged();
    //                 let focus_val = parent_node.node_get_val(node_key);
    //                 return (focus_node, focus_val)
    //             }
    //         }
    //     }
    //     //SAFETY: This is another "We need polonius" case.  We are totally done with all the borrows
    //     // of self before we get here, but the borrow checker can't see that since one of the return
    //     // paths keeps the borrow alive
    //     let (zipper_root_node, zipper_root_val) = prepare_exclusive_write_path(unsafe{ &mut *self_ptr }, &[]);
    //     (zipper_root_node.as_tagged(), zipper_root_val.as_ref())
    // }

    /// Internal method to borrow the node at the zipper's focus, splitting the node if necessary
    ///
    /// This is called for the ZipperHead's root, the first time a ReadZipper is created in a ZipperHead,
    /// to isolate the part of the trie that is below the ZipperHead from the part of the trie above it.
    pub(crate) fn splitting_borrow_focus(&mut self) -> (*const TrieNodeODRc<V, A>, Option<NonNull<V>>) {
        let node = match self.try_borrow_focus() {
            Some(root) => root,
            None => {
                self.split_at_focus();
                self.try_borrow_focus().unwrap()
            },
        };
        let val = self.val();
        (node, val.map(|v| v.into()))
    }

    /// Internal method to ensure the focus begins at its own node, splitting the node if necessary
    pub(crate) fn split_at_focus(&mut self) {
        let alloc = self.alloc.clone();
        let sub_branch_added = self.in_zipper_mut_static_result(
            |node, key| {
                let new_node = if let Some(remaining) = node.take_node_at_key(key, false) {
                    remaining
                } else {
                    #[cfg(not(feature = "all_dense_nodes"))]
                    {
                        TrieNodeODRc::new_in(super::trie_core::line_list::LineListNode::new_in(alloc.clone()), alloc)
                    }
                    #[cfg(feature = "all_dense_nodes")]
                    {
                        TrieNodeODRc::new_in(super::trie_core::dense_byte::DenseByteNode::new_in(alloc.clone()), alloc)
                    }
                };
                node.node_set_branch(key, new_node)
            },
            |_, _| true);
        if sub_branch_added {
            self.mend_root();
            self.descend_to_internal();
        }
    }

    /// Similar to [WriteZipperCore::try_borrow_focus], but it may reset the focus_stack to an
    /// undescended root state.  This is currently only called by [prepare_exclusive_write_path]
    pub(crate) fn try_borrow_focus_mut(&mut self) -> Option<&mut TrieNodeODRc<V, A>> {
        let node_key = self.key.node_key();
        if node_key.len() == 0 {
            debug_assert!(self.at_root());
            debug_assert_eq!(self.focus_stack.depth(), 1);
            self.focus_stack.to_root();
            self.focus_stack.root_mut()
        } else {
            match self.focus_stack.top_mut().unwrap().node_into_child_mut(node_key) {
                Some((consumed_bytes, child_node)) => {
                    debug_assert_eq!(consumed_bytes, node_key.len());
                    Some(child_node)
                },
                None => None
            }
        }
    }

    /// Internal method to re-borrow a WriteZipperCore without the `'path` lifetime
    fn as_static_path_zipper(&mut self) -> &mut WriteZipperCore<'a, 'static, V, A> {
        self.prepare_buffers();
        debug_assert!(!self.key.origin_path.is_slice() || self.key.origin_path.len() == 0);
        unsafe{ &mut *(self as *mut WriteZipperCore<V, A>).cast() }
    }

    //GOAT, the concept of a regularized zipper might not be very useful for WriteZippers, so I may be able to delete this code
    // /// Ensures the zipper is in its regularized form
    // ///
    // /// Unlike a ReadZipper, a WriteZipper's regularized form is holding the parent node at the top of the
    // /// `focus_stack`, where `node_key()` contains the key necesary to access the zipper's focus.  The
    // /// reason is because the most common and expensive operations in a ReadZipper are moves and iteration,
    // /// while the most common operations in a WriteZipper are sets and grafts.  Therefore the regularized
    // /// form is the closest to what's needed to perform those ops
    // ///
    // /// Therefore, `node_key().len() == 0` is usually deregularized.
    // ///
    // /// There is a special case, however, when the `focus_stack.top()` is the zipper's root node.  A
    // /// "thread-safe" WriteZipper must be able to function without accessing the parent node, because
    // /// the parent node may be shared among multiple zippers.
    // #[inline]
    // fn is_regularized(&self) -> bool {
    //     let key_start = self.key.node_key_start();
    //     self.key.prefix_buf.len() > key_start || self.at_root()
    // }

    /// Returns the parent and path from which the top of the focus_stack can be re-acquired
    fn focus_parent(&self) -> &TrieNodeODRc<V, A> {
        let parent_key = self.key.parent_key();
        if parent_key.len() == 0 {
            return unsafe{ self.focus_stack.root_unchecked() }
        }

        let parent_node = self.focus_stack.before_top_unchecked();
        let (key_len, node) = parent_node.node_get_child(parent_key).unwrap();
        debug_assert_eq!(key_len, parent_key.len());
        node
    }

    /// See [ZipperValues::val]
    pub fn val(&self) -> Option<&V> {
        let node_key = self.key.node_key();
        if node_key.len() > 0 {
            self.focus_stack.top().unwrap().node_get_val(node_key)
        } else {
            debug_assert!(self.at_root());
            self.root_val.as_ref().and_then(|val| unsafe{&**val}.as_ref())
        }
    }
    /// See [ZipperWriting::get_val_mut]
    pub fn get_val_mut(&mut self) -> Option<&mut V> {
        let node_key = self.key.node_key();
        if node_key.len() > 0 {
            self.focus_stack.top_mut().unwrap().node_into_val_ref_mut(node_key)
        } else {
            debug_assert!(self.at_root());
            self.root_val.as_mut().and_then(|val| unsafe{&mut **val}.as_mut())
        }
    }
    /// Consumes the zipper and returns an `&mut` ref to the value at the zipper's focus
    /// Used in the implementation of some top-level PathMap ops
    ///
    /// **WARNING** This API must NOT be made public, because it would allow the tracker to
    /// drop while retaining access to nodes in the trie
    pub(crate) fn into_value_mut(mut self) -> Option<&'a mut V> {
        let node_key = self.key.node_key();
        if node_key.len() > 0 {
            self.focus_stack.into_top().unwrap().node_into_val_ref_mut(node_key)
        } else {
            debug_assert!(self.at_root());
            self.root_val.as_mut().and_then(|val| unsafe{&mut **val}.as_mut())
        }
    }
    /// See [ZipperWriting::get_val_or_set_mut]
    pub fn get_val_or_set_mut(&mut self, default: V) -> &mut V {
        self.get_val_or_set_mut_with(|| default)
    }
    /// See [ZipperWriting::get_val_or_set_mut_with]
    pub fn get_val_or_set_mut_with<F>(&mut self, func: F) -> &mut V
        where F: FnOnce() -> V
    {
        if !self.is_val() {
            self.set_val(func());
        }
        self.get_val_mut().unwrap()
    }
    /// See [ZipperWriting::set_val]
    pub fn set_val(&mut self, val: V) -> Option<V> {
        if self.key.node_key().len() == 0 {
            debug_assert!(self.at_root());
            let root_val_ref = self.root_val.as_mut().unwrap();
            let mut temp_val = Some(val);
            core::mem::swap(unsafe{&mut **root_val_ref}, &mut temp_val);
            return temp_val
        }
        let (old_val, created_subnode) = self.in_zipper_mut_static_result(
            |node, remaining_key| node.node_set_val(remaining_key, val),
            |_new_leaf_node, _remaining_key| (None, true));
        if created_subnode {
            self.mend_root();
            self.descend_to_internal();
        }
        old_val
    }
    /// See [ZipperWriting::remove_val]
    pub fn remove_val(&mut self, prune: bool) -> Option<V> {
        if self.key.node_key().len() == 0 {
            debug_assert!(self.at_root());
            let root_val_ref = self.root_val.as_mut().unwrap();
            return core::mem::take(unsafe{&mut **root_val_ref})
        }
        let mut focus_node = self.focus_stack.top_mut().unwrap();
        if let Some(result) = focus_node.node_remove_val(self.key.node_key(), prune) {
            if prune {
                self.prune_path_internal(false);
            }
            Some(result)
        } else {
            None
        }
    }
    /// See [WriteZipper::zipper_head]
    pub fn zipper_head<'z>(&'z mut self) -> ZipperHead<'z, 'a, V, A> {
        self.key.prepare_buffers();
        ZipperHead::new_borrowed(self.as_static_path_zipper())
    }
    /// Consumes the WriteZipper, returning a ZipperHead
    ///
    /// NOTE: Currently this is an internal-only method to enable the [PathMap::zipper_head] method,
    /// although it might be convenient to expose it publicly.  We'd need to make sure the ZipperHead could
    /// carry along the tracker.
    /// UPDATE: No!!!  We definitely don't want to make this method public because a WriteZipperOwned's
    /// WriteZipperCore must never be separated from the fields that back its root (map, etc.).  and also
    /// it should not be separated from its tracker.  So in general it's a very bad idea to consume a
    /// WriteZipperCore without also consuming the object that contains it.
    pub(crate) fn into_zipper_head(self) -> ZipperHead<'a, 'a, V, A> where 'path: 'static {
        //NOTE, we are assuming this method is called from [PathMap::zipper_head] on a freshly-created
        // WriteZipper at the map root.  Is there is an associated path, we need to call `prepare_buffers`,
        // just like [ZipperWriting::zipper_head] does above.
        debug_assert_eq!(self.key.node_key().len(), 0);
        ZipperHead::new_owned(self)
    }
    /// See [ZipperWriting::graft]
    pub fn graft<Z: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &Z) {
        self.graft_internal(read_zipper.get_focus().into_option());

        #[cfg(feature = "graft_root_vals")]
        let _ = match read_zipper.val() {
            Some(src_val) => self.set_val(src_val.clone()),
            None => self.remove_val(false)
        };
    }
    /// See [ZipperWriting::graft_map]
    pub fn graft_map(&mut self, map: PathMap<V, A>) {
        let (src_root_node, src_root_val) = map.into_root();
        self.graft_internal(src_root_node);

        #[cfg(not(feature = "graft_root_vals"))]
        let _ = src_root_val;
        #[cfg(feature = "graft_root_vals")]
        let _ = match src_root_val {
            Some(src_val) => self.set_val(src_val),
            None => self.remove_val(false)
        };
    }

    /// Optimized implementation of [ZipperWriting::graft_child_maps] for WriteZipperCore
    ///
    /// This implementation constructs a new node with the appropriate children directly,
    /// rather than descending/ascending for each child.
    pub fn graft_child_maps<I: IntoIterator<Item=PathMap<V, A>>>(&mut self, child_mask: ByteMask, maps: I, remove_unset: bool) {
        let map_count = child_mask.count_bits();

        // If we're replacing all children, and we have enough children to justify a ByteNode,
        // then we want to build a new node with the children from the maps
        if map_count > 2 && remove_unset {
            //GOAT, we could add a fast-path for `map_count > 2 && !remove_unset`, but for now
            // we'll let the slow-path handle it, since it requires logic to upgrade the focus_node to ByteNode

            let mut new_node = super::trie_core::dense_byte::DenseByteNode::with_capacity_in(map_count, self.alloc.clone());
            let mut maps_iter = maps.into_iter();
            for child_byte in child_mask.iter() {
                let map = maps_iter.next().expect("maps iterator returned fewer items than the number of set bits in child_mask");
                let (src_root_node, src_root_val) = map.into_root();
                if let Some(node) = src_root_node {
                    new_node.set_child(child_byte, node);
                }
                if let Some(val) = src_root_val {
                    new_node.set_val(child_byte, val);
                }
            }
            let new_node_odrc = TrieNodeODRc::new_in(new_node, self.alloc.clone());
            self.graft_internal(Some(new_node_odrc));
        } else {
            // If we don't have enough children to justify forcing a new ByteNode, just set the nodes
            if remove_unset {
                self.remove_branches(false);
            }
            let mut maps_iter = maps.into_iter();
            for child_byte in child_mask.iter() {
                let map = maps_iter.next().expect("maps iterator returned fewer items than the number of set bits in child_mask");
                let (src_root_node, src_root_val) = map.into_root();

                if let Some(node) = src_root_node {
                    self.set_node_at_child_path(&[child_byte], node)
                }
                if let Some(val) = src_root_val {
                    let _ = self.set_val_at_child_path(&[child_byte], val);
                }
            }
        }
    }

    /// Sets a child node at a position below the current focus
    #[inline]
    fn set_node_at_child_path(&mut self, path: &[u8], src: TrieNodeODRc<V, A>) {
        let sub_branch_added = self.with_node_at_path(path,
            |node, key| {
                node.node_set_branch(key, src)
            },
            |_, _| true);
        if sub_branch_added {
            self.mend_root();
            self.descend_to_internal();
        }
    }

    /// Sets a child value one byte below the focus
    #[inline]
    fn set_val_at_child_path(&mut self, path: &[u8], val: V) -> Option<V> {
        let (old_val, created_subnode) = self.with_node_at_path(path,
            |node, remaining_key| node.node_set_val(remaining_key, val),
            |_new_leaf_node, _remaining_key| (None, true));
        if created_subnode {
            self.mend_root();
            self.descend_to_internal();
        }
        old_val
    }

    /// See [ZipperWriting::join_into]
    pub fn join_into<Z: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &Z) -> AlgebraicStatus where V: Lattice {
        let src = read_zipper.get_focus();
        let self_focus = self.get_focus();
        if src.is_none() || src.as_tagged().node_is_empty() {
            if self_focus.is_none() || self_focus.as_tagged().node_is_empty() {
                return AlgebraicStatus::None
            } else {
                return AlgebraicStatus::Identity
            }
        }
        match self_focus.try_as_tagged() {
            Some(self_node) => {
                match self_node.pjoin_dyn(src.as_tagged()) {
                    AlgebraicResult::Element(joined) => {
                        self.graft_internal(Some(joined));
                        AlgebraicStatus::Element
                    }
                    AlgebraicResult::Identity(mask) => {
                        if mask & SELF_IDENT > 0 {
                            AlgebraicStatus::Identity
                        } else {
                            debug_assert!(mask & COUNTER_IDENT > 0);
                            self.graft_internal(src.into_option());
                            AlgebraicStatus::Element
                        }
                    },
                    AlgebraicResult::None => {
                        self.graft_internal(None);
                        AlgebraicStatus::None
                    }
                }
            },
            None => { self.graft_internal(src.into_option()); AlgebraicStatus::Element }
        }
    }
    /// See [ZipperWriting::join_map_into]
    pub fn join_map_into(&mut self, map: PathMap<V, A>) -> AlgebraicStatus where V: Lattice {
        let (src_root_node, src_root_val) = map.into_root();
        #[cfg(not(feature = "graft_root_vals"))]
        let _ = src_root_val;
        #[cfg(feature = "graft_root_vals")]
        let val_status = match (self.get_val_mut(), src_root_val) {
            (Some(self_val), Some(src_val)) => { self_val.join_into(src_val) },
            (None, Some(src_val)) => { self.set_val(src_val); AlgebraicStatus::Element },
            (Some(_), None) => { AlgebraicStatus::Identity },
            (None, None) => { AlgebraicStatus::None },
        };

        let self_focus = self.get_focus();
        let src = match src_root_node {
            Some(src) => src,
            None => {
                if self_focus.is_none() {
                    return AlgebraicStatus::None
                } else {
                    return AlgebraicStatus::Identity
                }
            }
        };
        let node_status = match self_focus.try_as_tagged() {
            Some(self_node) => {
                match self_node.pjoin_dyn(src.as_tagged()) {
                    AlgebraicResult::Element(joined) => {
                        self.graft_internal(Some(joined));
                        AlgebraicStatus::Element
                    },
                    AlgebraicResult::Identity(mask) => {
                        if mask & SELF_IDENT > 0 {
                            AlgebraicStatus::Identity
                        } else {
                            debug_assert!(mask & COUNTER_IDENT > 0);
                            self.graft_internal(Some(src));
                            AlgebraicStatus::Element
                        }
                    },
                    AlgebraicResult::None => {
                        self.graft_internal(None);
                        AlgebraicStatus::None
                    }
                }
            },
            None => { self.graft_internal(Some(src)); AlgebraicStatus::Element }
        };

        #[cfg(not(feature = "graft_root_vals"))]
        return node_status;
        #[cfg(feature = "graft_root_vals")]
        return node_status.merge(val_status, true, true)
    }
    /// See [ZipperWriting::join_into_take]
    pub fn join_into_take<Z: ZipperInfallibleSubtries<V, A> + ZipperWriting<V, A>>(&mut self, src_zipper: &mut Z, prune: bool) -> AlgebraicStatus where V: Lattice {
        match src_zipper.take_focus(prune) {
            None => {
                if self.get_focus().is_none() {
                    return AlgebraicStatus::None
                } else {
                    return AlgebraicStatus::Identity
                }
            },
            Some(src) => {
                match self.take_focus(false) {
                    Some(mut self_node) => {
                        let (status, result) = self_node.make_mut().join_into_dyn(src);
                        match result {
                            Ok(()) => self.graft_internal(Some(self_node)),
                            Err(replacement_node) => self.graft_internal(Some(replacement_node)),
                        }
                        status
                    },
                    None => {
                        self.graft_internal(Some(src));
                        AlgebraicStatus::Element
                    }
                }
            }
        }
    }
    /// See [ZipperWriting::join_k_path_into]
    pub fn join_k_path_into(&mut self, byte_cnt: usize, prune: bool) -> bool where V: Lattice {
        let result = match self.get_focus().into_option() {
            Some(mut self_node) => {
                let new_node = self_node.make_mut().drop_head_dyn(byte_cnt);
                let result = new_node.is_some();
                self.graft_internal(new_node);
                result
            },
            None => { false }
        };
        if prune && !result {
            self.prune_path();
        }
        result
    }
    /// See [ZipperWriting::meet_k_path_into]
    pub fn meet_k_path_into(&mut self, byte_cnt: usize, prune: bool) -> bool where V: Lattice {
        //GOAT, this is a provisional implementation with the wrong performance characteristics, but should have the right behavior
        let temp_map = if self.descend_first_k_path(byte_cnt) {
            let mut temp_map = self.take_map(false).unwrap_or_else(|| PathMap::new_in(self.alloc.clone()));

            while self.to_next_k_path(byte_cnt) {
                if temp_map.is_empty() {
                    self.ascend(byte_cnt);
                    break;
                }
                let other_map = self.take_map(false).unwrap_or_else(|| PathMap::new_in(self.alloc.clone()));
                temp_map = temp_map.meet(&other_map);
            }
            temp_map
        } else {
            PathMap::new_in(self.alloc.clone())
        };
        if temp_map.is_empty() {
            self.remove_branches(prune);
            false
        } else {
            self.graft_map(temp_map);
            true
        }
    }

    /// GOAT.  Trash impl of k_path iteration, to facilitate provisional impl of `meet_k_path_into`
    fn descend_first_k_path(&mut self, k: usize) -> bool {
        self.k_path_internal(k, self.path().len())
    }

    /// GOAT.  Trash impl of k_path iteration, to facilitate provisional impl of `meet_k_path_into`
    fn to_next_k_path(&mut self, k: usize) -> bool {
        let base_idx = if self.path().len() >= k {
            self.path().len() - k
        } else {
            return false
        };
        self.k_path_internal(k, base_idx)
    }

    /// GOAT.  Trash impl of k_path iteration, to facilitate provisional impl of `meet_k_path_into`
    #[inline]
    fn k_path_internal(&mut self, k: usize, base_idx: usize) -> bool {
        loop {
            if self.path().len() < base_idx + k {
                while self.descend_first_byte() {
                    if self.path().len() == base_idx + k { return true }
                }
            }
            if self.to_next_sibling_byte() {
                if self.path().len() == base_idx + k { return true }
                continue
            }
            while self.path().len() > base_idx {
                self.ascend_byte();
                if self.path().len() == base_idx { return false }
                if self.to_next_sibling_byte() { break }
            }
        }
    }

    /// See [ZipperWriting::insert_prefix]
    pub fn insert_prefix<K: AsRef<[u8]>>(&mut self, prefix: K) -> bool {
        let prefix = prefix.as_ref();
        match self.get_focus().into_option() {
            Some(focus_node) => {
                let prefixed = make_parents_in(prefix, focus_node, self.alloc.clone());
                self.graft_internal(Some(prefixed));
                true
            },
            None => { false }
        }
    }
    /// See [ZipperWriting::remove_prefix]
    pub fn remove_prefix(&mut self, n: usize) -> bool {

        let downstream_node = self.get_focus().into_option();

        let fully_ascended = self.ascend(n);

        self.graft_internal(downstream_node);
        fully_ascended
    }
    /// See [ZipperWriting::meet_into]
    pub fn meet_into<Z: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &Z, prune: bool) -> AlgebraicStatus where V: Lattice {
        let src_root_val = read_zipper.val();
        #[cfg(not(feature = "graft_root_vals"))]
        let _ = src_root_val;
        #[cfg(feature = "graft_root_vals")]
        let (val_status, val_was_none) = match (self.get_val_mut(), src_root_val) {
            (Some(self_val), Some(src_val)) => {
                let new_status = match self_val.pmeet(src_val) {
                    AlgebraicResult::Element(new_val) => {self.set_val(new_val); AlgebraicStatus::Element },
                    AlgebraicResult::None => {self.remove_val(prune); AlgebraicStatus::None },
                    AlgebraicResult::Identity(_) => { AlgebraicStatus::Identity }
                };
                (new_status, false)
            },
            (None, Some(_)) => { (AlgebraicStatus::None, true) },
            (Some(_), None) => { self.remove_val(prune); (AlgebraicStatus::None, false) },
            (None, None) => { (AlgebraicStatus::None, true) },
        };

        let node_status = match self.get_focus().try_as_tagged() {
            Some(self_node) => {
                if !self_node.node_is_empty() {
                    let src = read_zipper.get_focus();
                    if src.is_none() {
                        self.graft_internal(None);
                        if prune {
                            self.prune_path();
                        }
                        AlgebraicStatus::None
                    } else {
                        match self_node.pmeet_dyn(src.as_tagged()) {
                            AlgebraicResult::Element(intersection) => {
                                self.graft_internal(Some(intersection));
                                AlgebraicStatus::Element
                            },
                            AlgebraicResult::None => {
                                self.graft_internal(None);
                                if prune {
                                    self.prune_path();
                                }
                                AlgebraicStatus::None
                            },
                            AlgebraicResult::Identity(mask) => {
                                if mask & SELF_IDENT > 0 {
                                    AlgebraicStatus::Identity
                                } else {
                                    debug_assert_eq!(mask, COUNTER_IDENT); //It's gotta be self or other
                                    self.graft_internal(Some(src.into_option().unwrap()));
                                    AlgebraicStatus::Element
                                }
                            },
                        }
                    }
                } else {
                    AlgebraicStatus::None
                }
            },
            None => {
                AlgebraicStatus::None
            }
        };

        #[cfg(not(feature = "graft_root_vals"))]
        return node_status;
        #[cfg(feature = "graft_root_vals")]
        return node_status.merge(val_status, false, val_was_none)
    }
    /// See [WriteZipper::meet_2]
    pub fn meet_2<ZA: ZipperInfallibleSubtries<V, A>, ZB: ZipperInfallibleSubtries<V, A>>(&mut self, rz_a: &ZA, rz_b: &ZB) -> AlgebraicStatus where V: Lattice {
        let a_focus = rz_a.get_focus();
        let a = match a_focus.try_as_tagged() {
            Some(src) => src,
            None => {
                self.graft_internal(None);
                return AlgebraicStatus::None
            }
        };
        let b_focus = rz_b.get_focus();
        let b = match b_focus.try_as_tagged() {
            Some(src) => src,
            None => {
                self.graft_internal(None);
                return AlgebraicStatus::None
            }
        };
        match a.pmeet_dyn(b) {
            AlgebraicResult::Element(intersection) => {
                self.graft_internal(Some(intersection));
                AlgebraicStatus::Element
            },
            AlgebraicResult::None => {
                self.graft_internal(None);
                AlgebraicStatus::None
            },
            AlgebraicResult::Identity(mask) => {
                if mask & SELF_IDENT > 0 {
                    //GOAT, document that meet_2 will not return identify because it doesn't actually check what's in the destination
                    self.graft_internal(Some(a_focus.into_option().unwrap()));
                } else {
                    debug_assert_eq!(mask, COUNTER_IDENT); //It's gotta be a or b
                    self.graft_internal(Some(b_focus.into_option().unwrap()));
                }
                AlgebraicStatus::Element
            },
        }
    }
    /// See [ZipperWriting::subtract_into]
    pub fn subtract_into<Z: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &Z, prune: bool) -> AlgebraicStatus where V: DistributiveLattice {
        let src_root_val = read_zipper.val();
        #[cfg(not(feature = "graft_root_vals"))]
        let _ = src_root_val;
        #[cfg(feature = "graft_root_vals")]
        let (val_status, val_was_none) = match (self.get_val_mut(), src_root_val) {
            (Some(self_val), Some(src_val)) => {
                let new_status = match self_val.psubtract(src_val) {
                    AlgebraicResult::Element(new_val) => {self.set_val(new_val); AlgebraicStatus::Element },
                    AlgebraicResult::None => {self.remove_val(prune); AlgebraicStatus::None },
                    AlgebraicResult::Identity(_) => { AlgebraicStatus::Identity }
                };
                (new_status, false)
            },
            (None, Some(_)) => { (AlgebraicStatus::None, true) },
            (Some(_), None) => { (AlgebraicStatus::Identity, false) },
            (None, None) => { (AlgebraicStatus::None, true) },
        };

        let src = read_zipper.get_focus();
        let self_focus = self.get_focus();
        let self_focus = self_focus.try_as_tagged().and_then(|node| {
            match node.node_is_empty() {
                true => None,
                false => Some(node)
            }
        });
        let node_status = if src.is_none() {
            if self_focus.is_none() {
                AlgebraicStatus::None
            } else {
                AlgebraicStatus::Identity
            }
        } else {
            match self_focus {
                Some(self_node) => {
                    match self_node.psubtract_dyn(src.as_tagged()) {
                        AlgebraicResult::Element(diff) => {
                            self.graft_internal(Some(diff));
                            AlgebraicStatus::Element
                        },
                        AlgebraicResult::None => {
                            self.graft_internal(None);
                            if prune {
                                self.prune_path();
                            }
                            AlgebraicStatus::None
                        },
                        AlgebraicResult::Identity(mask) => {
                            debug_assert_eq!(mask, SELF_IDENT); //subtract is non-commutative
                            AlgebraicStatus::Identity
                        },
                    }
                },
                None => {
                    AlgebraicStatus::None
                }
            }
        };

        #[cfg(not(feature = "graft_root_vals"))]
        return node_status;
        #[cfg(feature = "graft_root_vals")]
        return node_status.merge(val_status, false, val_was_none)
    }
    /// See [WriteZipper::restrict]
    pub fn restrict<Z: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &Z) -> AlgebraicStatus {
        let src = read_zipper.get_focus();
        if src.is_none() {
            self.graft_internal(None);
            return AlgebraicStatus::None
        }
        match self.get_focus().try_as_tagged() {
            Some(self_node) => {
                match self_node.prestrict_dyn(src.as_tagged()) {
                    AlgebraicResult::Element(restricted) => {
                        self.graft_internal(Some(restricted));
                        AlgebraicStatus::Element
                    },
                    AlgebraicResult::None => {
                        self.graft_internal(None);
                        AlgebraicStatus::None
                    },
                    AlgebraicResult::Identity(mask) => {
                        debug_assert_eq!(mask, SELF_IDENT); //restrict is non-commutative
                        AlgebraicStatus::Identity
                    },
                }
            },
            None => AlgebraicStatus::None
        }
    }
    /// See [WriteZipper::restricting]
    pub fn restricting<Z: ZipperInfallibleSubtries<V, A>>(&mut self, read_zipper: &Z) -> bool {
        let src = read_zipper.get_focus();
        if src.is_none() {
            return false
        }
        match self.get_focus().try_as_tagged() {
            Some(self_node) => {
                match src.as_tagged().prestrict_dyn(self_node) {
                    AlgebraicResult::Element(restricted) => self.graft_internal(Some(restricted)),
                    AlgebraicResult::None => self.graft_internal(None),
                    AlgebraicResult::Identity(mask) => {
                        debug_assert_eq!(mask, SELF_IDENT); //restrict is non-commutative
                        self.graft_internal(src.into_option())
                    },
                }
                true
            },
            None => false
        }
    }
    /// See [WriteZipper::remove_branches]
    pub fn remove_branches(&mut self, prune: bool) -> bool {
        let node_key = self.key.node_key();
        if node_key.len() > 0 {
            let mut focus_node = self.focus_stack.top_mut().unwrap();
            if focus_node.node_remove_all_branches(node_key, prune) {
                if prune {
                    self.prune_path_internal(false);
                }
                true
            } else {
                false
            }
        } else {
            debug_assert_eq!(self.focus_stack.depth(), 1);
            if self.focus_stack.top().map(|node| node.node_is_empty()).unwrap_or(false) {
                return false
            } else {
                self.focus_stack.to_root();
                let stack_root = self.focus_stack.root_mut().unwrap();
                *stack_root = TrieNodeODRc::new_allocated_in(0, 0, self.alloc.clone());
                true
            }
        }
    }
    /// See [WriteZipper::take_map]
    pub fn take_map(&mut self, prune: bool) -> Option<PathMap<V, A>> {
        #[cfg(not(feature = "graft_root_vals"))]
        let root_val = None;
        #[cfg(feature = "graft_root_vals")]
        let root_val = self.remove_val(prune);

        let root_node = self.take_focus(prune);

        self.get_focus().into_option();
        if root_node.is_some() || root_val.is_some() {
            Some(PathMap::new_with_root_in(root_node, root_val, self.alloc.clone()))
        } else {
            None
        }
    }
    /// See [WriteZipper::remove_unmasked_branches]
    pub fn remove_unmasked_branches(&mut self, mask: ByteMask, prune: bool) {
        let mut focus_node = self.focus_stack.top_mut().unwrap();
        let node_key = self.key.node_key();
        if node_key.len() > 0 {
            match focus_node.node_get_child_mut(node_key) {
                Some((consumed_bytes, child_node)) => {
                    if node_key.len() >= consumed_bytes {
                        child_node.make_mut().node_remove_unmasked_branches(&node_key[consumed_bytes..], mask, prune);
                        if child_node.as_tagged().node_is_empty() {
                            focus_node.node_remove_all_branches(&node_key[..consumed_bytes], prune);
                        }
                    } else {
                        //Zipper is positioned at non-existent node.  Removing anything from nothing is nothing
                    }
                },
                None => {
                    focus_node.node_remove_unmasked_branches(node_key, mask, prune);
                }
            }
        } else {
            debug_assert!(self.key.prefix_buf.len() <= self.key.origin_path.len()); //Equivalent to `self.at_root()`, but can't borrow `self` here
            focus_node.node_remove_unmasked_branches(node_key, mask, prune);
        }
        if prune {
            self.prune_path_internal(false);
        }
    }

    /// See [ZipperWriting::create_path]
    fn create_path(&mut self) -> bool {
        if self.key.node_key().len() == 0 {
            debug_assert!(self.at_root());
            return false;
        } else {
            let (created_path, created_subnode) = self.in_zipper_mut_static_result(
                |node, remaining_key| node.node_create_dangling(remaining_key),
                |_new_leaf_node, _remaining_key| (true, true));
            if created_subnode {
                self.mend_root();
                self.descend_to_internal();
            }
            created_path
        }
    }

    /// See [ZipperWriting::prune_path]
    pub(crate) fn prune_path(&mut self) -> usize {
        let key = self.key.node_key();
        if key.len() > 0 {
            let node_pruned_bytes = self.focus_stack.top_mut().unwrap().node_remove_dangling(key);
            let trie_pruned_bytes = if node_pruned_bytes > 0 {
                self.prune_path_internal(false)
            } else { 0 };
            node_pruned_bytes.max(trie_pruned_bytes)
        } else {
            0
        }
    }

    /// See [ZipperWriting::prune_ascend]
    fn prune_ascend(&mut self) -> usize {
        let bytes = self.prune_path();
        self.ascend(bytes);
        bytes
    }

    /// Internal method, Removes and returns the node at the zipper's focus.  This method may leave behind a dangling path
    #[inline]
    fn take_focus(&mut self, prune: bool) -> Option<TrieNodeODRc<V, A>> {
        let mut focus_node = self.focus_stack.top_mut().unwrap();
        let node_key = self.key.node_key();
        if node_key.len() == 0 {
            debug_assert!(self.at_root());
            let mut replacement_node = TrieNodeODRc::new_allocated_in(0, 0, self.alloc.clone());
            self.focus_stack.backtrack();
            let stack_root = self.focus_stack.root_mut().unwrap();
            core::mem::swap(stack_root, &mut replacement_node);
            if !replacement_node.as_tagged().node_is_empty() {
                Some(replacement_node)
            } else {
                None
            }
        } else {
            if let Some(new_node) = focus_node.take_node_at_key(node_key, prune) {
                if prune {
                    self.prune_path_internal(false);
                }
                Some(new_node)
            } else {
                None
            }
        }
    }

    #[inline]
    fn take_root_prefix_path(&mut self) -> Vec<u8> {
        self.prepare_buffers();
        self.reset();
        let mut prefix_path = vec![];
        core::mem::swap(&mut self.key.prefix_buf, &mut prefix_path);

        if !self.key.origin_path.is_slice() {
            //This leaves the zipper with a potentially messed-up origin path, which is ok if we are ready to
            // drop this zipper.  If, however, we want to continue using it, we need to make a new prefix_path
            // that is initialized with the origin_path data, instead of setting it to a zero-length slice
            self.key.origin_path.set_slice(&[]);
        }
        prefix_path
    }

    /// Internal implementation of graft, and other methods that do the same thing
    #[inline]
    pub(crate) fn graft_internal(&mut self, src: Option<TrieNodeODRc<V, A>>) {
        match src {
            Some(src) => {
                debug_assert!(!src.as_tagged().node_is_empty());
                if self.key.node_key().len() > 0 {
                    //The focus_stack.top() is the parent node of the focus, so we'll replace its child
                    let sub_branch_added = self.in_zipper_mut_static_result(
                        |node, key| {
                            node.node_set_branch(key, src)
                        },
                        |_, _| true);
                    if sub_branch_added {
                        self.mend_root();
                        self.descend_to_internal();
                    }
                } else {
                    //The zipper is at the root, so we need to replace the root node
                    debug_assert!(self.at_root());
                    debug_assert_eq!(self.key.prefix_idx.len(), 0);
                    debug_assert_eq!(self.key.prefix_buf.len(), self.key.origin_path.len());
                    debug_assert_eq!(self.focus_stack.depth(), 1);
                    self.focus_stack.to_root();
                    let stack_root = self.focus_stack.root_mut().unwrap();
                    *stack_root = src;
                }
            },
            None => { self.remove_branches(false); }
        }
    }

    /// An internal function to attempt a mutable operation on a node, and replace the node if the node needed
    /// to be upgraded
    #[inline]
    pub(crate) fn in_zipper_mut_static_result<NodeF, RetryF, R>(&mut self, node_f: NodeF, retry_f: RetryF) -> R
        where
        NodeF: FnOnce(&mut TaggedNodeRefMut<'_, V, A>, &[u8]) -> Result<R, TrieNodeODRc<V, A>>,
        RetryF: FnOnce(&mut TaggedNodeRefMut<'_, V, A>, &[u8]) -> R,
    {
        let key = self.key.node_key();
        match node_f(&mut self.focus_stack.top_mut().unwrap(), key) {
            Ok(result) => result,
            Err(replacement_node) => {
                replace_top_node(&mut self.focus_stack, &self.key, replacement_node);
                retry_f(&mut self.focus_stack.top_mut().unwrap(), key)
            },
        }
    }

    /// An internal function to follow a path relative to the zipper's focus, and perform an operation on the
    /// node at the path
    #[inline]
    pub(crate) fn with_node_at_path<NodeF, RetryF, R>(&mut self, path: &[u8], node_f: NodeF, retry_f: RetryF) -> R
        where
        NodeF: FnOnce(&mut TaggedNodeRefMut<'_, V, A>, &[u8]) -> Result<R, TrieNodeODRc<V, A>>,
        RetryF: FnOnce(&mut TaggedNodeRefMut<'_, V, A>, &[u8]) -> R,
    {
        let key = self.key.node_key();
        let mut focus_node = self.focus_stack.top_mut().unwrap();
        if let Some((key_bytes, child_node)) = focus_node.node_get_child_mut(key) {
            debug_assert_eq!(key_bytes, key.len());
            let (key, node) = node_along_path_mut(child_node, path, true);
            let mut node_ref = node.make_mut();
            match node_f(&mut node_ref, key) {
                Ok(result) => result,
                Err(replacement_node) => {
                    *node = replacement_node;
                    retry_f(&mut node.make_mut(), key)
                },
            }
        } else {
            self.in_zipper_mut_static_result(
                |focus_node, partial_key| {
                    let mut key_buf = [0u8; MAX_NODE_KEY_BYTES];
                    key_buf[0..partial_key.len()].copy_from_slice(partial_key);
                    //GOAT, currently this will panic if the path is too long to fit in the buffer, which means this internal API
                    // isn't suitable for general-purpose path-based ops yet, but we're using it to deal with single-byte ops
                    key_buf[partial_key.len()..partial_key.len()+path.len()].copy_from_slice(path);
                    let full_key = &key_buf[0..partial_key.len()+path.len()];
                    node_f(focus_node, full_key)
                },
                retry_f
            )
        }
    }

    /// Internal method to recursively prune empty nodes from the trie, starting at the focus, and working
    /// upward until a value or branch is encountered.
    ///
    /// If `should_ascend` is `false`, then this method does not move the zipper, but may cause [Self::path_exists]
    /// to return `false`.  If `should_ascend` is `true`, this method will move the zipper in an identical way
    /// to [`ZipperMoving::ascend_until`]
    pub(crate) fn prune_path_internal(&mut self, should_ascend: bool) -> usize {

        //We need to make sure we're at the end of a dangling path
        if !self.focus_stack.top().unwrap().node_is_empty() {
            return 0
        }

        //Reimplementation of KeyFields.origin_path(), to allow us to split the borrow
        let path_buf = if self.key.prefix_buf.capacity() == 0 {
            self.key.origin_path.as_slice()
        } else {
            &self.key.prefix_buf[..]
        };
        let mut temp_path = path_buf;
        let mut ascended = false;
        let mut just_popped = false;
        let mut node_key_end = temp_path.len();

        //This loop mirrors the behavior of `ascend_until`, popping from the node stack but leaving the path buffer alone
        loop {
            debug_assert!(temp_path.len() >= self.key.origin_path.len());
            if temp_path.len() == 0 || temp_path.len() == self.key.origin_path.len() {
                break
            }
            let node_key_start = self.key.node_key_start();
            let node_key = &temp_path[node_key_start..];

            //This mirrors the logic of `ascend_within_node`, but using our alternative path buffer
            let branch_key = self.focus_stack.top().unwrap().prior_branch_key(node_key);
            let new_len = self.key.origin_path.len().max(node_key_start + branch_key.len());
            ascended = true;
            temp_path = &temp_path[..new_len];

            // When we break, we want to drop everything after the current `node_key`, so
            // this will yield a 1-byte node key
            node_key_end = new_len + 1;

            //This mirrors the logic of `ascend_across_nodes`
            let node_key = &temp_path[node_key_start..];
            if node_key.len() == 0 {
                if self.key.prefix_idx.len() == 0 {
                    break;
                }
                self.focus_stack.try_backtrack_node();
                self.key.prefix_idx.pop();
                just_popped = true;
            }

            //This mirrors the logic of `child_count` and `is_val`
            let mut node_key_start = self.key.node_key_start();
            let node_key = &temp_path[node_key_start..];
            let focus_node = self.focus_stack.top().unwrap();
            if node_count_branches_recursive(focus_node, node_key) > 1 {
                if just_popped {
                    let mut node_path = &path_buf[node_key_start..];
                    Self::descend_step_internal(&mut self.focus_stack, &mut self.key.prefix_idx, &mut node_path, &mut node_key_start);
                }
                break
            }
            just_popped = false;

            //If we encounter a value, then we should also stop ascending
            if focus_node.node_contains_val(node_key) {
                //In this case, we want to remove the current subtrie, as opposed to the subtrie
                // one level deeper that we remove in the other cases, when we stop due to a branch
                node_key_end = temp_path.len();
                break;
            }
        };

        //At this point, the zipper's node stack reflects the position it would be after `ascend_until`
        if ascended {
            let mut focus_node = self.focus_stack.top_mut().unwrap();
            let node_key_start = self.key.node_key_start();
            let next_node_key = &path_buf[node_key_start..node_key_end];

            //The path to the node or subnode we need to remove might not be within the focus node,
            // so get the actual node that we want to remove the contents from
            let (mut container_node, next_node_key) = match focus_node.node_get_child_mut(next_node_key) {
                Some((consumed_bytes, new_focus)) => {
                    if consumed_bytes < next_node_key.len() {
                        (new_focus.make_mut(), &next_node_key[consumed_bytes..])
                    } else {
                        (focus_node, next_node_key)
                    }
                },
                None => (focus_node, next_node_key)
            };

            let removed = container_node.node_remove_all_branches(next_node_key, true);

            //If we got here, we should have either removed something, or we should be at the top of the zipper
            debug_assert!(removed || self.focus_stack.depth()==1);
        }
        debug_assert!(temp_path.len() >= self.key.origin_path.len());
        let pruned_bytes = path_buf.len() - temp_path.len();

        if should_ascend {
            self.key.prefix_buf.truncate(temp_path.len());
        }

        pruned_bytes
    }

    /// Internal method that regularizes the `focus_stack` if nodes were created above the root
    #[inline]
    pub(crate) fn mend_root(&mut self) {
        if self.key.prefix_idx.len() == 0 && self.key.origin_path.len() > 1 {
            debug_assert_eq!(self.focus_stack.depth(), 1);

            let root_prefix_path = &self.key.root_prefix_path();
            let node_key_start = self.key.node_key_start();
            if node_key_start < root_prefix_path.len() {
                let root_slice = &root_prefix_path[node_key_start..];
                let root_ref = self.focus_stack.take_root().unwrap();
                let (key, node) = node_along_path_mut(root_ref, root_slice, true);
                if key.len() < root_slice.len() {
                    self.key.root_key_start += root_slice.len() - key.len();
                }
                self.focus_stack.replace_root(node);
            }
        }
    }

    /// Internal method to perform the part of `descend_to` that moves the focus node
    pub(crate) fn descend_to_internal(&mut self) {

        let mut key_start = self.key.node_key_start();
        //NOTE: this is a copy of the self.key.node_key() function, but we can't borrow the whole key structure in this code
        let mut key = if self.key.prefix_buf.len() > 0 {
            &self.key.prefix_buf
        } else {
            unsafe{ self.key.origin_path.as_slice_unchecked() }
        };
        key = &key[key_start..];
        //Explanation: This 2 is based on the fact that a WriteZipper's focus_stack holds the parent node
        // to the focus, so we must have a `node_key` unless the zipper is at the root, and the minimum
        // `node_key` length is 1 byte
        if key.len() < 2 {
            return;
        }

        //Step until we get to the end of the key or find a leaf node
        while Self::descend_step_internal(&mut self.focus_stack, &mut self.key.prefix_idx, &mut key, &mut key_start) { }
    }

    /// Follows the path buffer, pushing a single node onto the stack
    #[inline]
    pub(crate) fn descend_step_internal(focus_stack: &mut MutNodeStack<'a, V, A>, prefix_idx: &mut Vec<usize>, key: &mut &[u8], key_start: &mut usize) -> bool {
        focus_stack.advance(|node| {
            if let Some((consumed_byte_cnt, next_node)) = node.node_get_child_mut(key) {
                if consumed_byte_cnt < key.len() {
                    *key_start += consumed_byte_cnt;
                    prefix_idx.push(*key_start);
                    *key = &key[consumed_byte_cnt..];
                    Some(next_node.make_mut())
                } else {
                    None
                }
            } else {
                None
            }
        })
    }
    /// Internal method which doesn't actually move the zipper, but ensures `self.node_key().len() > 0`
    /// WARNING, must never be called if `self.node_key().len() != 0`
    #[inline]
    fn ascend_across_nodes(&mut self) {
        debug_assert!(self.key.node_key().len() == 0);
        self.focus_stack.try_backtrack_node();
        self.key.prefix_idx.pop();
    }
    /// Internal method used to impement `ascend_until` when ascending within a node
    #[inline]
    fn ascend_within_node(&mut self) {
        let branch_key = self.focus_stack.top().unwrap().prior_branch_key(self.key.node_key());
        let new_len = self.key.origin_path.len().max(self.key.node_key_start() + branch_key.len());
        self.key.prefix_buf.truncate(new_len);
    }
}

/// An internal function to replace the node at a the top of the focus stack
#[inline]
pub(crate) fn replace_top_node<'cursor, V: Clone + Send + Sync, A: Allocator + 'cursor>(focus_stack: &mut MutNodeStack<'cursor, V, A>,
    key: &KeyFields, replacement_node: TrieNodeODRc<V, A>)
{
    if focus_stack.depth() > 1 {
        focus_stack.backtrack();
        let mut parent_node = unsafe{ focus_stack.top_mut().unwrap_unchecked() };
        let parent_key = key.parent_key();
        parent_node.node_replace_child(parent_key, replacement_node);
        focus_stack.advance(|node| node.node_get_child_mut(parent_key).map(|(_, child_node)| child_node.make_mut()));
    } else {
        let stack_root = focus_stack.root_mut().unwrap();
        *stack_root = replacement_node;
    }
}

/// An internal function to replace the node at a the top of the focus stack
#[inline]
pub(crate) fn swap_top_node<'cursor, V: Clone + Send + Sync, A: Allocator + 'cursor, F>(focus_stack: &mut MutNodeStack<'cursor, V, A>,
    key: &KeyFields, func: F)
    where F: FnOnce(TrieNodeODRc<V, A>) -> TrieNodeODRc<V, A>
{
    if focus_stack.depth() > 1 {
        focus_stack.backtrack();
        let mut parent_node = unsafe{ focus_stack.top_mut().unwrap_unchecked() };
        let parent_key = key.parent_key();
        let existing_node = parent_node.take_node_at_key(parent_key, false).unwrap();
        let replacement_node = func(existing_node);
        parent_node.node_set_branch(parent_key, replacement_node).unwrap();
        focus_stack.advance(|node| node.node_get_child_mut(parent_key).map(|(_, child_node)| child_node.make_mut()));
    } else {
        let stack_root = focus_stack.root_mut().unwrap();
        let mut temp_node = TrieNodeODRc::new_empty();
        core::mem::swap(&mut temp_node, stack_root);
        let replacement_node = func(temp_node);
        *stack_root = replacement_node;
    }
}

/// Internal function to create a parent path leading up to the supplied `child_node`
#[inline]
fn make_parents_in<V: Clone + Send + Sync, A: Allocator>(path: &[u8], child_node: TrieNodeODRc<V, A>, alloc: A) -> TrieNodeODRc<V, A> {

    #[cfg(not(feature = "all_dense_nodes"))]
    {
        #[cfg(not(feature = "bridge_nodes"))]
        {
            let mut new_node = super::trie_core::line_list::LineListNode::new_in(alloc.clone());
            new_node.node_set_branch(path, child_node).unwrap_or_else(|_| panic!());
            TrieNodeODRc::new_in(new_node, alloc)
        }
        #[cfg(feature = "bridge_nodes")]
        {
            let new_node = super::trie_core::bridge::BridgeNode::new_in(path, true, child_node.into());
            TrieNodeODRc::new_in(new_node)
        }
    }

    #[cfg(feature = "all_dense_nodes")]
    {
        let mut end = child_node;
        for i in (0..path.len()).rev() {
            let mut new_node = super::trie_core::dense_byte::DenseByteNode::new_in(alloc.clone());
            new_node.set_child(path[i], end);
            end = TrieNodeODRc::new_in(new_node, alloc.clone());
        }
        end
    }
}

impl KeyFields<'static> {
    #[inline]
    fn new_cloned_path(path: &[u8], root_key_start: usize) -> Self {
        let prefix_buf = path.to_vec();
        Self {
            origin_path: SliceOrLen::new_owned(path.len()),
            root_key_start,
            prefix_buf,
            prefix_idx: vec![],
        }
    }
}

impl<'k> KeyFields<'k> {
    #[inline]
    fn new(path: &'k [u8], root_key_start: usize) -> Self {
        Self {
            origin_path: path.into(),
            root_key_start,
            prefix_buf: vec![],
            prefix_idx: vec![],
        }
    }
    /// Local implementation of `origin_path`
    pub(crate) fn origin_path(&self) -> &[u8] {
        if self.prefix_buf.capacity() == 0 {
            unsafe{ self.origin_path.as_slice_unchecked() }
        } else {
            &self.prefix_buf
        }
    }
    pub(crate) fn root_prefix_path(&self) -> &[u8] {
        if self.prefix_buf.capacity() > 0 {
            &self.prefix_buf[..self.origin_path.len()]
        } else {
            unsafe{ &self.origin_path.as_slice_unchecked() }
        }
    }
    /// Internal method to ensure buffers to facilitate movement of zipper are allocated and initialized
    #[inline]
    pub(crate) fn prepare_buffers(&mut self) {
        if self.prefix_buf.capacity() == 0 {
            self.reserve_buffers(EXPECTED_PATH_LEN, EXPECTED_DEPTH)
        }
    }
    #[cold]
    fn reserve_buffers(&mut self, path_len: usize, stack_depth: usize) {
        let path_len = path_len.max(self.origin_path.len());
        if self.prefix_buf.capacity() < path_len {
            let was_unallocated = self.prefix_buf.capacity() == 0;
            self.prefix_buf.reserve(path_len.saturating_sub(self.prefix_buf.len()));
            if was_unallocated {
                self.prefix_buf.extend(unsafe{ self.origin_path.as_slice_unchecked() });
            }
        }
        if self.prefix_idx.capacity() < stack_depth {
            self.prefix_idx.reserve(stack_depth.saturating_sub(self.prefix_idx.len()));
        }
    }
    /// Internal method returning the index to the key char beyond the path to the `self.focus_node`
    #[inline]
    pub(crate) fn node_key_start(&self) -> usize {
        self.prefix_idx.last().map(|i| *i).unwrap_or(self.root_key_start)
    }
    /// Internal method returning the key within the focus node
    #[inline]
    pub(crate) fn node_key(&self) -> &[u8] {
        let key_start = self.node_key_start();
        if self.prefix_buf.len() > 0 {
            &self.prefix_buf[key_start..]
        } else {
            unsafe{ &self.origin_path.as_slice_unchecked()[key_start..] }
        }
    }
    /// Internal method similar to `self.node_key().len()`, but returns the number of chars that can be
    /// legally ascended within the node, taking into account the root_key
    #[inline]
    fn excess_key_len(&self) -> usize {
        self.prefix_buf.len() - self.prefix_idx.last().map(|i| *i).unwrap_or(self.origin_path.len())
    }
    /// Internal method returning the key that leads to `self.focus_node` within the parent
    ///
    /// See corresponding function [read_zipper_core::ReadZipperCore::parent_key] for more context
    #[inline]
    pub(crate) fn parent_key(&self) -> &[u8] {
        if self.prefix_buf.len() > 0 {
            let key_start = if self.prefix_idx.len() > 1 {
                unsafe{ *self.prefix_idx.get_unchecked(self.prefix_idx.len()-2) }
            } else {
                self.root_key_start
            };
            &self.prefix_buf[key_start..self.node_key_start()]
        } else {
            &[]
        }
    }
}

// ***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---
// MutNodeStack: A replacement for MutCursor, follows the same pattern but stores TaggedNodeRefMut internally
// ***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---

use mut_node_stack::MutNodeStack;
mod mut_node_stack {
    use smallvec::{SmallVec, smallvec};
    use core::ptr::NonNull;
    use core::marker::PhantomData;
    use super::super::alloc::Allocator;
    use super::super::trie_core::node::{TaggedNodeRef, TaggedNodeRefMut, TaggedNodePtr, TrieNodeODRc};

    /// See [mutcursor::MutCursorRootedVec] for discussion about behavior
    pub struct MutNodeStack<'a, V: Clone + Send + Sync, A: Allocator> {
        root: Option<NonNull<TrieNodeODRc<V, A>>>,
        //GOAT, TODO get ridda small_vec.  I don't think it does anything for us anymore because we recreate the root TaggedNodeRefMut as needed
        stack: SmallVec<[TaggedNodePtr<V, A>; 1]>,
        phantom: PhantomData<TaggedNodeRefMut<'a, V, A>>
    }

    impl<'a, V: Clone + Send + Sync, A: Allocator + 'a> MutNodeStack<'a, V, A> {
        #[inline]
        pub fn new(root: &'a mut TrieNodeODRc<V, A>) -> Self {
            Self { root: Some(NonNull::from(root)), stack: smallvec![], phantom: PhantomData }
        }
        #[inline]
        pub fn top(&self) -> Option<TaggedNodeRef<'_, V, A>> {
            match self.stack.last() {
                Some(top) => Some(unsafe{ top.as_tagged() }),
                None => unsafe{ self.root.and_then(|mut ptr| {
                    let ptr = ptr.as_mut();
                    match ptr.is_empty() {
                        true => None,
                        false => Some(ptr.make_mut().cast())
                    }
                }) }
            }
        }
        ///GOAT, This is going to make miri mad.  This is used in the creation of a ReadZipper, forked
        /// from this WriteZipper.  Ideally we'd ascend the zipper up a level, do the forking, and re-descend,
        /// however we don't have a mutable borrow of the source zipper in `fork_read_zipper`, which leads here.
        ///While this method can cause UB and must be fixed, fixing it is a pain and I'm going to hold off until
        /// the change to the TrieNode contract moves root values inside nodes
        ///
        ///GOAT update... Why can't we just use the same logic as `get_focus`??
        pub fn before_top_unchecked(&self) -> TaggedNodeRef<'a, V, A> {
            if self.stack.len() >= 2 {
                let before_last = self.stack.len() - 2;
                unsafe{ self.stack[before_last].as_tagged() }
            } else {
                unsafe{ self.root.unwrap().as_mut().make_mut().cast() }
            }
        }
        #[inline]
        pub fn into_top(mut self) -> Option<TaggedNodeRefMut<'a, V, A>> {
            match self.stack.pop() {
                Some(node_ptr) => unsafe{ Some(node_ptr.into_tagged_mut()) },
                None => unsafe{ self.root.map(|mut ptr| ptr.as_mut().make_mut()) }
            }
        }
        #[inline]
        pub fn top_mut(&mut self) -> Option<TaggedNodeRefMut<'_, V, A>> {
            match self.stack.last() {
                Some(node_ptr) => unsafe{ Some(node_ptr.into_tagged_mut()) },
                None => unsafe{ self.root.map(|mut ptr| ptr.as_mut().make_mut()) }
            }
        }
        #[inline]
        pub fn root_mut(&mut self) -> Option<&mut TrieNodeODRc<V, A>> {
            if self.stack.is_empty() {
                self.root.map(|mut root| unsafe{ root.as_mut() })
            } else {
                None
            }
        }
        #[inline]
        pub unsafe fn root_unchecked(&self) -> &TrieNodeODRc<V, A> {
            debug_assert_eq!(self.stack.len(), 0);
            self.root.map(|root| unsafe{ root.as_ref() }).unwrap()
        }
        #[inline]
        pub fn take_root(&mut self) -> Option<&'a mut TrieNodeODRc<V, A>> {
            self.to_root();
            self.root.take().map(|mut root| unsafe{ root.as_mut() })
        }
        #[inline]
        pub fn replace_root(&mut self, root: &'a mut TrieNodeODRc<V, A>) {
            debug_assert_eq!(self.stack.len(), 0); //panic!("Illegal operation, unable to replace borrowed root");
            self.root = Some(NonNull::from(root));
        }
        #[inline]
        pub fn to_root(&mut self) {
            self.stack.clear();
        }
        #[inline]
        pub fn depth(&self) -> usize {
            self.stack.len() + 1
        }
        #[inline]
        pub fn advance<'r, F>(&mut self, step_f: F) -> bool
            where
            'a: 'r,
            F: FnOnce(&'r mut TaggedNodeRefMut<'a, V, A>) -> Option<TaggedNodeRefMut<'a, V, A>>,
        {
            let mut old_top_ref = self.top_mut().unwrap();

            //SAFETY: The `MutNodeStack` type ensures that the mutably borrowed stack frames aren't
            // accessible.  See the `mutcursor` crate for a more thorough discussion on this pattern
            // and why it's safe.
            let borrowed_top = unsafe{ core::mem::transmute(&mut old_top_ref) };

            match step_f(borrowed_top) {
                Some(new_ref) => {
                    self.stack.push(new_ref.into());
                    true
                },
                None => false
            }
        }
        #[inline]
        pub fn backtrack(&mut self) {
            self.stack.pop();
        }
        #[inline]
        pub fn try_backtrack_node(&mut self) {
            if self.stack.len() > 0 {
                self.backtrack()
            }
        }
    }
}


#[cfg(all(test, feature = "pathmap-internal-tests"))]
mod tests {
    use super::ring::AlgebraicStatus;
    use super::trie_map::*;
    use super::utils::ByteMask;
    use super::zipper::{*, zipper_priv::*};
    use super::trie_core::node::*;
use super::trie_core::r#ref::{TrieRef, TrieRefOwned};
    use super::alloc::GlobalAlloc;

    #[test]
    fn write_zipper_set_val_test1() {
        let mut map = PathMap::<usize>::new();
        let mut zipper = map.write_zipper_at_path(b"in");
        for i in 0usize..32 {
            zipper.descend_to_byte(0);
            zipper.descend_to(i.to_be_bytes());
            zipper.set_val(i);
            zipper.reset();
        }
        drop(zipper);

        // for (k, v) in map.iter() {
        //     println!("{:?} {v}", k);
        // }

        let mut zipper = map.read_zipper_at_path(b"in\0");
        for i in 0usize..32 {
            zipper.descend_to(i.to_be_bytes());
            assert_eq!(zipper.path_exists(), true);
            assert_eq!(*zipper.get_val().unwrap(), i);
            zipper.reset();
        }
        drop(zipper);
    }

    /// Hits an edge case, where we want to ensure that the zipper's root gets mended
    /// (with `mend_root`) when a node is upgraded to a different node type
    #[test]
    fn write_zipper_set_val_test2() {
        let mut map = PathMap::<()>::new();

        //We want to ensure we get a PairNode at the map root
        map.set_val_at(b"OnePath", ());
        map.set_val_at(b"2Path", ());
        #[cfg(not(feature = "all_dense_nodes"))]
        assert!(map.root().unwrap().as_tagged().as_list().is_some());

        let mut wz = map.write_zipper_at_path(b"3Path");
        assert_eq!(wz.is_val(), false);

        //Now force the node to upgrade with set_val
        assert_eq!(wz.set_val(()), None);
        assert_eq!(wz.is_val(), true);
        assert_eq!(wz.get_val_mut(), Some(&mut ()));
    }

    /// Hits an edge case around fixing a WriteZipper's root (with `mend_root`)
    #[test]
    fn write_zipper_set_val_test3() {
        let mut map = PathMap::<()>::new();

        //We want to ensure we get a PairNode at the map root
        map.set_val_at(b"aaa111", ());
        map.set_val_at(b"bbb", ());
        map.set_val_at(b"aaa222", ());

        let mut wz = map.write_zipper_at_path(b"aaa");
        assert_eq!(wz.is_val(), false);

        //Now force the node to upgrade with set_val
        assert_eq!(wz.set_val(()), None);
        assert_eq!(wz.is_val(), true);
        assert_eq!(wz.get_val_mut(), Some(&mut ()));
    }

    #[test]
    fn write_zipper_root_value_test() {
        let mut map = PathMap::<usize>::new();
        let mut zipper = map.write_zipper();

        assert_eq!(zipper.is_val(), false);
        assert_eq!(zipper.val(), None);
        assert_eq!(zipper.get_val_mut(), None);

        assert_eq!(zipper.set_val(42), None);
        assert_eq!(zipper.is_val(), true);
        assert_eq!(zipper.val(), Some(&42));
        assert_eq!(zipper.get_val_mut().unwrap(), &42);

        *zipper.get_val_mut().unwrap() = 1337;
        assert_eq!(zipper.val(), Some(&1337));

        assert_eq!(zipper.remove_val(true), Some(1337));
        assert_eq!(zipper.is_val(), false);
        assert_eq!(zipper.val(), None);
        assert_eq!(zipper.get_val_mut(), None);
    }

    #[test]
    fn write_zipper_get_val_or_set_test() {
        let mut map = PathMap::<u64>::new();
        map.write_zipper_at_path(b"Drenths").get_val_or_set_mut(42);
        assert_eq!(map.get_val_at(b"Drenths"), Some(&42));

        *map.write_zipper_at_path(b"Drenths").get_val_or_set_mut(42) = 24;
        assert_eq!(map.get_val_at(b"Drenths"), Some(&24));

        let mut zipper = map.write_zipper_at_path(b"Drenths");
        *zipper.get_val_or_set_mut(42) = 0;
        assert_eq!(zipper.val(), Some(&0));
        drop(zipper);

        map.write_zipper().get_val_or_set_mut(42);
        assert_eq!(map.get_val_at([]), Some(&42));

        *map.write_zipper().get_val_or_set_mut(42) = 24;
        assert_eq!(map.get_val_at([]), Some(&24));

        let mut zipper = map.write_zipper();
        *zipper.get_val_or_set_mut(42) = 0;
        assert_eq!(zipper.val(), Some(&0))
    }

    #[test]
    fn write_zipper_iter_copy_test() {
        const N: usize = 32;

        let mut map = PathMap::<usize>::new();
        let mut zipper = map.write_zipper_at_path(b"in\0");
        for i in 0..N {
            zipper.descend_to(i.to_be_bytes());
            zipper.set_val(i);
            zipper.reset();
        }
        drop(zipper);

        let zipper_head = map.zipper_head();
        {
            let mut sanity_counter = 0;
            let mut writer_z = unsafe{ zipper_head.write_zipper_at_exclusive_path_unchecked(b"out\0") };
            let mut reader_z = unsafe{ zipper_head.read_zipper_at_path_unchecked(b"in\0") };
            let witness = reader_z.witness();
            while let Some(val) = reader_z.to_next_get_val_with_witness(&witness) {
                writer_z.descend_to(reader_z.path());
                writer_z.set_val(*val * 65536);
                writer_z.reset();
                sanity_counter += 1;
            }
            assert_eq!(sanity_counter, N);
        }
        drop(zipper_head);

        assert_eq!(map.val_count(), N*2);
        let mut in_path = b"in\0".to_vec();
        let mut out_path = b"out\0".to_vec();
        for i in 0..N {
            in_path.truncate(3);
            in_path.extend(i.to_be_bytes());
            assert_eq!(map.get_val_at(&in_path), Some(&i));
            out_path.truncate(4);
            out_path.extend(i.to_be_bytes());
            assert_eq!(map.get_val_at(&out_path), Some(&(i * 65536)));
        }
    }

    #[test]
    fn write_zipper_graft_test1() {
        let a_keys = ["arrow", "bow", "cannon", "roman", "romane", "romanus", "romulus", "rubens", "ruber", "rubicon", "rubicundus", "rom'i"];
        let mut a: PathMap<i32> = a_keys.iter().enumerate().map(|(i, k)| (k, i as i32)).collect();

        let b_keys = ["ad", "d", "ll", "of", "om", "ot", "ugh", "und"];
        let b: PathMap<i32> = b_keys.iter().enumerate().map(|(i, k)| (k, (i + 1000) as i32)).collect();

        let mut wz = a.write_zipper_at_path(b"ro");
        let rz = b.read_zipper();
        wz.graft(&rz);
        drop(wz);

        //Test that the original keys were left alone, above the graft point
        assert_eq!(a.get_val_at(b"arrow").unwrap(), &0);
        assert_eq!(a.get_val_at(b"bow").unwrap(), &1);
        assert_eq!(a.get_val_at(b"cannon").unwrap(), &2);

        //Test that the pruned keys are gone
        assert_eq!(a.get_val_at(b"roman"), None);
        assert_eq!(a.get_val_at(b"romulus"), None);
        assert_eq!(a.get_val_at(b"rom'i"), None);

        //More keys after but above the graft point weren't harmed
        assert_eq!(a.get_val_at(b"rubens").unwrap(), &7);
        assert_eq!(a.get_val_at(b"ruber").unwrap(), &8);
        assert_eq!(a.get_val_at(b"rubicundus").unwrap(), &10);

        //And test that the new keys were grafted into place
        assert_eq!(a.get_val_at(b"road").unwrap(), &1000);
        assert_eq!(a.get_val_at(b"rod").unwrap(), &1001);
        assert_eq!(a.get_val_at(b"roll").unwrap(), &1002);
        assert_eq!(a.get_val_at(b"roof").unwrap(), &1003);
        assert_eq!(a.get_val_at(b"room").unwrap(), &1004);
        assert_eq!(a.get_val_at(b"root").unwrap(), &1005);
        assert_eq!(a.get_val_at(b"rough").unwrap(), &1006);
        assert_eq!(a.get_val_at(b"round").unwrap(), &1007);
    }

    /// Tests to make sure graft doesn't create aliasing by accident 
    #[test]
    fn write_zipper_graft_test2() {
        let mut src = PathMap::<()>::new();
        let mut dst = PathMap::<()>::new();
        src.set_val_at(b"one:val", ());
        src.set_val_at(b"one:two:val", ());
        src.set_val_at(b"one:two:three:val", ());

        let mut wz = dst.write_zipper_at_path(b"one:");
        let mut rz = src.read_zipper();
        rz.descend_to(b"one:");
        wz.graft(&rz);
        drop(wz);

        assert_eq!(dst.get_val_at(b"one:two:val"), Some(&()));
        assert_eq!(src.get_val_at(b"one:two:junk"), None);

        let zh = dst.zipper_head();
        let mut wz = zh.write_zipper_at_exclusive_path(b"one:").unwrap();
        wz.descend_to(b"two:junk");
        wz.set_val(());
        drop(wz);
        drop(zh);

        assert_eq!(dst.get_val_at(b"one:two:junk"), Some(&()));
        assert_eq!(src.get_val_at(b"one:two:junk"), None);
    }

    #[test]
    fn write_zipper_join_into_test1() {
        let a_keys = ["arrow", "bow", "cannon", "roman", "romane", "romanus", "romulus", "rubens", "ruber", "rubicon", "rubicundus", "rom'i"];
        let mut a: PathMap<u64> = a_keys.iter().enumerate().map(|(i, k)| (k, i as u64)).collect();
        assert_eq!(a.val_count(), 12);

        let b_keys = ["road", "rod", "roll", "roof", "room", "root", "rough", "round"];
        let b: PathMap<u64> = b_keys.iter().enumerate().map(|(i, k)| (k, (i + 1000) as u64)).collect();
        assert_eq!(b.val_count(), 8);

        let mut wz = a.write_zipper_at_path(b"ro");
        let mut rz = b.read_zipper();
        rz.descend_to(b"ro");
        wz.join_into(&rz);
        drop(wz);

        //Test that the original keys were left alone, above the graft point
        assert_eq!(a.val_count(), 20);
        assert_eq!(a.get_val_at(b"arrow").unwrap(), &0);
        assert_eq!(a.get_val_at(b"bow").unwrap(), &1);
        assert_eq!(a.get_val_at(b"cannon").unwrap(), &2);

        //Test that the blended downstream keys are still there
        assert_eq!(a.get_val_at(b"roman").unwrap(), &3);
        assert_eq!(a.get_val_at(b"romulus").unwrap(), &6);
        assert_eq!(a.get_val_at(b"rom'i").unwrap(), &11);

        //More keys after but above the graft point weren't harmed
        assert_eq!(a.get_val_at(b"rubens").unwrap(), &7);
        assert_eq!(a.get_val_at(b"ruber").unwrap(), &8);
        assert_eq!(a.get_val_at(b"rubicundus").unwrap(), &10);

        //And test that the new keys were grafted into place
        assert_eq!(a.get_val_at(b"road").unwrap(), &1000);
        assert_eq!(a.get_val_at(b"rod").unwrap(), &1001);
        assert_eq!(a.get_val_at(b"roll").unwrap(), &1002);
        assert_eq!(a.get_val_at(b"roof").unwrap(), &1003);
        assert_eq!(a.get_val_at(b"room").unwrap(), &1004);
        assert_eq!(a.get_val_at(b"root").unwrap(), &1005);
        assert_eq!(a.get_val_at(b"rough").unwrap(), &1006);
        assert_eq!(a.get_val_at(b"round").unwrap(), &1007);
    }

    /// Tests how `join_into` handles dangling path arguments (no values, just path structure)
    #[test]
    fn write_zipper_join_into_test2() {
        // Test 1: join_into with read zipper at a dangling path
        let mut btm: PathMap<()> = PathMap::new();
        btm.create_path(&[1, 255, 0]);
        let zh = btm.zipper_head();

        let mut wz = zh.write_zipper_at_exclusive_path(&[0, 255, 0]).unwrap();
        wz.create_path();
        let rz = zh.read_zipper_at_path(&[1, 255, 0]).unwrap();
        let alg_result = wz.join_into(&rz);
        assert_eq!(alg_result, AlgebraicStatus::None); // Both zippers are at dangling paths
        drop(wz);
        drop(rz);
        drop(zh);

        // Verify both dangling paths exist but no values
        assert_eq!(btm.get_val_at(&[0, 255, 0]), None);
        assert_eq!(btm.get_val_at(&[1, 255, 0]), None);

        // Test 2: join_into to move some dangling paths
        let mut btm2: PathMap<()> = PathMap::new();
        btm2.create_path(&[1, 255, 0]);
        btm2.create_path(&[1, 255, 1]);
        btm2.create_path(&[1, 200, 5]);
        let zh2 = btm2.zipper_head();

        let mut wz = zh2.write_zipper_at_exclusive_path(&[0]).unwrap();
        let rz = zh2.read_zipper_at_path(&[1]).unwrap();
        let alg_result = wz.join_into(&rz);
        assert_eq!(alg_result, AlgebraicStatus::Element); // We created some new dangling paths

        drop(wz);
        drop(rz);
        drop(zh2);

        // Verify the structure was joined but no values exist
        assert_eq!(btm2.path_exists_at(&[0, 255, 0]), true);
        assert_eq!(btm2.path_exists_at(&[0, 255, 1]), true);
        assert_eq!(btm2.path_exists_at(&[0, 200, 5]), true);
        assert_eq!(btm2.get_val_at(&[0, 255, 0]), None);
        assert_eq!(btm2.get_val_at(&[0, 255, 1]), None);
        assert_eq!(btm2.get_val_at(&[0, 200, 5]), None);
        assert_eq!(btm2.get_val_at(&[1, 255, 0]), None);
        let rz = btm2.read_zipper();
        assert_eq!(rz.child_count(), 2); // Should have both [0] and [1] branches
    }

    #[test]
    fn write_zipper_join_into_take_test1() {
        let keys = ["a:arrow", "a:bow", "a:cannon", "a:roman", "a:romane", "a:romanus", "a:romulus", "a:rubens", "a:ruber", "a:rubicon", "a:rubicundus", "a:rom'i",
            "b:road", "b:rod", "b:roll", "b:roof", "b:room", "b:root", "b:rough", "b:round"];
        let mut map: PathMap<u64> = keys.iter().enumerate().map(|(i, k)| (k, i as u64)).collect();
        assert_eq!(map.val_count(), 20);

        assert_eq!(map.val_count(), 20);
        assert_eq!(map.get_val_at(b"a:arrow").unwrap(), &0);
        assert_eq!(map.get_val_at(b"a:bow").unwrap(), &1);
        assert_eq!(map.get_val_at(b"a:cannon").unwrap(), &2);
        assert_eq!(map.get_val_at(b"a:roman").unwrap(), &3);
        assert_eq!(map.get_val_at(b"a:romulus").unwrap(), &6);
        assert_eq!(map.get_val_at(b"a:rom'i").unwrap(), &11);
        assert_eq!(map.get_val_at(b"a:rubens").unwrap(), &7);
        assert_eq!(map.get_val_at(b"a:ruber").unwrap(), &8);
        assert_eq!(map.get_val_at(b"a:rubicundus").unwrap(), &10);
        assert_eq!(map.get_val_at(b"b:road").unwrap(), &12);
        assert_eq!(map.get_val_at(b"b:rod").unwrap(), &13);
        assert_eq!(map.get_val_at(b"b:roll").unwrap(), &14);
        assert_eq!(map.get_val_at(b"b:roof").unwrap(), &15);
        assert_eq!(map.get_val_at(b"b:room").unwrap(), &16);
        assert_eq!(map.get_val_at(b"b:root").unwrap(), &17);
        assert_eq!(map.get_val_at(b"b:rough").unwrap(), &18);
        assert_eq!(map.get_val_at(b"b:round").unwrap(), &19);

        let head = map.zipper_head();
        let mut a = head.write_zipper_at_exclusive_path(b"a:").unwrap();
        let mut b = head.write_zipper_at_exclusive_path(b"b:").unwrap();
        assert_eq!(a.val_count(), 12);
        assert_eq!(b.val_count(), 8);

        a.join_into_take(&mut b, true);
        assert_eq!(a.val_count(), 20);
        assert_eq!(b.val_count(), 0);

        drop(a);
        drop(b);
        drop(head);

        //Test the keys are where we expect them to be, and not where they should not be
        assert_eq!(map.val_count(), 20);
        assert_eq!(map.get_val_at(b"a:arrow").unwrap(), &0);
        assert_eq!(map.get_val_at(b"a:bow").unwrap(), &1);
        assert_eq!(map.get_val_at(b"a:cannon").unwrap(), &2);
        assert_eq!(map.get_val_at(b"a:roman").unwrap(), &3);
        assert_eq!(map.get_val_at(b"a:romulus").unwrap(), &6);
        assert_eq!(map.get_val_at(b"a:rom'i").unwrap(), &11);
        assert_eq!(map.get_val_at(b"a:rubens").unwrap(), &7);
        assert_eq!(map.get_val_at(b"a:ruber").unwrap(), &8);
        assert_eq!(map.get_val_at(b"a:rubicundus").unwrap(), &10);
        assert_eq!(map.get_val_at(b"a:road").unwrap(), &12);
        assert_eq!(map.get_val_at(b"a:rod").unwrap(), &13);
        assert_eq!(map.get_val_at(b"a:roll").unwrap(), &14);
        assert_eq!(map.get_val_at(b"a:roof").unwrap(), &15);
        assert_eq!(map.get_val_at(b"a:room").unwrap(), &16);
        assert_eq!(map.get_val_at(b"a:root").unwrap(), &17);
        assert_eq!(map.get_val_at(b"a:rough").unwrap(), &18);
        assert_eq!(map.get_val_at(b"a:round").unwrap(), &19);

        assert_eq!(map.get_val_at(b"b:road"), None);
        assert_eq!(map.get_val_at(b"b:round"), None);
    }

    /// Tests how `join_into_take` handles dangling path arguments with prune parameter
    #[test]
    fn write_zipper_join_into_take_test2() {
        // Test 1: Join and take dangling paths from [1] into [0] with prune=true
        let mut btm: PathMap<()> = PathMap::new();
        btm.create_path(&[1, 255, 0]);
        btm.create_path(&[1, 255, 1]);
        let zh = btm.zipper_head();

        let mut wz = zh.write_zipper_at_exclusive_path(&[0, 255]).unwrap();
        let mut src_wz = zh.write_zipper_at_exclusive_path(&[1]).unwrap();
        src_wz.descend_to_byte(255);
        let alg_result = wz.join_into_take(&mut src_wz, true);
        assert_eq!(alg_result, AlgebraicStatus::Element); // All paths are dangling
        drop(wz);
        zh.cleanup_write_zipper(src_wz);
        drop(zh);

        // Dangling paths should be moved to [0] and removed from [1]
        assert_eq!(btm.path_exists_at(&[1]), false);
        assert_eq!(btm.path_exists_at(&[0, 255, 0]), true);
        assert_eq!(btm.path_exists_at(&[0, 255, 1]), true);
        assert_eq!(btm.get_val_at(&[0, 255, 0]), None);
        assert_eq!(btm.get_val_at(&[0, 255, 1]), None);
        let rz = btm.read_zipper();
        assert_eq!(rz.child_count(), 1);
        assert_eq!(rz.child_mask(), ByteMask::from(0));

        // Test 2: with prune=false to leave dangling paths
        let mut btm2: PathMap<()> = PathMap::new();
        btm2.create_path(&[1, 255, 0]);
        let zh2 = btm2.zipper_head();

        let mut wz = zh2.write_zipper_at_exclusive_path(&[0, 255]).unwrap();
        let mut src_wz = zh2.write_zipper_at_exclusive_path(&[1]).unwrap();
        src_wz.descend_to_byte(255);
        let alg_result = wz.join_into_take(&mut src_wz, false);
        assert_eq!(alg_result, AlgebraicStatus::Element);
        drop(wz);
        zh2.cleanup_write_zipper(src_wz);
        drop(zh2);

        assert_eq!(btm2.path_exists_at(&[1, 255, 0]), false);
        assert_eq!(btm2.path_exists_at(&[1, 255]), true);
        assert_eq!(btm2.path_exists_at(&[0, 255, 0]), true);
        assert_eq!(btm2.get_val_at(&[0, 255, 0]), None);
        let rz = btm2.read_zipper();
        assert_eq!(rz.child_count(), 2);
        assert_eq!(rz.child_mask(), ByteMask::from_iter([0, 1]));
    }

    #[test]
    fn write_zipper_meet_into_test1() {
        let a_keys = ["12345", "1aaaa", "1bbbb", "1cccc", "1dddd"];
        let b_keys = ["12345", "1zzzz"];
        let a: PathMap<()> = a_keys.iter().map(|k| (k, ())).collect();
        let mut b: PathMap<()> = b_keys.iter().map(|k| (k, ())).collect();

        let az = a.read_zipper();
        assert_eq!(az.val_count(), a_keys.len());

        //Test an Element result
        let mut bz = b.write_zipper();
        assert_eq!(bz.val_count(), b_keys.len());
        let result = bz.meet_into(&az, true);
        assert_eq!(result, AlgebraicStatus::Element);
        assert_eq!(bz.val_count(), 1);
        bz.descend_to("12345");
        assert!(bz.path_exists());
        assert_eq!(bz.val(), Some(&()));

        //Test an Identity result
        let b_keys = ["12345"];
        let mut b: PathMap<()> = b_keys.iter().map(|k| (k, ())).collect();
        let mut bz = b.write_zipper();
        let result = bz.meet_into(&az, true);
        assert_eq!(result, AlgebraicStatus::Identity);
        assert_eq!(bz.val_count(), 1);
        bz.descend_to("12345");
        assert!(bz.path_exists());
        assert_eq!(bz.val(), Some(&()));

        //Test a None result
        let a_keys = ["1aaaa", "1bbbb", "1cccc", "1dddd"];
        let a: PathMap<()> = a_keys.iter().map(|k| (k, ())).collect();
        let az = a.read_zipper();
        assert_eq!(az.val_count(), a_keys.len());
        bz.reset();
        let result = bz.meet_into(&az, true);
        assert_eq!(result, AlgebraicStatus::None);
        assert_eq!(bz.child_count(), 0);
    }

    /// This tests a ByteNode meeting a ListNode through a WriteZipper positioned above the root.  This is
    /// designed to shake out bugs in the abstract-meet function, such as the bug where the `ListNode` `self`
    /// was a perfect subset of the `DenseNode` `other`, but the `COUNTER_IDENT` flag was set in error.
    #[test]
    fn write_zipper_meet_into_test2() {
        let a_keys = [
            vec![193, 11],
            vec![194, 1, 0],
            vec![194, 2, 5],
            vec![194, 3, 2],
            vec![194, 5, 8],
            vec![194, 6, 4],
            vec![194, 7, 63],
            vec![194, 7, 160],
            vec![194, 7, 161],
            vec![194, 7, 162],
            vec![194, 7, 163],
            vec![194, 7, 164],
        ];
        let b_keys = [
            vec![194, 7, 163, 194, 4, 160],
            vec![194, 7, 163, 194, 7, 162],
            vec![194, 7, 163, 194, 7, 163],
            vec![194, 7, 163, 194, 8, 0],
        ];

        let a: PathMap<()> = a_keys.iter().map(|k| (k, ())).collect();
        let mut b: PathMap<()> = b_keys.iter().map(|k| (k, ())).collect();

        let rz = a.read_zipper();

        let mut wz = b.write_zipper();
        wz.descend_to([194, 7, 163]);

        //Create enough peer branches that we can be reasonably sure we're a byte node now
        // and then clean them up
        wz.descend_to([0, 0, 0, 0]);
        wz.set_val(());
        wz.ascend(4);
        wz.descend_to([1, 0, 0, 1]);
        wz.set_val(());
        wz.ascend(4);
        wz.descend_to([2, 0, 0, 2]);
        wz.set_val(());
        wz.ascend(4);
        wz.descend_to([0, 0, 0, 0]);
        wz.remove_val(true);
        wz.ascend(4);
        wz.descend_to([1, 0, 0, 1]);
        wz.remove_val(true);
        wz.ascend(4);
        wz.descend_to([2, 0, 0, 2]);
        wz.remove_val(true);
        wz.ascend(4);

        wz.meet_into(&rz, true);

        assert_eq!(wz.val_count(), 2);
        wz.descend_to([194, 7, 162]);
        assert!(wz.path_exists());
        assert!(wz.val().is_some());
        assert!(wz.ascend(3));
        wz.descend_to([194, 7, 163]);
        assert!(wz.path_exists());
        assert!(wz.val().is_some());
    }

    /// Tests whether the [WriteZipper::meet_into] operation will do the right thing with the root value
    #[test]
    fn write_zipper_meet_into_test3() {
        //Validate meet with empty clears the root val
        let mut map = PathMap::new();
        map.insert(b"b", ());
        map.insert(b"a", ());
        map.insert(b"", ());
        let empty_map = PathMap::new();
        assert_eq!(map.write_zipper().meet_into(&empty_map.read_zipper(), true), AlgebraicStatus::None);
        assert_eq!(map.iter().count(), 0);

        //Validate meet with identity leaves the root val alone
        let mut map = PathMap::new();
        map.insert(b"b", ());
        map.insert(b"a", ());
        map.insert(b"", ());
        let ident_map = map.clone();
        assert_eq!(map.write_zipper().meet_into(&ident_map.read_zipper(), true), AlgebraicStatus::Identity);
        assert_eq!(map.iter().count(), 3);

        //Validate meet with just_root keeps the root val and removes the rest
        let mut map = PathMap::new();
        map.insert(b"b", ());
        map.insert(b"a", ());
        map.insert(b"", ());
        let mut just_root_map = PathMap::new();
        just_root_map.insert(b"", ());
        assert_eq!(map.write_zipper().meet_into(&just_root_map.read_zipper(), true), AlgebraicStatus::Element);
        assert_eq!(map.iter().count(), 1);

        //Validate meet with all_but_root removes the root
        let mut map = PathMap::new();
        map.insert(b"b", ());
        map.insert(b"a", ());
        map.insert(b"", ());
        let mut all_but_root_map = PathMap::new();
        all_but_root_map.insert(b"b", ());
        all_but_root_map.insert(b"a", ());
        assert_eq!(map.write_zipper().meet_into(&all_but_root_map.read_zipper(), true), AlgebraicStatus::Element);
        assert_eq!(map.iter().count(), 2);
    }

    /// Tests how `meet_into` handles dangling path arguments (no values, just path structure)
    #[test]
    fn write_zipper_meet_into_test4() {
        // Test 1: meet_into at leaf level with both paths dangling
        let mut btm: PathMap<()> = PathMap::new();
        btm.create_path(&[1, 255, 0]);
        btm.create_path(&[0, 255, 0]);
        btm.create_path(&[0, 255, 1]);
        let zh = btm.zipper_head();

        let mut wz = zh.write_zipper_at_exclusive_path(&[0, 255, 0]).unwrap();
        let rz = zh.read_zipper_at_path(&[1, 255, 0]).unwrap();
        let alg_result = wz.meet_into(&rz, true);
        assert_eq!(alg_result, AlgebraicStatus::None); // We're at the end of two dangling paths
        drop(wz);
        drop(rz);

        // Test 2: meet where read zipper is at a non-existent path
        let mut wz = zh.write_zipper_at_exclusive_path(&[0, 255, 1]).unwrap();
        let rz = zh.read_zipper_at_path(&[1, 255, 1]).unwrap();
        let alg_result = wz.meet_into(&rz, true);
        assert_eq!(alg_result, AlgebraicStatus::None);
        zh.cleanup_write_zipper(wz);
        drop(rz);
        drop(zh);

        // Verify the wz's path was pruned, but the other paths are left alone
        assert_eq!(btm.path_exists_at(&[0, 255, 1]), false);
        assert_eq!(btm.path_exists_at(&[1, 255, 0]), true);
        assert_eq!(btm.path_exists_at(&[0, 255, 0]), true);

        // Test 3: meet from a higher level with all dangling paths and prune=true
        let mut btm2: PathMap<()> = PathMap::new();
        btm2.create_path(&[0, 255, 0]);
        btm2.create_path(&[0, 255, 1]);
        btm2.create_path(&[0, 200, 5]);
        btm2.create_path(&[1, 255, 0]);
        let zh2 = btm2.zipper_head();

        let mut wz = zh2.write_zipper_at_exclusive_path(&[0]).unwrap();
        let rz = zh2.read_zipper_at_path(&[1]).unwrap();
        let alg_result = wz.meet_into(&rz, true);
        assert_eq!(alg_result, AlgebraicStatus::Element);
        drop(wz);
        drop(rz);
        drop(zh2);

        // Verify the meet operation did what it should have
        assert_eq!(btm2.path_exists_at(&[1, 255, 0]), true);
        assert_eq!(btm2.path_exists_at(&[0, 255, 0]), true);
        assert_eq!(btm2.path_exists_at(&[0, 200, 5]), false);
        assert_eq!(btm2.path_exists_at(&[0, 255, 1]), false);
    }

    /// Tests whether the [WriteZipper::subtract_into] operation will do the right thing with the root value
    #[test]
    fn write_zipper_subtract_into_test1() {
        //Validate subtract of identity clears the root val
        let mut map = PathMap::new();
        map.insert(b"b", ());
        map.insert(b"a", ());
        map.insert(b"", ());
        let ident_map = map.clone();
        assert_eq!(map.write_zipper().subtract_into(&ident_map.read_zipper(), true), AlgebraicStatus::None);
        assert_eq!(map.iter().count(), 0);

        //Validate subtract of empty keeps the root val
        let mut map = PathMap::new();
        map.insert(b"b", ());
        map.insert(b"a", ());
        map.insert(b"", ());
        let empty_map = PathMap::new();
        assert_eq!(map.write_zipper().subtract_into(&empty_map.read_zipper(), true), AlgebraicStatus::Identity);
        assert_eq!(map.iter().count(), 3);

        //Validate subtract of just_root clears the root val
        let mut map = PathMap::new();
        map.insert(b"b", ());
        map.insert(b"a", ());
        map.insert(b"", ());
        let mut just_root_map = PathMap::new();
        just_root_map.insert(b"", ());
        assert_eq!(map.write_zipper().subtract_into(&just_root_map.read_zipper(), true), AlgebraicStatus::Element);
        assert_eq!(map.iter().count(), 2);

        //Validate subtract of all_but_root keeps it
        let mut map = PathMap::new();
        map.insert(b"b", ());
        map.insert(b"a", ());
        map.insert(b"", ());
        let mut all_but_root_map = PathMap::new();
        all_but_root_map.insert(b"b", ());
        all_but_root_map.insert(b"a", ());
        assert_eq!(map.write_zipper().subtract_into(&all_but_root_map.read_zipper(), true), AlgebraicStatus::Element);
        assert_eq!(map.iter().count(), 1);
    }

    /// Tests how `subtract_into` handles dangling paths, including situations with extraneous empty nodes hanging around
    #[test]
    fn write_zipper_subtract_into_test2() {
        let mut btm: PathMap<()> = PathMap::new();
        btm.insert([1, 255, 0], ());
        let zh = btm.zipper_head();

        //Make a single value, at the root of a shared path
        // then do a subtract, which should result in the removal of the value
        let mut wz = zh.write_zipper_at_exclusive_path(&[0, 255, 0]).unwrap();
        wz.set_val(());
        let rz = zh.read_zipper_at_path(&[1, 255, 0]).unwrap();
        let alg_result = wz.subtract_into(&rz, true);
        assert_eq!(alg_result, AlgebraicStatus::None);
        drop(wz);
        drop(rz);

        //Now, recreate the value, but do a subtract from another zipper higher in the trie
        // this should result in the removal of the whole [0, ...] path
        let mut wz = zh.write_zipper_at_exclusive_path(&[0, 255, 0]).unwrap();
        wz.set_val(());
        drop(wz);

        let mut wz = zh.write_zipper_at_exclusive_path(&[0]).unwrap();
        let rz = zh.read_zipper_at_path(&[1]).unwrap();
        let alg_result = wz.subtract_into(&rz, true);
        assert_eq!(alg_result, AlgebraicStatus::None);
        zh.cleanup_write_zipper(wz);
        drop(rz);

        drop(zh);
        assert_eq!(btm.read_zipper().child_count(), 1);
        assert_eq!(btm.read_zipper().child_mask(), ByteMask::from(1));
    }

    /// Tests `subtract_into` interactions specifically involving Dangling Paths
    #[test]
    fn write_zipper_subtract_into_test3() {
        // Case 1: Value - Dangling Path (Same Path)
        // Subtracting a path with NO value from a path WITH a value should change nothing.
        {
            let mut map: PathMap<()> = PathMap::new();
            map.insert(b"a", ());
            let mut sub: PathMap<()> = PathMap::new();
            sub.create_path(b"a");

            let mut wz = map.write_zipper();
            let rz = sub.read_zipper();
            wz.descend_to(b"a");
            assert_eq!(wz.subtract_into(&rz, true), AlgebraicStatus::Identity);
            drop(wz);
            assert!(map.get_val_at(b"a").is_some());
        }

        // Case 2: Dangling Path - Value (Same Path)
        // Subtracting a value from a dangling path should result in a removed path.
        {
            let mut map: PathMap<()> = PathMap::new();
            map.create_path(b"b");
            let mut sub: PathMap<()> = PathMap::new();
            sub.insert(b"b", ());

            let mut wz = map.write_zipper();
            let rz = sub.read_zipper();
            assert_eq!(wz.subtract_into(&rz, true), AlgebraicStatus::None);
            drop(wz);
            assert!(!map.path_exists_at(b"b"));
        }

        // Case 3: Dangling Path - Dangling Path (Same Path)
        // Subtracting a dangling path from a dangling path should result in a removed path.
        // (Empty - Empty -> Empty -> Prune)
        {
            let mut map: PathMap<()> = PathMap::new();
            map.create_path(b"c");
            let mut sub: PathMap<()> = PathMap::new();
            sub.create_path(b"c");

            let mut wz = map.write_zipper();
            let rz = sub.read_zipper();
            assert_eq!(wz.subtract_into(&rz, true), AlgebraicStatus::None);
            drop(wz);
            assert!(!map.path_exists_at(b"c"));
        }

        // Case 4: Dangling Path - Deep Value
        // Subtracting a deep value from a shallow dangling path.
        // 'map' has no value at 'd'. 'sub' has value at 'd/sub'.
        // The result at 'd' is still no value, so it should be pruned.
        {
            let mut map: PathMap<()> = PathMap::new();
            map.create_path(b"d");
            let mut sub: PathMap<()> = PathMap::new();
            sub.insert(b"d/sub", ());

            let mut wz = map.write_zipper();
            let rz = sub.read_zipper();
            assert_eq!(wz.subtract_into(&rz, true), AlgebraicStatus::None);
            drop(wz);
            assert!(!map.path_exists_at(b"d"));
        }

        // Case 5: Deep Value - Dangling Path
        // Subtracting a shallow dangling path from a deep value.
        // 'map' has value at 'e/sub'. 'sub' has dangling path at 'e'.
        // The dangling path at 'e' has no value to subtract from 'map's structure.
        {
            let mut map: PathMap<()> = PathMap::new();
            map.insert(b"e/sub", ());
            let mut sub: PathMap<()> = PathMap::new();
            sub.create_path(b"e");

            let mut wz = map.write_zipper();
            let rz = sub.read_zipper();
            assert_eq!(wz.subtract_into(&rz, true), AlgebraicStatus::Identity);
            drop(wz);
            assert!(map.get_val_at(b"e/sub").is_some());
        }
    }

    /// Exercises a mixed sequence of joins, meets, and subtraction
    ///
    /// With `n=200` and keys `"k_0"`, `"k_1"`, etc., this operation
    /// sequence builds internal trie shapes that route through
    /// compressed `LineListNode` handling during subtraction.
    #[test]
    fn write_zipper_subtract_into_test4() {
        let n = 200u64;

        // Build 8 maps with keys "k_{j*step}" for step 1..8
        let maps: Vec<PathMap<u64>> = (0..8).map(|i| {
            let mut m = PathMap::<u64>::new();
            let step = (i + 1) as u64;
            for j in 0..n {
                let k = j * step;
                m.set_val_at(format!("k_{}", k).as_bytes(), k);
            }
            m
        }).collect();

        // Chain of operations using only public zipper API:
        // ((((A|B) & C) | D) & E) | F) & G) \ H
        let mut r = maps[0].clone();
        r.write_zipper().join_into(&maps[1].read_zipper());       // A | B
        r.write_zipper().meet_into(&maps[2].read_zipper(), true); // & C
        r.write_zipper().join_into(&maps[3].read_zipper());       // | D
        r.write_zipper().meet_into(&maps[4].read_zipper(), true); // & E
        r.write_zipper().join_into(&maps[5].read_zipper());       // | F
        r.write_zipper().meet_into(&maps[6].read_zipper(), true); // & G
        r.write_zipper().subtract_into(&maps[7].read_zipper(), true); // \ H

        // Sanity check: result should be non-empty.
        let mut rz = r.read_zipper();
        let mut count = 0;
        use super::zipper::ZipperIteration;
        while rz.to_next_val() { count += 1; }
        assert!(count > 0);
    }

    /// Tests how `restrict` handles dangling path arguments (no values, just path structure)
    #[test]
    fn write_zipper_restrict_test2() {
        // Test 1: Restrict with dangling paths in both write and read zippers
        let mut btm: PathMap<()> = PathMap::new();
        btm.create_path(&[1, 255, 0]);
        btm.create_path(&[0, 255, 0]);
        btm.create_path(&[0, 255, 1]);
        btm.create_path(&[0, 200, 5]);
        let zh = btm.zipper_head();

        // Restrict [0] subtree to paths that exist in [1] subtree (all dangling)
        // NOTE: For now, it's values and not dangling paths that define which paths should be included,
        // but we'll likely revisit this when restrict becomes a meet policy
        let mut wz = zh.write_zipper_at_exclusive_path(&[0]).unwrap();
        let rz = zh.read_zipper_at_path(&[1]).unwrap();
        let alg_result = wz.restrict(&rz);
        assert_eq!(alg_result, AlgebraicStatus::None);
        drop(wz);
        drop(rz);
        drop(zh);

        // Test 2: restrict at leaf level with dangling paths
        // NOTE: Once again it's values and not dangling paths that inform restrict, but this will probably
        // be changed in the future
        let mut btm2: PathMap<()> = PathMap::new();
        btm2.create_path(&[0, 255, 0]);
        btm2.create_path(&[1, 255, 0]);
        let zh2 = btm2.zipper_head();

        let mut wz = zh2.write_zipper_at_exclusive_path(&[0, 255, 0]).unwrap();
        let rz = zh2.read_zipper_at_path(&[1, 255, 0]).unwrap();
        let alg_result = wz.restrict(&rz);
        assert_eq!(alg_result, AlgebraicStatus::None); // Both dangling
        drop(wz);
        drop(rz);
        drop(zh2);

        // Test 3: restrict where read zipper has no matching paths - should remove everything
        let mut btm3: PathMap<()> = PathMap::new();
        btm3.create_path(&[0, 255, 0]);
        btm3.create_path(&[0, 255, 1]);
        btm3.create_path(&[1, 200, 5]);
        let zh3 = btm3.zipper_head();

        let mut wz = zh3.write_zipper_at_exclusive_path(&[0]).unwrap();
        let rz = zh3.read_zipper_at_path(&[1]).unwrap();
        let alg_result = wz.restrict(&rz);
        assert_eq!(alg_result, AlgebraicStatus::None); // No matching structure
        zh3.cleanup_write_zipper(wz);
        drop(rz);
        drop(zh3);

        // All paths under [0] should be removed since [1] has no matching structure
        assert_eq!(btm3.path_exists_at(&[0, 255]), false);
        assert_eq!(btm3.path_exists_at(&[0]), false);
        assert_eq!(btm3.path_exists_at(&[1, 200, 5]), true);
        let rz = btm3.read_zipper();
        assert_eq!(rz.child_count(), 1);
        assert_eq!(rz.child_mask(), ByteMask::from(1));
    }

    #[test]
    fn write_zipper_movement_test() {
        let keys = ["romane", "romanus", "romulus", "rubens", "ruber", "rubicon", "rubicundus", "rom'i"];
        let mut map: PathMap<u64> = keys.iter().enumerate().map(|(i, k)| (k, i as u64)).collect();

        let mut wz = map.write_zipper_at_path(b"ro");
        assert_eq!(wz.child_count(), 1);
        wz.descend_to(b"manus");
        assert!(wz.path_exists());
        assert_eq!(wz.path(), b"manus");
        assert_eq!(wz.child_count(), 0);
        wz.reset();
        assert_eq!(wz.path(), b"");
        assert_eq!(wz.child_count(), 1);
        wz.descend_to(b"mulus");
        assert!(wz.path_exists());
        assert_eq!(wz.path(), b"mulus");
        assert_eq!(wz.child_count(), 0);
        assert!(wz.ascend_until());
        assert_eq!(wz.path(), b"m");
        assert_eq!(wz.child_count(), 3);

        //Make sure we can't ascend above the zipper's root with ascend_until
        assert!(wz.ascend_until());
        assert_eq!(wz.path(), b"");
        assert!(!wz.ascend_until());

        //Test step-wise `ascend`
        wz.descend_to(b"manus");
        assert_eq!(wz.path(), b"manus");
        assert_eq!(wz.ascend(1), true);
        assert_eq!(wz.path(), b"manu");
        assert_eq!(wz.ascend(5), false);
        assert_eq!(wz.path(), b"");
        assert_eq!(wz.at_root(), true);
        wz.descend_to(b"mane");
        assert_eq!(wz.path(), b"mane");
        assert_eq!(wz.ascend(3), true);
        assert_eq!(wz.path(), b"m");
        assert_eq!(wz.child_count(), 3);
    }

    #[test]
    fn write_zipper_compound_join_test() {
        let mut map = PathMap::<u64>::new();

        let b_keys = ["alligator", "giraffe", "gazelle", "gadfly"];
        let b: PathMap<u64> = b_keys.iter().enumerate().map(|(i, k)| (k, i as u64)).collect();

        let mut wz = map.write_zipper();
        let mut rz = b.read_zipper();
        rz.descend_to(b"alli");
        wz.graft(&rz);
        rz.reset();
        assert_eq!(wz.join_into(&rz), AlgebraicStatus::Element);
        drop(wz);

        assert_eq!(map.val_count(), 5);
        let values: Vec<String> = map.iter().map(|(path, _)| String::from_utf8_lossy(&path[..]).to_string()).collect();
        assert_eq!(values, vec!["alligator", "gadfly", "gator", "gazelle", "giraffe"]);
    }

    #[test]
    fn write_zipper_remove_branches_test() {
        let keys = ["arrow", "bow", "cannon", "roman", "romane", "romanus", "romulus", "rubens", "ruber", "rubicon", "rubicundus", "rom'i",
            "abcdefghijklmnopqrstuvwxyz"];
        let mut map: PathMap<i32> = keys.iter().enumerate().map(|(i, k)| (k, i as i32)).collect();

        let mut wz = map.write_zipper_at_path(b"roman");
        wz.remove_branches(true);
        drop(wz);

        //Test that the original keys were left alone, above the graft point
        assert_eq!(map.get_val_at(b"arrow").unwrap(), &0);
        assert_eq!(map.get_val_at(b"bow").unwrap(), &1);
        assert_eq!(map.get_val_at(b"cannon").unwrap(), &2);
        assert_eq!(map.get_val_at(b"rom'i").unwrap(), &11);

        //Test that the value is ok
        assert_eq!(map.get_val_at(b"roman").unwrap(), &3);

        //Test that the pruned keys are gone
        assert_eq!(map.get_val_at(b"romane"), None);
        assert_eq!(map.get_val_at(b"romanus"), None);

        let mut wz = map.write_zipper();
        wz.descend_to(b"ro");
        assert!(wz.path_exists());
        wz.remove_branches(true);
        assert!(!wz.path_exists());
        drop(wz);

        let mut wz = map.write_zipper();
        wz.descend_to(b"abcdefghijklmnopq");
        assert!(wz.path_exists());
        assert_eq!(wz.path(), b"abcdefghijklmnopq");
        wz.remove_branches(true);
        assert!(!wz.path_exists());
        assert_eq!(wz.path(), b"abcdefghijklmnopq");
        drop(wz);

        assert!(!map.path_exists_at(b"abcdefghijklmnopq"));
        assert!(!map.path_exists_at(b"abc"));
    }

    #[test]
    fn write_zipper_drop_head_test1() {
        let keys = [
            "123:abc:Bob",
            "123:def:Jim",
            "123:ghi:Pam",
            "123:jkl:Sue",
            "123:dog:Bob:Fido",
            "123:cat:Jim:Felix",
            "123:dog:Pam:Bandit",
            "123:owl:Sue:Cornelius"];
        let mut map: PathMap<u64> = keys.iter().enumerate().map(|(i, k)| (k, i as u64)).collect();
        let mut wz = map.write_zipper_at_path(b"123:");

        wz.join_k_path_into(4, true);
        drop(wz);

        let ref_keys: Vec<&[u8]> = vec![
            b"123:Bob",
            b"123:Bob:Fido",
            b"123:Jim",
            b"123:Jim:Felix",
            b"123:Pam",
            b"123:Pam:Bandit",
            b"123:Sue",
            b"123:Sue:Cornelius"];
        assert_eq!(map.iter().map(|(k, _v)| k).collect::<Vec<Vec<u8>>>(), ref_keys);
    }

    #[test]
    fn write_zipper_drop_head_long_key_test1() {

        //A single long key
        let key = b"12345678901234567890123456789012345678901234567890";
        let mut map = PathMap::<u64>::new();
        map.set_val_at(key, 42);
        for i in 0..key.len() {
            assert_eq!(map.get_val_at(&key[i..]), Some(&42));
            let mut wz = map.write_zipper();
            wz.join_k_path_into(1, true);
        }

        //A slightly more complicated tree
        let keys: Vec<&[u8]> = vec![
            b"12345678901234567890123456789012345678901234567890",
            b"12345ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrs",
            b"1234567890FGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrs",
            b"123456789012345KLMNOPQRSTUVWXYZabcdefghijklmnopqrs",
            b"12345678901234567890PQRSTUVWXYZabcdefghijklmnopqrs",
            b"1234567890123456789012345UVWXYZabcdefghijklmnopqrs",
            b"123456789012345678901234567890Zabcdefghijklmnopqrs",
            b"12345678901234567890123456789012345efghijklmnopqrs",
            b"1234567890123456789012345678901234567890jklmnopqrs",
            b"123456789012345678901234567890123456789012345opqrs", ];
        let mut map: PathMap<u64> = keys.iter().enumerate().map(|(i, k)| (k, i as u64)).collect();
        for i in 0..keys[0].len() {
            assert_eq!(map.get_val_at(&keys[0][i..]), Some(&0));
            if i < 45 {
                assert_eq!(map.get_val_at(&keys[9][i..]), Some(&9));
            }
            if i > 10 {
                assert_eq!(map.val_count(), 11-(i/5));
            }
            let mut wz = map.write_zipper();
            wz.join_k_path_into(1, true);
        }
    }

    #[test]
    fn write_zipper_drop_head_test2() {
        let keys: Vec<Vec<u8>> = vec![
            vec![1, 2, 4, 65, 2, 42, 237, 3, 1, 173, 165, 3, 16, 200, 213, 4, 0, 166, 47, 81, 4, 0, 167, 216, 181, 4, 6, 125, 178, 225, 4, 6, 142, 119, 117, 4, 64, 232, 214, 129, 4, 65, 128, 13, 13, 4, 65, 144],
            vec![1, 2, 4, 69, 2, 13, 183],
        ];
        let mut map: PathMap<u64> = keys.iter().enumerate().map(|(i, k)| (k, i as u64)).collect();
        let mut wz = map.write_zipper_at_path(&[1]);
        wz.join_k_path_into(3, true);
        drop(wz);

        assert_eq!(map.get_val_at(&vec![1, 2, 42, 237, 3, 1, 173, 165, 3, 16, 200, 213, 4, 0, 166, 47, 81, 4, 0, 167, 216, 181, 4, 6, 125, 178, 225, 4, 6, 142, 119, 117, 4, 64, 232, 214, 129, 4, 65, 128, 13, 13, 4, 65, 144]), Some(&0));
        assert_eq!(map.get_val_at(&vec![1, 2, 13, 183]), Some(&1));
        assert_eq!(map.val_count(), 2);

        let mut map: PathMap<u64> = keys.iter().enumerate().map(|(i, k)| (k, i as u64)).collect();
        let mut wz = map.write_zipper_at_path(&[1]);
        wz.join_k_path_into(27, true);
        drop(wz);

        assert_eq!(map.get_val_at(&vec![1, 178, 225, 4, 6, 142, 119, 117, 4, 64, 232, 214, 129, 4, 65, 128, 13, 13, 4, 65, 144]), Some(&0));
        assert_eq!(map.val_count(), 1);
    }

    #[test]
    fn write_zipper_drop_head_test3() {
        let keys = [[0, 0], [0, 1], [1, 0], [1, 1]];
        let mut map: PathMap<()> = keys.iter().map(|k| (k, ())).collect();
        let mut wz = map.write_zipper();

        wz.join_k_path_into(1, true);
        assert_eq!(wz.val_count(), 2);

        let keys = [
            [194, 11, 87, 194, 3, 165],
            [194, 11, 87, 194, 8, 218],
            [194, 11, 87, 194, 10, 156],
            [194, 11, 87, 194, 13, 138],
            [194, 11, 87, 194, 21, 128],
            [194, 11, 87, 194, 21, 132],
            [194, 17, 239, 194, 3, 165],
            [194, 17, 239, 194, 8, 218],
            [194, 17, 239, 194, 10, 156],
            [194, 17, 239, 194, 13, 138],
            [194, 17, 239, 194, 21, 128],
            [194, 17, 239, 194, 21, 132],
        ];

        let mut b: PathMap<()> = keys.iter().map(|k| (k, ())).collect();
        let mut wz = b.write_zipper();

        wz.join_k_path_into(3, true);
        assert_eq!(wz.val_count(), 6);
    }

    #[test]
    fn write_zipper_drop_head_test4() {
        let paths = [
            vec![0, 1, 0, 1, 2, 1, 6, 1, 8, 1, 12, 1, 16],
            vec![0, 1, 1, 1, 0, 2, 16, 224, 3, 0, 240, 0],
            vec![0, 1, 1, 1, 0, 3, 2, 255, 208, 4, 0, 255],
            vec![0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
            vec![0, 1, 1, 1, 2, 1, 4, 1, 12, 1, 40, 1, 116],
            vec![0, 1, 2, 1, 0, 1, 0, 2, 0, 128, 1, 0, 2, 1],
            vec![0, 1, 2, 1, 10, 1, 11, 1, 100, 2, 3, 233, 2],
            vec![0, 1, 3, 1, 21, 1, 22, 1, 23, 1, 24, 1, 25],
        ];
        let mut map: PathMap<()> = paths.iter().map(|k| (k, ())).collect();

        let mut wz = map.write_zipper();
        wz.descend_to([0, 1]);
        wz.join_k_path_into(1, true);
        assert_eq!(wz.val_count(), 8);
    }

    #[test]
    fn write_zipper_drop_head_test5() {
        let paths = [
            vec![193, 191, 193, 193, 191],
            vec![193, 191, 193, 194, 12, 28],
            vec![193, 191, 193, 194, 18, 9],
            vec![193, 191, 194, 193, 191],
            vec![193, 191, 194, 194, 12, 28],
            vec![193, 191, 194, 194, 15, 47],
            vec![193, 191, 194, 194, 18, 9],
        ];
        let mut map: PathMap<()> = paths.iter().map(|k| (k, ())).collect();

        let mut wz = map.write_zipper();
        wz.descend_to([193, 191]);
        wz.join_k_path_into(1, true);
        assert_eq!(wz.val_count(), 4);
    }

    /// Test pruning (or not pruning) behavior of `drop_head`
    #[test]
    fn write_zipper_drop_head_test6() {
        let paths = [
            vec![193, 191, 193, 193, 191],
            vec![193, 191, 193, 194, 12, 28],
            vec![193, 191, 193, 194, 18, 9],
            vec![193, 191, 194, 193, 191],
            vec![193, 191, 194, 194, 12, 28],
            vec![193, 191, 194, 194, 15, 47],
            vec![193, 191, 194, 194, 18, 9],
        ];

        //Here, we're totally dropping the entirety of the map
        let mut map: PathMap<()> = paths.iter().map(|k| (k, ())).collect();
        let mut wz = map.write_zipper();
        wz.descend_to([193, 191]);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.join_k_path_into(4, true), false);
        assert_eq!(wz.val_count(), 0);
        wz.reset();
        assert_eq!(wz.child_mask(), ByteMask::EMPTY);
        drop(wz);

        //Here, we're keeping some dangling paths
        let mut map: PathMap<()> = paths.iter().map(|k| (k, ())).collect();
        let mut wz = map.write_zipper();
        wz.descend_to([193, 191]);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.join_k_path_into(4, false), false);
        assert_eq!(wz.val_count(), 0);
        wz.reset();
        assert_eq!(wz.child_mask(), ByteMask::from_iter([193]));
        wz.descend_to_byte(193);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.child_mask(), ByteMask::from_iter([191]));
        wz.descend_to_byte(191);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.child_mask(), ByteMask::EMPTY);
        drop(wz);
    }

    #[test]
    fn write_zipper_meet_k_path_into_test1() {
        let keys = [
            "123:abc:Bob",
            "123:abc:Jim",
            "123:abc:Pam",
            "123:abc:Sue",
            "123:def:Nan",
            "123:def:Mel",
            "123:def:Bob",
            "123:def:Sue"];
        let mut map: PathMap<()> = keys.iter().map(|k| (k, ())).collect();
        let mut wz = map.write_zipper_at_path(b"123:");

        wz.meet_k_path_into(4, true);

        assert_eq!(map.val_count(), 2);
        assert_eq!(map.get(b"123:Bob"), Some(&()));
        assert_eq!(map.get(b"123:Sue"), Some(&()));
    }

    #[test]
    fn write_zipper_meet_k_path_into_test2() {
        // keys := {foo, bar}.e0 \/ {foo, cux, baz}.e1 \/ {cux}.e2
        let keys = [
            b"123:foo:e0",
            b"123:foo:e1",
            b"123:bar:e0",
            b"123:cux:e1",
            b"123:cux:e2",
            b"123:baz:e1"].map(|e| e.as_slice());
        let mut map: PathMap<()> = PathMap::from_iter(keys);
        let mut wz = map.write_zipper_at_path(b"123:");

        // /\(keys <| {foo, bar}) == {e0}
        wz.restrict(&PathMap::from_iter([&b"foo"[..], &b"bar"[..]]).into_read_zipper(&[]));
        wz.meet_k_path_into(4, true);
        drop(wz);
        assert_eq!(map.val_count(), 1);
        assert_eq!(map.get(b"123:e0"), Some(&()));
    }

    #[test]
    fn write_zipper_insert_prefix_test() {
        let keys = [
            "123:Bob:Fido",
            "123:Jim:Felix",
            "123:Pam:Bandit",
            "123:Sue:Cornelius"];
        let mut map: PathMap<u64> = keys.iter().enumerate().map(|(i, k)| (k, i as u64)).collect();
        let mut wz = map.write_zipper_at_path(b"123:");

        wz.insert_prefix(b"pet:");
        drop(wz);

        // let paths: Vec<String> = map.iter().map(|(k, _)| String::from_utf8_lossy(&k[..]).to_string()).collect();
        let ref_keys: Vec<&[u8]> = vec![
            b"123:pet:Bob:Fido",
            b"123:pet:Jim:Felix",
            b"123:pet:Pam:Bandit",
            b"123:pet:Sue:Cornelius"];
        assert_eq!(map.iter().map(|(k, _v)| k).collect::<Vec<Vec<u8>>>(), ref_keys);

        // Test that drop_head undoes insert_prefix
        let mut wz = map.write_zipper();
        wz.insert_prefix(b"people:");
        //let paths: Vec<String> = map.iter().map(|(k, _)| String::from_utf8_lossy(&k[..]).to_string()).collect();
        wz.join_k_path_into(b"people:".len(), true);
        drop(wz);

        assert_eq!(map.iter().map(|(k, _v)| k).collect::<Vec<Vec<u8>>>(), ref_keys);
    }

    #[test]
    fn write_zipper_remove_prefix_test() {
        let keys = [
            "123:Bob.Fido",
            "123:Jim.Felix",
            "123:Pam.Bandit",
            "123:Sue.Cornelius"];

        //Test where we don't bottom-out the zipper
        let mut map: PathMap<u64> = keys.iter().enumerate().map(|(i, k)| (k, i as u64)).collect();
        let mut wz = map.write_zipper_at_path(b"123");

        wz.descend_to(b":Pam");
        assert_eq!(wz.remove_prefix(4), true);
        drop(wz);

        assert_eq!(map.val_count(), 1);
        assert_eq!(map.get_val_at(b"123.Bandit"), Some(&2));

        //Test where we *do* exactly bottom-out the zipper
        let mut map: PathMap<u64> = keys.iter().enumerate().map(|(i, k)| (k, i as u64)).collect();
        let mut wz = map.write_zipper_at_path(b"123:");

        wz.descend_to(b"Pam.");
        assert_eq!(wz.remove_prefix(4), true);
        drop(wz);

        assert_eq!(map.val_count(), 1);
        assert_eq!(map.get_val_at(b"123:Bandit"), Some(&2));

        //Now test where we crash into the bottom of the zipper
        let mut map: PathMap<u64> = keys.iter().enumerate().map(|(i, k)| (k, i as u64)).collect();
        let mut wz = map.write_zipper_at_path(b"123:");

        wz.descend_to(b"Pam.");
        assert_eq!(wz.remove_prefix(9), false);
        drop(wz);

        assert_eq!(map.val_count(), 1);
        assert_eq!(map.get_val_at(b"123:Bandit"), Some(&2));
    }

    #[test]
    fn write_zipper_map_test() {
        let keys = ["arrow", "bow", "cannon", "roman", "romane", "romanus", "romulus", "rubens", "ruber", "rubicon", "rubicundus", "rom'i"];
        let mut map: PathMap<u64> = keys.iter().enumerate().map(|(i, k)| (k, i as u64)).collect();

        let mut wr = map.write_zipper();
        wr.descend_to(b"rom");
        let sub_map = wr.take_map(true).unwrap();
        drop(wr);

        let sub_map_keys: Vec<String> = sub_map.iter().map(|(k, _v)| String::from_utf8_lossy(&k).to_string()).collect();
        assert_eq!(sub_map_keys, ["'i", "an", "ane", "anus", "ulus"]);
        let map_keys: Vec<String> = map.iter().map(|(k, _v)| String::from_utf8_lossy(&k).to_string()).collect();
        assert_eq!(map_keys, ["arrow", "bow", "cannon", "rubens", "ruber", "rubicon", "rubicundus"]);

        let mut wr = map.write_zipper();
        wr.descend_to(b"c");
        wr.join_map_into(sub_map);
        drop(wr);

        let map_keys: Vec<String> = map.iter().map(|(k, _v)| String::from_utf8_lossy(&k).to_string()).collect();
        assert_eq!(map_keys, ["arrow", "bow", "c'i", "can", "cane", "cannon", "canus", "culus", "rubens", "ruber", "rubicon", "rubicundus"]);
    }

    #[test]
    fn write_zipper_mask_children_and_values() {
        let keys = ["arrow", "bow", "cannon", "roman", "romane", "romanus", "romulus", "rubens", "ruber", "rubicon", "rubicundus", "rom'i",
            "abcdefghijklmnopqrstuvwxyz"];
        let mut map: PathMap<i32> = keys.iter().enumerate().map(|(i, k)| (k, i as i32)).collect();

        let mut wr = map.write_zipper();

        let mut m = [0, 0, 0, 0];
        for b in "abc".bytes() { m[((b & 0b11000000) >> 6) as usize] |= 1u64 << (b & 0b00111111); }
        wr.remove_unmasked_branches(m.into(), true);
        drop(wr);

        let result = map.iter().map(|(k, _v)| String::from_utf8_lossy(&k).to_string()).collect::<Vec<_>>();

        assert_eq!(result, ["abcdefghijklmnopqrstuvwxyz", "arrow", "bow", "cannon"]);
    }

    #[test]
    fn write_zipper_mask_children_and_values_at_path() {
        let keys = [
            "123:abc:Bob",
            "123:def:Jim",
            "123:ghi:Pam",
            "123:jkl:Sue",
            "123:dog:Bob:Fido",
            "123:cat:Jim:Felix",
            "123:dog:Pam:Bandit",
            "123:owl:Sue:Cornelius"];
        let mut map: PathMap<u64> = keys.iter().enumerate().map(|(i, k)| (k, i as u64)).collect();

        let mut wr = map.write_zipper();
        wr.descend_to("123:".as_bytes());
        // println!("{:?}", wr.child_mask());

        let mut m = [0, 0, 0, 0];
        for b in "dco".bytes() { m[((b & 0b11000000) >> 6) as usize] |= 1u64 << (b & 0b00111111); }
        wr.remove_unmasked_branches(m.into(), true);
        m = [0, 0, 0, 0];
        wr.descend_to("d".as_bytes());
        for b in "o".bytes() { m[((b & 0b11000000) >> 6) as usize] |= 1u64 << (b & 0b00111111); }
        wr.remove_unmasked_branches(m.into(), true);
        drop(wr);

        let result = map.iter().map(|(k, _v)| String::from_utf8_lossy(&k).to_string()).collect::<Vec<_>>();
        assert_eq!(result, [
            "123:cat:Jim:Felix",
            "123:dog:Bob:Fido",
            "123:dog:Pam:Bandit",
            "123:owl:Sue:Cornelius"]);

        let keys = [
            "a1",
            "a2",
            "a1a",
            "a1b",
            "a1a1",
            "a1a2",
            "a1a1a",
            "a1a1b"];
        let mut map: PathMap<u64> = keys.iter().enumerate().map(|(i, k)| (k, i as u64)).collect();
        let mut wr = map.write_zipper_at_path(b"a1");
        // println!("{:?}", wr.child_mask());

        m = [0, 0, 0, 0];
        for b in "b".bytes() { m[((b & 0b11000000) >> 6) as usize] |= 1u64 << (b & 0b00111111); }
        wr.remove_unmasked_branches(m.into(), true);
        drop(wr);

        let result = map.iter().map(|(k, _v)| String::from_utf8_lossy(&k).to_string()).collect::<Vec<_>>();
        assert_eq!(result, [
            "a1",
            "a1b",
            "a2"]);
    }

    #[test]
    fn write_zipper_remove_unmask_branches() {
        let keys = ["Wilson", "Taft", "Roosevelt", "McKinley", "Cleveland", "Harrison", "Arthur", "Garfield"];
        let mut map: PathMap<u64> = keys.iter().enumerate().map(|(i, k)| (k, i as u64)).collect();

        let mut wr = map.write_zipper();
        wr.remove_unmasked_branches([0xFF, !(1<<(b'M'-64)), 0xFF, 0xFF].into(), true);
        //McKinley didn't make it
        wr.descend_to("McKinley");
        assert_eq!(wr.val(), None);

        wr.reset();
        wr.descend_to("Roos");
        assert_eq!(wr.path_exists(), true);
        wr.remove_unmasked_branches([0xFF, !(1<<(b'i'-64)), 0xFF, 0xFF].into(), true);
        //Missed Roosevelt
        wr.descend_to("evelt");
        assert_eq!(wr.val(), Some(&2));

        wr.reset();
        wr.descend_to("Garf");
        assert_eq!(wr.path_exists(), true);
        wr.remove_unmasked_branches([0xFF, !(1<<(b'i'-64)), 0xFF, 0xFF].into(), true);
        wr.descend_to("ield");
        //Garfield was removed
        assert_eq!(wr.val(), None);
    }
    #[test]
    fn write_zipper_test_zipper_conversion() {
        let keys = [
            "123:dog:Bob:Fido",
            "123:cat:Jim:Felix",
            "123:dog:Pam:Bandit",
            "123:owl:Sue:Cornelius"];
        let mut map: PathMap<u64> = keys.iter().enumerate().map(|(i, k)| (k, i as u64)).collect();

        // Simplistic test where the WZ is untracker, created with a statically safe method
        let mut wz = map.write_zipper_at_path(b"12");
        assert_eq!(wz.path(), b"");
        wz.descend_to(b"3:");
        assert_eq!(wz.path(), b"3:");

        let mut rz = wz.into_read_zipper();
        assert_eq!(rz.path(), b"3:");
        rz.reset();
        rz.descend_to(b"3:dog:");
        assert_eq!(rz.path_exists(), true);
        assert_eq!(rz.child_count(), 2);
        drop(rz);

        // ZipperHead test, to make sure the tracker is doing the right thing when converted
        let zh = map.zipper_head();
        let mut wz = zh.write_zipper_at_exclusive_path(b"12").unwrap();
        assert_eq!(wz.path(), b"");
        wz.descend_to(b"3:");
        assert_eq!(wz.path(), b"3:");

        let mut rz = wz.into_read_zipper();
        assert_eq!(rz.path(), b"3:");
        rz.reset();
        rz.descend_to(b"3:dog:");
        assert_eq!(rz.path_exists(), true);
        assert_eq!(rz.child_count(), 2);

        assert!(zh.write_zipper_at_exclusive_path(b"1").is_err());
        assert!(zh.write_zipper_at_exclusive_path(b"12").is_err());
        assert!(zh.write_zipper_at_exclusive_path(b"123").is_err());

        let mut rz2 = zh.read_zipper_at_borrowed_path(b"1").unwrap();
        assert_eq!(rz2.path(), b"");
        rz2.descend_to(b"23:dog:");
        assert_eq!(rz2.path_exists(), true);
        assert_eq!(rz.child_count(), 2);

        let rz3 = zh.read_zipper_at_borrowed_path(b"123:").unwrap();
        assert_eq!(rz3.child_count(), 3);
    }

    #[test]
    fn write_zipper_join_results_test1() {
        let mut map = PathMap::<bool>::new();
        let head = map.zipper_head();

        // Empty \/-> Empty should be `None`
        let mut wz = head.write_zipper_at_exclusive_path(b"dst:").unwrap();
        let rz = head.read_zipper_at_path(b"src:").unwrap();
        assert_eq!(wz.join_into(&rz), AlgebraicStatus::None);
        drop(wz);
        drop(rz);

        // Something \/-> Empty should be `Element`
        let mut wz = head.write_zipper_at_exclusive_path(b"src:").unwrap();
        wz.descend_to_byte(b'A');
        wz.set_val(true);
        drop(wz);
        let mut wz = head.write_zipper_at_exclusive_path(b"dst:").unwrap();
        let rz = head.read_zipper_at_path(b"src:").unwrap();
        assert_eq!(wz.join_into(&rz), AlgebraicStatus::Element);
        assert_eq!(wz.join_into(&rz), AlgebraicStatus::Identity); //Subsequent call should be `Identity`
        drop(wz);
        drop(rz);

        // [A] \/-> [A] should be `Identity`
        let mut wz = head.write_zipper_at_exclusive_path(b"dst:").unwrap();
        let rz = head.read_zipper_at_path(b"src:").unwrap();
        assert_eq!(wz.join_into(&rz), AlgebraicStatus::Identity);
        drop(wz);
        drop(rz);

        // [A, B] \/-> [A] should be `Element`
        let mut wz = head.write_zipper_at_exclusive_path(b"src:").unwrap();
        wz.descend_to_byte(b'B');
        wz.set_val(true);
        drop(wz);
        let mut wz = head.write_zipper_at_exclusive_path(b"dst:").unwrap();
        let rz = head.read_zipper_at_path(b"src:").unwrap();
        assert_eq!(wz.join_into(&rz), AlgebraicStatus::Element);
        assert_eq!(wz.join_into(&rz), AlgebraicStatus::Identity); //Subsequent call should be `Identity`
        drop(wz);
        drop(rz);

        // [B] \/-> [A, B] should be `Identity`
        let mut wz = head.write_zipper_at_exclusive_path(b"src:").unwrap();
        wz.descend_to_byte(b'A');
        wz.remove_val(true);
        drop(wz);
        let mut wz = head.write_zipper_at_exclusive_path(b"dst:").unwrap();
        let rz = head.read_zipper_at_path(b"src:").unwrap();
        assert_eq!(wz.join_into(&rz), AlgebraicStatus::Identity);
        drop(wz);
        drop(rz);

        // [C, D] \/-> [A, B] should be `Element`
        let mut wz = head.write_zipper_at_exclusive_path(b"src:").unwrap();
        wz.remove_branches(true);
        wz.descend_to_byte(b'C');
        wz.set_val(true);
        wz.ascend_byte();
        wz.descend_to_byte(b'D');
        wz.set_val(true);
        drop(wz);
        let mut wz = head.write_zipper_at_exclusive_path(b"dst:").unwrap();
        let rz = head.read_zipper_at_path(b"src:").unwrap();
        assert_eq!(wz.join_into(&rz), AlgebraicStatus::Element);
        assert_eq!(wz.join_into(&rz), AlgebraicStatus::Identity); //Subsequent call should be `Identity`
        drop(wz);
        drop(rz);

        // [Carousel] \/-> [A, B, C, D] should be `Element`
        let mut wz = head.write_zipper_at_exclusive_path(b"src:").unwrap();
        wz.remove_branches(true);
        wz.descend_to(b"Carousel");
        wz.set_val(true);
        drop(wz);
        let mut wz = head.write_zipper_at_exclusive_path(b"dst:").unwrap();
        let rz = head.read_zipper_at_path(b"src:").unwrap();
        assert_eq!(wz.join_into(&rz), AlgebraicStatus::Element);
        assert_eq!(wz.join_into(&rz), AlgebraicStatus::Identity); //Subsequent call should be `Identity`
        drop(wz);
        drop(rz);

        // Empty \/-> Something should be `Identity`
        let mut wz = head.write_zipper_at_exclusive_path(b"src:").unwrap();
        wz.remove_branches(true);
        drop(wz);
        let mut wz = head.write_zipper_at_exclusive_path(b"dst:").unwrap();
        let rz = head.read_zipper_at_path(b"src:").unwrap();
        assert_eq!(wz.join_into(&rz), AlgebraicStatus::Identity);
        drop(wz);
        drop(rz);
    }

    /// Tests correctness of the `origin_path` for a WriteZipper off a map
    #[test]
    fn origin_path_test1() {
        let mut map = PathMap::<()>::new();
        let mut wz = map.write_zipper_at_path(b"This path can take you anywhere.  Just close your eyes...");

        assert_eq!(wz.path(), b"");
        assert_eq!(wz.origin_path(), b"This path can take you anywhere.  Just close your eyes...");
        wz.set_val(());

        wz.descend_to(b" and open your heart.");
        assert_eq!(wz.path(), b" and open your heart.");
        assert_eq!(wz.origin_path(), b"This path can take you anywhere.  Just close your eyes... and open your heart.");

        wz.set_val(());
        assert_eq!(wz.origin_path(), b"This path can take you anywhere.  Just close your eyes... and open your heart.");
    }

    /// Tests the origin_path for zippers created from a ZipperHead
    #[test]
    fn origin_path_test2() {
        let mut map = PathMap::<()>::new();
        map.set_val_at(b"You can do anything with Zombocom.  The only limit is yourself.", ());
        let zh = map.zipper_head();

        // Make sure ReadZippers off a ZipperHead have the right origin_path
        let mut rz = zh.read_zipper_at_borrowed_path(b"You can do anything with Zombocom.").unwrap();
        assert_eq!(rz.path(), b"");
        assert_eq!(rz.origin_path(), b"You can do anything with Zombocom.");
        rz.descend_to(b"  The only limit is yourself.");
        assert_eq!(rz.path(), b"  The only limit is yourself.");
        assert_eq!(rz.origin_path(), b"You can do anything with Zombocom.  The only limit is yourself.");

        // Make sure WriteZippers off a ZipperHead have the right origin_path
        let mut wz = zh.write_zipper_at_exclusive_path(b"This path can take you anywhere.  Just close your eyes...").unwrap();
        assert_eq!(wz.path(), b"");
        assert_eq!(wz.origin_path(), b"This path can take you anywhere.  Just close your eyes...");
        wz.set_val(());
        wz.descend_to(b" and open your heart.");
        assert_eq!(wz.path(), b" and open your heart.");
        assert_eq!(wz.origin_path(), b"This path can take you anywhere.  Just close your eyes... and open your heart.");
        wz.set_val(());
        assert_eq!(wz.is_val(), true);
        assert_eq!(wz.origin_path(), b"This path can take you anywhere.  Just close your eyes... and open your heart.");

        // Test forking a zipper from a WriteZipper and make sure it inherits the origin_path
        wz.ascend(6);
        assert_eq!(wz.is_val(), false);
        let mut rz = wz.fork_read_zipper();
        assert_eq!(rz.path(), b"");
        assert_eq!(rz.origin_path(), b"This path can take you anywhere.  Just close your eyes... and open your ");
        assert_eq!(rz.is_val(), false);
        rz.descend_to(b"heart.");
        assert_eq!(wz.origin_path(), b"This path can take you anywhere.  Just close your eyes... and open your ");
        assert_eq!(rz.origin_path(), b"This path can take you anywhere.  Just close your eyes... and open your heart.");
        assert_eq!(rz.is_val(), true);
        drop(rz);
        wz.descend_to(b"heart.");
        assert_eq!(wz.is_val(), true);

        // Test converting a WriteZipper into a ReadZipper
        let mut rz = wz.into_read_zipper();
        assert_eq!(rz.is_val(), true);
        assert_eq!(rz.path(), b" and open your heart.");
        assert_eq!(rz.origin_path(), b"This path can take you anywhere.  Just close your eyes... and open your heart.");
        rz.ascend(6);
        assert_eq!(rz.path(), b" and open your ");
        assert_eq!(rz.origin_path(), b"This path can take you anywhere.  Just close your eyes... and open your ");
        assert_eq!(rz.is_val(), false);
        rz.reset();
        assert_eq!(rz.path(), b"");
        assert_eq!(rz.origin_path(), b"This path can take you anywhere.  Just close your eyes...");
        assert_eq!(rz.is_val(), true);
    }

    #[test]
    fn write_zipper_prune_path_test1() {
        let mut map = PathMap::<()>::new();
        map.set_val_at([196, 34, 48, 48, 34, 2, 193, 44, 3, 195, 118, 97, 108, 192, 192, 2, 193, 44, 3, 202, 115, 119, 97, 112, 101, 100, 45, 118, 97, 108, 3, 195, 118, 97, 108, 128, 129, 3, 195, 118, 97, 108, 129, 128], ());
        map.set_val_at([196, 34, 48, 49, 34, 2, 193, 44, 3, 195, 118, 97, 108, 192, 192, 2, 193, 44, 3, 196, 112, 97, 105, 114, 128, 129], ());
        let mut wz = map.write_zipper();

        //Sanity checking
        wz.descend_to([196, 34, 48, 48, 34, 2, 193, 44, 3, 195, 118, 97, 108, 192, 192, 2, 193, 44, 3, 202, 115, 119, 97, 112, 101, 100, 45, 118, 97, 108, 3, 195, 118, 97, 108, 128, 129, 3, 195, 118, 97, 108, 129, 128]);
        assert_eq!(wz.is_val(), true);
        assert_eq!(wz.path_exists(), true);

        //Now delete one of the paths
        wz.remove_val(true); //This remove should already perform a prune
        assert_eq!(wz.is_val(), false);
        assert_eq!(wz.path_exists(), false);

        //Validate that it pruned up to the shared branching point
        wz.move_to_path([196, 34, 48,]);
        assert_eq!(wz.is_val(), false);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.child_count(), 1);
        assert_eq!(wz.child_mask(), ByteMask::from(49));

        //Move back to the deleted branch
        wz.move_to_path([196, 34, 48, 48, 34, 2, 193, 44, 3, 195, 118, 97, 108, 192, 192, 2, 193, 44, 3, 202, 115, 119, 97, 112, 101, 100, 45, 118, 97, 108, 3, 195, 118, 97, 108, 128, 129, 3, 195, 118, 97, 108, 129, 128]);
        assert_eq!(wz.is_val(), false);
        assert_eq!(wz.path_exists(), false);

        //Now run `prune_path`.  This `prune_path` should do nothing
        wz.z.prune_path();
        drop(wz);

        //Now we should still see the branch we didn't prune
        assert_eq!(map.val_count(), 1);
        let mut rz = map.read_zipper();
        rz.descend_to([196, 34, 48, 49, 34, 2, 193, 44, 3, 195, 118, 97, 108, 192, 192, 2, 193, 44, 3, 196, 112, 97, 105, 114, 128, 129]);
        assert_eq!(rz.is_val(), true);
        assert_eq!(rz.path_exists(), true);
        rz.move_to_path([196, 34, 48,]);
        assert_eq!(rz.is_val(), false);
        assert_eq!(rz.path_exists(), true);
        assert_eq!(rz.child_count(), 1);
        assert_eq!(rz.child_mask(), ByteMask::from(49));
    }

    /// This test exercises removing values to create dangling paths, with an emphasis on pair nodes
    #[test]
    fn write_zipper_prune_path_test2() {
        let mut map = PathMap::<()>::new();
        map.set_val_at([0], ());
        map.set_val_at([0, 0, 0], ());
        map.set_val_at([0, 0, 1, 0, 0], ());
        let mut wz = map.write_zipper();
        wz.descend_to([0, 0, 1, 0, 0]);

        //Remove a value from a path within a pair node
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.remove_val(false), Some(()));
        assert_eq!(wz.remove_val(false), None);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.prune_path(), 3);
        assert_eq!(wz.path(), &[0, 0, 1, 0, 0]);
        assert_eq!(wz.path_exists(), false);
        assert_eq!(wz.set_val(()), None);
        assert_eq!(wz.remove_val(false), Some(()));
        assert_eq!(wz.path_exists(), true);

        //Now re-add the value again at the now-dangling path
        assert_eq!(wz.set_val(()), None);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.remove_val(false), Some(()));
        assert_eq!(wz.val(), None);
        assert_eq!(wz.path_exists(), true);

        //Now try adding more stuff downstream of the dangling path
        wz.descend_to([2, 3, 4]);
        assert_eq!(wz.path_exists(), false);
        assert_eq!(wz.path(), &[0, 0, 1, 0, 0, 2, 3, 4]);
        assert_eq!(wz.path_exists(), false);
        assert_eq!(wz.set_val(()), None);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.val(), Some(&()));
        assert_eq!(wz.remove_val(false), Some(()));
        assert_eq!(wz.val(), None);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.path(), &[0, 0, 1, 0, 0, 2, 3, 4]);
        assert_eq!(wz.prune_path(), 6); //Prune back to the value at [0, 0, 0]
        assert_eq!(wz.path_exists(), false);
        assert_eq!(wz.ascend(4), true);
        assert_eq!(wz.path_exists(), false);
        assert_eq!(wz.ascend(2), true);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.path(), &[0, 0]);

        //Set up the conditions to test removing nodes from the middle of paths
        wz.descend_to([0, 1, 2]);
        assert_eq!(wz.path_exists(), false);
        assert_eq!(wz.set_val(()), None);
        wz.descend_to([3, 4]);
        assert_eq!(wz.path_exists(), false);
        assert_eq!(wz.set_val(()), None);
        assert_eq!(wz.remove_val(false), Some(()));
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.path(), &[0, 0, 0, 1, 2, 3, 4]);
        wz.reset();

        //Remove some values from the middle of paths
        wz.descend_to([0]);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.remove_val(false), Some(()));
        assert_eq!(wz.path_exists(), true);
        wz.descend_to([0]);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.remove_val(false), None);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.child_count(), 1);
        wz.descend_to([0]);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.child_count(), 1);
        assert_eq!(wz.remove_val(false), Some(()));
        assert_eq!(wz.path_exists(), true);
        wz.descend_to([1]);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.child_count(), 1);
        assert_eq!(wz.remove_val(false), None);
        assert_eq!(wz.path_exists(), true);
        wz.descend_to([2]);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.child_count(), 1);

        //Attempt a prune-remove, when there is still some upstream path
        assert_eq!(wz.remove_val(true), Some(()));
        assert_eq!(wz.path_exists(), true);
        wz.descend_to([3]);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.child_count(), 1);
        assert_eq!(wz.remove_val(true), None);
        assert_eq!(wz.path_exists(), true);
        wz.descend_to([4]);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.child_count(), 0);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.val(), None);
        wz.descend_to([5]);
        assert_eq!(wz.path_exists(), false);

        //Try pruning below the end of a dangling path and make sure it fails
        assert_eq!(wz.prune_path(), 0);

        //And try pruning above the end
        assert_eq!(wz.ascend(2), true);
        assert_eq!(wz.prune_path(), 0);
        assert_eq!(wz.descend_first_byte(), true);

        //Now validate that prune goes all the way to the root
        assert_eq!(wz.path(), &[0, 0, 0, 1, 2, 3, 4]);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.prune_path(), 7);
        assert_eq!(wz.path_exists(), false);
        assert_eq!(wz.ascend(7), true);
        assert_eq!(wz.path(), &[]);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.child_count(), 0);
        assert_eq!(wz.val(), None);
    }

    //Similar test to `write_zipper_prune_path_test2`, but designed to exercise byte nodes
    #[test]
    fn write_zipper_prune_path_test3() {
        let mut map = PathMap::<()>::new();
        map.set_val_at([0], ());
        map.set_val_at([1, 0, 0], ());
        map.set_val_at([1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], ());
        map.set_val_at([1, 0, 2], ());
        map.set_val_at([1, 0, 3], ());
        map.set_val_at([2, 0, 0], ());
        let mut wz = map.write_zipper();

        //Remove a value from a path within a byte node
        wz.descend_to([1, 0, 3]);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.remove_val(false), Some(()));
        assert_eq!(wz.remove_val(false), None);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.prune_path(), 1);
        assert_eq!(wz.path(), &[1, 0, 3]);
        assert_eq!(wz.path_exists(), false);
        assert_eq!(wz.set_val(()), None);
        assert_eq!(wz.remove_val(false), Some(()));
        assert_eq!(wz.path_exists(), true);

        //Now re-add the value again at the now-dangling path
        assert_eq!(wz.set_val(()), None);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.remove_val(false), Some(()));
        assert_eq!(wz.val(), None);
        assert_eq!(wz.path_exists(), true);

        //Now try adding more stuff downstream of the dangling path
        wz.descend_to([4, 5, 6]);
        assert_eq!(wz.path(), &[1, 0, 3, 4, 5, 6]);
        assert_eq!(wz.path_exists(), false);
        assert_eq!(wz.set_val(()), None);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.val(), Some(&()));
        assert_eq!(wz.remove_val(false), Some(()));
        assert_eq!(wz.val(), None);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.path(), &[1, 0, 3, 4, 5, 6]);
        assert_eq!(wz.prune_path(), 4); //Prune back to the value at [1, 0, 0]
        assert_eq!(wz.path_exists(), false);
        assert_eq!(wz.ascend(3), true);
        assert_eq!(wz.path_exists(), false);
        assert_eq!(wz.ascend(1), true);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.path(), &[1, 0]);
        assert_eq!(wz.child_count(), 3);

        //Set up the conditions to test removing nodes from the middle of paths
        wz.descend_to([1]);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.val(), None);
        assert_eq!(wz.set_val(()), None);
        wz.descend_to([0, 0]);
        assert_eq!(wz.set_val(()), None);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.path(), &[1, 0, 1, 0, 0]);
        wz.reset();

        //Remove some values from the middle of paths
        wz.descend_to([1, 0, 1]);
        assert_eq!(wz.remove_val(false), Some(()));
        assert_eq!(wz.path_exists(), true);
        wz.descend_to([0, 0]);
        assert_eq!(wz.remove_val(true), Some(()));
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.child_count(), 1);
        wz.descend_to([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);        assert_eq!(wz.child_count(), 0);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.val(), Some(&()));
        assert_eq!(wz.remove_val(false), Some(()));
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.prune_path(), 50);
        assert_eq!(wz.path_exists(), false);
        assert_eq!(wz.ascend(49), true);
        assert_eq!(wz.path_exists(), false);
        assert_eq!(wz.ascend_byte(), true);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.path(), &[1, 0]);
        assert_eq!(wz.child_count(), 2);

        //Make sure the `remove_val` with the `prune` flag does the right thing
        wz.descend_to_byte(0);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.prune_path(), 0);
        assert_eq!(wz.remove_val(true), Some(()));
        assert_eq!(wz.path_exists(), false);
        assert_eq!(wz.prune_path(), 0);
        assert_eq!(wz.ascend_byte(), true);
        assert_eq!(wz.child_count(), 1);
        wz.descend_to_byte(2);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.remove_val(false), Some(()));
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.prune_path(), 3);
        assert_eq!(wz.path_exists(), false);
        assert_eq!(wz.ascend(3), true);
        assert_eq!(wz.child_count(), 2);
    }

    /// This test exercises removing branches to create dangling paths, with an emphasis on pair nodes
    #[test]
    fn write_zipper_prune_path_test4() {
        let mut map = PathMap::<()>::new();
        map.set_val_at([0], ());
        map.set_val_at([0, 0, 0], ());
        map.set_val_at([0, 0, 1, 0, 0, 0, 0], ());
        let mut wz = map.write_zipper();

        //Use `remove_branches` to to cut a path within a pair node
        wz.descend_to([0, 0, 1, 0, 0]);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.remove_branches(false), true);
        assert_eq!(wz.path_exists(), true);
        wz.descend_to_byte(0);
        assert_eq!(wz.path_exists(), false);
        assert_eq!(wz.ascend_byte(), true);
        assert_eq!(wz.path_exists(), true);

        //Test `prune`
        assert_eq!(wz.prune_path(), 3);
        assert_eq!(wz.path(), &[0, 0, 1, 0, 0]);
        assert_eq!(wz.path_exists(), false);
        assert_eq!(wz.ascend(3), true);
        assert_eq!(wz.path_exists(), true);

        //Recreate some new paths, remove one and try re-extending it
        wz.descend_to([0, 0, 0, 0]);
        assert_eq!(wz.path_exists(), false);
        assert_eq!(wz.set_val(()), None);
        assert_eq!(wz.ascend(4), true);
        wz.descend_to([0, 0, 1, 0]);
        assert_eq!(wz.path_exists(), false);
        assert_eq!(wz.set_val(()), None);
        assert_eq!(wz.ascend(2), true);
        wz.descend_to_byte(0);
        assert_eq!(wz.remove_branches(false), true);
        assert_eq!(wz.path_exists(), true);
        wz.descend_to_byte(0);
        assert_eq!(wz.path_exists(), false);
        assert_eq!(wz.set_val(()), None);
        assert_eq!(wz.ascend_byte(), true);
        assert_eq!(wz.remove_branches(false), true);

        //A test for `remove_unmasked_branches`
        wz.reset();
        assert_eq!(wz.child_count(), 1);
        wz.descend_to_byte(0);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.child_count(), 1);
        wz.descend_to_byte(0);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.child_count(), 1);
        assert_eq!(wz.prune_path(), 0);
        wz.remove_unmasked_branches(ByteMask::from_iter([1u8]), false);
        assert_eq!(wz.child_count(), 0);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.prune_path(), 1);
        wz.reset();
        assert_eq!(wz.child_count(), 1);
        wz.descend_to_byte(0);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.child_count(), 0);
        assert_eq!(wz.val(), Some(&()));
        assert_eq!(wz.prune_path(), 0);
        assert_eq!(wz.remove_val(false), Some(()));
        assert_eq!(wz.prune_path(), 1);
    }

    /// This test exercises `take_node` (source for join_take, take_map, etc.) to remove branches to create dangling paths, with an emphasis on pair nodes
    #[test]
    fn write_zipper_prune_path_test5() {
        let mut src_map = PathMap::<()>::new();
        src_map.set_val_at([0], ());
        src_map.set_val_at([0, 0, 0], ());
        src_map.set_val_at([0, 0, 1, 0, 0, 0, 0], ());
        let mut src_z = src_map.write_zipper();

        //Test removing from the middle of a node
        src_z.descend_to([0, 0, 1, 0, 0]);
        assert_eq!(src_z.path_exists(), true);
        let mut dst_map = src_z.take_map(false).unwrap();
        assert_eq!(src_z.path_exists(), true);
        src_z.descend_to_byte(0);
        assert_eq!(src_z.path_exists(), false);
        assert_eq!(src_z.ascend_byte(), true);
        assert_eq!(src_z.path_exists(), true);

        //Test prune
        assert_eq!(src_z.prune_path(), 3);
        assert_eq!(src_z.path(), &[0, 0, 1, 0, 0]);
        assert_eq!(src_z.path_exists(), false);
        assert_eq!(src_z.ascend(3), true);
        assert_eq!(src_z.path_exists(), true);

        //Test removing from a node boundary
        let mut dst_z = dst_map.write_zipper();
        assert_eq!(dst_z.join_into_take(&mut src_z, false), AlgebraicStatus::Element);
        assert_eq!(src_z.path_exists(), true);
        assert_eq!(src_z.prune_path(), 1);
        assert_eq!(src_z.path_exists(), false);

        drop(src_z);
        drop(dst_z);
        assert_eq!(src_map.val_count(), 1);
        assert_eq!(dst_map.val_count(), 2);
    }

    //Similar test to `write_zipper_prune_path_test4`, but designed to exercise byte nodes
    #[test]
    fn write_zipper_prune_path_test6() {
        let mut map = PathMap::<()>::new();
        map.set_val_at([0], ());
        map.set_val_at([1, 9, 0], ());
        map.set_val_at([1, 9, 0, 9, 0], ());
        map.set_val_at([1, 9, 0, 9, 1], ());
        map.set_val_at([1, 9, 0, 9, 2], ());
        map.set_val_at([1, 9, 0, 9, 3], ());
        map.set_val_at([1, 9, 1], ());
        map.set_val_at([1, 9, 2], ());
        map.set_val_at([1, 9, 3], ());
        map.set_val_at([2, 9, 0], ());
        let mut wz = map.write_zipper();

        //Use `remove_unmaksed_branches` to chop off every part of a ByteNode
        wz.descend_to([1, 9, 0, 9]);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.child_count(), 4);
        wz.remove_unmasked_branches(ByteMask::EMPTY, false);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.child_count(), 0);
        assert_eq!(wz.prune_path(), 1);
        assert_eq!(wz.path_exists(), false);

        assert_eq!(wz.ascend_byte(), true);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.child_count(), 0);
        assert_eq!(wz.ascend_byte(), true);
        assert_eq!(wz.child_count(), 4);
        wz.remove_branches(false);
        assert_eq!(wz.child_count(), 0);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.path(), &[1, 9]);
        assert_eq!(wz.prune_path(), 2);
        assert_eq!(wz.path_exists(), false);
        assert_eq!(wz.ascend_byte(), true);
        assert_eq!(wz.path_exists(), false);
        assert_eq!(wz.ascend_byte(), true);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.child_count(), 2);
    }

    /// This test exercises `create_path`
    #[test]
    fn write_zipper_prune_path_test7() {
        let mut map = PathMap::<()>::new();
        let mut wz = map.write_zipper();

        wz.descend_to([0, 0]);
        assert_eq!(wz.path_exists(), false);
        assert_eq!(wz.set_val(()), None);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.ascend_byte(), true);

        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.create_path(), false);
        assert_eq!(wz.descend_first_byte(), true);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.create_path(), false);
        assert_eq!(wz.val(), Some(&()));
        assert_eq!(wz.descend_first_byte(), false);
        wz.descend_to_byte(0);
        assert_eq!(wz.path_exists(), false);
        assert_eq!(wz.create_path(), true);
        assert_eq!(wz.path_exists(), true);
        wz.descend_to([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
        assert_eq!(wz.path_exists(), false);
        assert_eq!(wz.create_path(), true);
        assert_eq!(wz.prune_ascend(), 57);
        assert_eq!(wz.path(), &[0, 0]);
        assert_eq!(wz.path_exists(), true);

        assert_eq!(wz.ascend_byte(), true);
        wz.descend_to_byte(0);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.create_path(), false);
        assert_eq!(wz.val(), Some(&()));
        assert_eq!(wz.ascend_byte(), true);
        wz.descend_to_byte(1);
        assert_eq!(wz.path_exists(), false);
        assert_eq!(wz.create_path(), true);
        assert_eq!(wz.ascend_byte(), true);
        wz.descend_to_byte(2);
        assert_eq!(wz.path_exists(), false);
        assert_eq!(wz.create_path(), true);
        assert_eq!(wz.ascend_byte(), true);
        wz.descend_to([3, 0, 0, 0]);
        assert_eq!(wz.path_exists(), false);
        assert_eq!(wz.create_path(), true);
        assert_eq!(wz.ascend(4), true);
        assert_eq!(wz.child_count(), 4);
    }

    /// Tests an edge case in pruning where we end up needing to prune to a point in the
    /// middle of a multi-node path
    #[test]
    fn write_zipper_prune_test8() {
        let paths = [
            "123:abc:Bob", "123:abc:Jim", "123:abc:Pam", "123:abc:Sue",
            "123:def:Bob", "123:def:Mel", "123:def:Nan", "123:def:Sue",
            "123:ghi:Jan", "123:ghi:Jen", "123:ghi:Jim", "123:ghi:Jon",
            "123:g1", "123:g2", "123:g3",
        ];
        let mut map = PathMap::<()>::new();
        for path in paths {
            map.create_path(path);
        }
        map.prune_path(b"123:g1");
        map.prune_path(b"123:g2");
        map.prune_path(b"123:g3");
        let mut wz = map.write_zipper_at_path(b"123:");

        wz.descend_to(b"abc:");
        let _ = wz.take_map(true);
        assert_eq!(wz.move_to_path(b"def:"), 0);
        assert_eq!(wz.child_mask(), ByteMask::from_iter([b'B', b'M', b'N', b'S',]));
        assert_eq!(wz.child_count(), 4);
        let _ = wz.take_map(true);
        assert_eq!(wz.child_count(), 0);
        assert_eq!(wz.child_mask(), ByteMask::EMPTY);
        assert_eq!(wz.move_to_path(b"ghi:"), 0);
        assert_eq!(wz.child_mask(), ByteMask::from(b'J'));
        assert_eq!(wz.child_count(), 1);
        let _ = wz.take_map(true);
        assert_eq!(wz.child_mask(), ByteMask::EMPTY);
        assert_eq!(wz.child_count(), 0);
    }

    /// Tests that `prune_path` won't destroy a value at the zipper's focus, when that zipper's focus is at the root
    /// of a ZipperHead's WriteZipper
    #[test]
    fn write_zipper_prune_test9() {
        let mut btm: PathMap<()> = PathMap::new();

        //Make a single value at the root of a ZipperHead's WriteZipper
        let zh = btm.zipper_head();
        let mut wz = zh.write_zipper_at_exclusive_path(&[2, 199, 116, 114, 105, 103, 103, 101, 114, 193, 120]).unwrap();
        wz.set_val(());
        drop(wz);
        drop(zh);
        let mut wz = btm.write_zipper_at_path(&[2, 199, 116, 114, 105, 103, 103, 101, 114, 193, 120]);

        //Validate that the value is where we think it is
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.is_val(), true);
        assert_eq!(wz.child_count(), 0);
        assert_eq!(wz.child_mask(), ByteMask::EMPTY);

        //Now try and prune it away, which should fail to do anything
        wz.prune_path();
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.is_val(), true);
        assert_eq!(wz.child_count(), 0);
        assert_eq!(wz.child_mask(), ByteMask::EMPTY);

        //Now remove the val, but don't prune
        wz.remove_val(false);
        assert_eq!(wz.path_exists(), true);
        assert_eq!(wz.is_val(), false);
        assert_eq!(wz.child_count(), 0);
        assert_eq!(wz.child_mask(), ByteMask::EMPTY);

        //Finally, prune again, and make sure that did what it was supposed to do
        wz.prune_path();
        assert_eq!(wz.path_exists(), false);
        assert_eq!(wz.is_val(), false);
        assert_eq!(wz.child_count(), 0);
        assert_eq!(wz.child_mask(), ByteMask::EMPTY);
    }

    /// Tests [`ZipperPriv::get_focus`] and [`ZipperPriv::try_borrow_focus`] internal APIs on [`WriteZipperCore`]
    #[test]
    fn write_zipper_focus_nodes() {
        let mut map = PathMap::<()>::new();
        map.set_val_at(b"one.val", ());
        map.set_val_at(b"one.two.val", ());
        map.set_val_at(b"one.two.three.val", ());
        map.set_val_at(b"one.two.three.four.val", ());

        //Test descending a read zipper
        let mut wz = map.write_zipper();
        wz.descend_to(b"one.");

        //We should be at a node boundary, as long as this part of the path got encoded as a PairNode (or a ByteNode)
        let node = wz.try_borrow_focus().unwrap();
        assert_eq!(node.as_tagged().count_branches(&[]), 2);
        let expected_mask: ByteMask = [b't', b'v'].into_iter().collect();
        assert_eq!(node.as_tagged().node_branches_mask(&[]), expected_mask);
        assert!(matches!(wz.get_focus(), AbstractNodeRef::BorrowedRc(_))); //Make sure we get the ODRc
        drop(wz);

        //Test creating a read zipper at the location, using `read_zipper_at_path`
        let wz = map.write_zipper_at_path(b"one.two.");

        //We should be at a node boundary, as long as this part of the path got encoded as a PairNode (or a ByteNode)
        let node = wz.try_borrow_focus().unwrap();
        assert_eq!(node.as_tagged().count_branches(&[]), 2);
        let expected_mask: ByteMask = [b't', b'v'].into_iter().collect();
        assert_eq!(node.as_tagged().node_branches_mask(&[]), expected_mask);
        assert!(matches!(wz.get_focus(), AbstractNodeRef::BorrowedRc(_))); //Make sure we get the ODRc
        drop(wz);

        //Test creating a read zipper from a ZipperHead
        let zh = map.zipper_head();
        let wz = zh.write_zipper_at_exclusive_path(b"one.two.three.").unwrap();

        // We should be at a node boundary, as long as this part of the path got encoded as a PairNode (or a ByteNode)
        let node = wz.try_borrow_focus().unwrap();
        assert_eq!(node.as_tagged().count_branches(&[]), 2);
        let expected_mask: ByteMask = [b'f', b'v'].into_iter().collect();
        assert_eq!(node.as_tagged().node_branches_mask(&[]), expected_mask);
        assert!(matches!(wz.get_focus(), AbstractNodeRef::BorrowedRc(_))); //Make sure we get the ODRc
    }

    /// Tests the zipper `val_count` method, including with root values
    const ZIPPER_VAL_COUNT_TEST1_KEYS: &[&[u8]] = &[b"", b"arrow"];
    #[test]
    fn write_zipper_val_count_test1() {
        let mut map: PathMap<()> = ZIPPER_VAL_COUNT_TEST1_KEYS.into_iter().cloned().collect();
        let mut zipper = map.write_zipper();

        assert_eq!(zipper.path(), b"");
        assert_eq!(zipper.val_count(), 2);
        assert_eq!(zipper.descend_until(), true);
        assert_eq!(zipper.path(), b"arrow");
        assert_eq!(zipper.val_count(), 1);
    }

    #[test]
    fn write_zipper_graft_child_maps_test1() {
        // Create a base map with some initial structure
        let mut map: PathMap<i32> = PathMap::new();
        map.set_val_at(b"root:a:x", 1);
        map.set_val_at(b"root:a:y", 2);
        map.set_val_at(b"root:b:x", 3);
        map.set_val_at(b"root:b:y", 4);
        map.set_val_at(b"root:c:x", 5);
        map.set_val_at(b"root:c:y", 6);
        map.set_val_at(b"root:d:x", 7);

        // Test 1: Graft child maps with remove_unset = true
        // This should replace children 'a' and 'c', and remove 'b' and 'd'
        let mut map_for_a: PathMap<i32> = PathMap::new();
        map_for_a.set_val_at(b":new_a", 10);

        let mut map_for_c: PathMap<i32> = PathMap::new();
        map_for_c.set_val_at(b":new_c", 30);

        let child_mask = ByteMask::from(b'a') | ByteMask::from(b'c');
        let maps = vec![map_for_a, map_for_c];

        let mut wz = map.write_zipper_at_path(b"root:");
        wz.graft_child_maps(child_mask, maps, true);
        drop(wz);

        // After grafting with remove_unset=true, only 'a' and 'c' branches should exist
        assert_eq!(map.get_val_at(b"root:a:new_a"), Some(&10));
        assert_eq!(map.get_val_at(b"root:c:new_c"), Some(&30));
        assert_eq!(map.get_val_at(b"root:a:x"), None);
        assert_eq!(map.get_val_at(b"root:a:y"), None);
        assert_eq!(map.get_val_at(b"root:b:x"), None);
        assert_eq!(map.get_val_at(b"root:b:y"), None);
        assert_eq!(map.get_val_at(b"root:d:x"), None);

        // Test 2: Graft child maps with remove_unset = false
        let mut map2: PathMap<i32> = PathMap::new();
        map2.set_val_at(b"root:a:old", 100);
        map2.set_val_at(b"root:b:old", 200);
        map2.set_val_at(b"root:c:old", 300);

        let mut map_for_b: PathMap<i32> = PathMap::new();
        map_for_b.set_val_at(b":new_b", 222);

        let child_mask2 = ByteMask::from(b'b');
        let maps2 = vec![map_for_b];

        let mut wz2 = map2.write_zipper_at_path(b"root:");
        wz2.graft_child_maps(child_mask2, maps2, false);
        drop(wz2);

        // After grafting with remove_unset=false, 'a' and 'c' should remain, 'b' should be replaced
        assert_eq!(map2.get_val_at(b"root:a:old"), Some(&100));
        assert_eq!(map2.get_val_at(b"root:b:old"), None);
        assert_eq!(map2.get_val_at(b"root:b:new_b"), Some(&222));
        assert_eq!(map2.get_val_at(b"root:c:old"), Some(&300));

        // Test 3: Graft multiple child maps
        let mut map3: PathMap<i32> = PathMap::new();

        let mut map_for_x: PathMap<i32> = PathMap::new();
        map_for_x.set_val_at(b":data", 111);

        let mut map_for_y: PathMap<i32> = PathMap::new();
        map_for_y.set_val_at(b":info", 222);

        let mut map_for_z: PathMap<i32> = PathMap::new();
        map_for_z.set_val_at(b":stuff", 333);

        let child_mask3 = ByteMask::from(b'x') | ByteMask::from(b'y') | ByteMask::from(b'z');
        let maps3 = vec![map_for_x, map_for_y, map_for_z];

        let mut wz3 = map3.write_zipper();
        wz3.graft_child_maps(child_mask3, maps3, true);
        drop(wz3);

        assert_eq!(map3.get_val_at(b"x:data"), Some(&111));
        assert_eq!(map3.get_val_at(b"y:info"), Some(&222));
        assert_eq!(map3.get_val_at(b"z:stuff"), Some(&333));
        assert_eq!(map3.val_count(), 3);

        // Test 4: Empty mask should result in all branches removed when remove_unset=true
        let mut map4: PathMap<i32> = PathMap::new();
        map4.set_val_at(b"root:a", 1);
        map4.set_val_at(b"root:b", 2);

        let empty_mask = ByteMask::EMPTY;
        let empty_maps: Vec<PathMap<i32>> = vec![];

        let mut wz4 = map4.write_zipper_at_path(b"root:");
        wz4.graft_child_maps(empty_mask, empty_maps, true);
        drop(wz4);

        assert_eq!(map4.get_val_at(b"root:a"), None);
        assert_eq!(map4.get_val_at(b"root:b"), None);
    }

    #[test]
    fn write_zipper_graft_child_maps_test2() {
        // This test explicitly covers all 4 code paths through the optimized graft_child_maps implementation:
        // 1. map_count < 2 && remove_unset == true
        // 2. map_count < 2 && remove_unset == false
        // 3. map_count > 2 && remove_unset == true
        // 4. map_count > 2 && remove_unset == false

        // Path 1: map_count < 2 (specifically 1) && remove_unset == true
        // This tests the slow path with a single child being grafted and other children removed
        let mut map1: PathMap<i32> = PathMap::new();
        map1.set_val_at(b"root:a:old1", 1);
        map1.set_val_at(b"root:b:old2", 2);
        map1.set_val_at(b"root:c:old3", 3);

        let mut map_for_a: PathMap<i32> = PathMap::new();
        map_for_a.set_val_at(b":new_a", 100);
        map_for_a.set_val_at(b":nested:deep", 101);

        let child_mask1 = ByteMask::from(b'a');  // Only 1 child, so map_count == 1
        let maps1 = vec![map_for_a];

        let mut wz1 = map1.write_zipper_at_path(b"root:");
        wz1.graft_child_maps(child_mask1, maps1, true);
        drop(wz1);

        // Only 'a' branch should exist, 'b' and 'c' should be removed
        assert_eq!(map1.get_val_at(b"root:a:new_a"), Some(&100));
        assert_eq!(map1.get_val_at(b"root:a:nested:deep"), Some(&101));
        assert_eq!(map1.get_val_at(b"root:a:old1"), None);
        assert_eq!(map1.get_val_at(b"root:b:old2"), None);
        assert_eq!(map1.get_val_at(b"root:c:old3"), None);

        // Path 2: map_count < 2 (specifically 1) && remove_unset == false
        // This tests the slow path with a single child being grafted but other children preserved
        let mut map2: PathMap<i32> = PathMap::new();
        map2.set_val_at(b"root:x:old_x", 10);
        map2.set_val_at(b"root:y:old_y", 20);
        map2.set_val_at(b"root:z:old_z", 30);

        let mut map_for_y: PathMap<i32> = PathMap::new();
        map_for_y.set_val_at(b":new_y", 200);

        let child_mask2 = ByteMask::from(b'y');  // Only 1 child, so map_count == 1
        let maps2 = vec![map_for_y];

        let mut wz2 = map2.write_zipper_at_path(b"root:");
        wz2.graft_child_maps(child_mask2, maps2, false);
        drop(wz2);

        // 'y' should be replaced, 'x' and 'z' should be preserved
        assert_eq!(map2.get_val_at(b"root:x:old_x"), Some(&10));
        assert_eq!(map2.get_val_at(b"root:y:old_y"), None);
        assert_eq!(map2.get_val_at(b"root:y:new_y"), Some(&200));
        assert_eq!(map2.get_val_at(b"root:z:old_z"), Some(&30));

        // Path 3: map_count > 2 (specifically 3) && remove_unset == true
        // This tests the FAST PATH optimized implementation that builds a new ByteNode directly
        let mut map3: PathMap<i32> = PathMap::new();
        map3.set_val_at(b"root:a:old", 1);
        map3.set_val_at(b"root:b:old", 2);
        map3.set_val_at(b"root:c:old", 3);
        map3.set_val_at(b"root:d:old", 4);
        map3.set_val_at(b"root:e:old", 5);

        let mut map_for_a: PathMap<i32> = PathMap::new();
        map_for_a.set_val_at(b":new_a", 300);

        let mut map_for_c: PathMap<i32> = PathMap::new();
        map_for_c.set_val_at(b":new_c", 301);

        let mut map_for_e: PathMap<i32> = PathMap::new();
        map_for_e.set_val_at(b":new_e", 302);

        let child_mask3 = ByteMask::from(b'a') | ByteMask::from(b'c') | ByteMask::from(b'e');  // 3 children, map_count == 3
        let maps3 = vec![map_for_a, map_for_c, map_for_e];

        let mut wz3 = map3.write_zipper_at_path(b"root:");
        wz3.graft_child_maps(child_mask3, maps3, true);
        drop(wz3);

        // Only 'a', 'c', and 'e' branches should exist, 'b' and 'd' should be removed
        assert_eq!(map3.get_val_at(b"root:a:new_a"), Some(&300));
        assert_eq!(map3.get_val_at(b"root:c:new_c"), Some(&301));
        assert_eq!(map3.get_val_at(b"root:e:new_e"), Some(&302));
        assert_eq!(map3.get_val_at(b"root:a:old"), None);
        assert_eq!(map3.get_val_at(b"root:b:old"), None);
        assert_eq!(map3.get_val_at(b"root:d:old"), None);
        assert_eq!(map3.val_count(), 3);

        // Path 4: map_count > 2 (specifically 4) && remove_unset == false
        // This tests the slow path with multiple children being grafted but others preserved
        let mut map4: PathMap<i32> = PathMap::new();
        map4.set_val_at(b"root:p:old_p", 40);
        map4.set_val_at(b"root:q:old_q", 41);
        map4.set_val_at(b"root:r:old_r", 42);
        map4.set_val_at(b"root:s:old_s", 43);
        map4.set_val_at(b"root:t:old_t", 44);

        let mut map_for_p: PathMap<i32> = PathMap::new();
        map_for_p.set_val_at(b":new_p", 400);

        let mut map_for_q: PathMap<i32> = PathMap::new();
        map_for_q.set_val_at(b":new_q", 401);

        let mut map_for_r: PathMap<i32> = PathMap::new();
        map_for_r.set_val_at(b":new_r", 402);

        let mut map_for_s: PathMap<i32> = PathMap::new();
        map_for_s.set_val_at(b":new_s", 403);

        let child_mask4 = ByteMask::from(b'p') | ByteMask::from(b'q') | ByteMask::from(b'r') | ByteMask::from(b's');  // 4 children, map_count == 4
        let maps4 = vec![map_for_p, map_for_q, map_for_r, map_for_s];

        let mut wz4 = map4.write_zipper_at_path(b"root:");
        wz4.graft_child_maps(child_mask4, maps4, false);
        drop(wz4);

        // 'p', 'q', 'r', 's' should be replaced, 't' should be preserved
        assert_eq!(map4.get_val_at(b"root:p:old_p"), None);
        assert_eq!(map4.get_val_at(b"root:p:new_p"), Some(&400));
        assert_eq!(map4.get_val_at(b"root:q:old_q"), None);
        assert_eq!(map4.get_val_at(b"root:q:new_q"), Some(&401));
        assert_eq!(map4.get_val_at(b"root:r:old_r"), None);
        assert_eq!(map4.get_val_at(b"root:r:new_r"), Some(&402));
        assert_eq!(map4.get_val_at(b"root:s:old_s"), None);
        assert_eq!(map4.get_val_at(b"root:s:new_s"), Some(&403));
        assert_eq!(map4.get_val_at(b"root:t:old_t"), Some(&44));  // 't' preserved
    }

    super::zipper::zipper_moving_tests::zipper_moving_tests!(write_zipper,
        |keys: &[&[u8]]| {
            let mut btm = PathMap::new();
            keys.iter().for_each(|k| { btm.set_val_at(k, ()); });
            btm
        },
        |btm: &mut PathMap<()>, path: &[u8]| -> WriteZipperUntracked<(), GlobalAlloc> {
            btm.write_zipper_at_path(path)
    });

    super::zipper::zipper_iteration_tests::zipper_iteration_tests!(write_zipper_owned,
        |keys: &[&[u8]]| {
            let mut btm = PathMap::new();
            keys.iter().for_each(|k| { btm.set_val_at(k, ()); });
            btm
        },
        |btm: &mut PathMap<()>, path: &[u8]| -> WriteZipperOwned<()> {
            btm.clone().into_write_zipper(path)
    });
}
