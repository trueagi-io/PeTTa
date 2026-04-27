//! Experimental zipper implementations for testing.
//!
//! Contains `FullZipper` and `NullZipper` - test stubs with incomplete methods.

#![allow(unused_variables, unreachable_code, unused_mut, dead_code, unused_imports)]

use super::PathMap;
use super::TrieValue;
use super::alloc::Allocator;
use super::ring::{AlgebraicStatus, DistributiveLattice, Lattice};
use super::trie_core::node::TrieNodeODRc;
use super::trie_core::node::*;
use super::utils::ByteMask;
use super::write_zipper::write_zipper_priv::WriteZipperPriv;
use super::zipper::*;

#[cfg(feature = "serialization")]
pub mod serialization;
#[cfg(feature = "serialization")]
pub mod tree_serialization;

#[cfg(feature = "zipper_alg")]
pub mod zipper_algebra;
#[cfg(not(feature = "zipper_alg"))]
mod zipper_algebra;

struct FullZipper {
    path: Vec<u8>,
}

impl Zipper for FullZipper {
    fn path_exists(&self) -> bool {
        true
    }
    fn is_val(&self) -> bool {
        true
    }
    fn child_count(&self) -> usize {
        256
    }
    fn child_mask(&self) -> ByteMask {
        [!0u64, !0u64, !0u64, !0u64].into()
    }
}

impl ZipperPathBuffer for FullZipper {
    unsafe fn origin_path_assert_len(&self, len: usize) -> &[u8] {
        assert!(len <= self.path.capacity());
        unsafe { core::slice::from_raw_parts(self.path.as_ptr(), len) }
    }

    fn prepare_buffers(&mut self) {
        self.reserve_buffers(EXPECTED_PATH_LEN, 0)
    }
    fn reserve_buffers(&mut self, path_len: usize, _stack_depth: usize) {
        self.path.reserve(path_len)
    }
}

impl ZipperMoving for FullZipper {
    fn at_root(&self) -> bool {
        self.path.is_empty()
    }
    fn reset(&mut self) {
        self.path.clear()
    }
    fn path(&self) -> &[u8] {
        &self.path[..]
    }
    fn val_count(&self) -> usize {
        usize::MAX / 2
    } // usize::MAX is a dangerous default for overflow
    fn descend_to<K: AsRef<[u8]>>(&mut self, k: K) {
        self.path.extend_from_slice(k.as_ref());
    }
    fn descend_to_byte(&mut self, k: u8) {
        self.path.push(k);
    }
    fn descend_indexed_byte(&mut self, idx: usize) -> bool {
        assert!(idx < 256);
        self.path.push(idx as u8);
        true
    }
    fn descend_first_byte(&mut self) -> bool {
        self.path.push(0);
        true
    }
    fn descend_until(&mut self) -> bool {
        self.path.push(0); // not sure?
        true
    }
    fn ascend(&mut self, steps: usize) -> bool {
        if steps > self.path.len() {
            self.path.clear();
            false
        } else {
            self.path.truncate(self.path.len() - steps);
            true
        }
    }
    fn ascend_byte(&mut self) -> bool {
        self.path.pop().is_some()
    }
    fn ascend_until(&mut self) -> bool {
        self.path.pop().is_some() // not sure?
    }
    fn ascend_until_branch(&mut self) -> bool {
        self.path.pop().is_some() // not sure? What's the difference with the previous?
    }
    fn to_next_sibling_byte(&mut self) -> bool {
        self.to_sibling(true)
    }
    fn to_prev_sibling_byte(&mut self) -> bool {
        self.to_sibling(false)
    }
}

impl FullZipper {
    fn to_sibling(&mut self, next: bool) -> bool {
        if self.path.is_empty() {
            return false;
        } // right?
        if next {
            let last = self.path.last_mut().unwrap();
            if *last != 255 {
                *last += 1;
                true
            } else {
                false
            }
        } else {
            let first = self.path.first_mut().unwrap();
            if *first != 0 {
                *first -= 1;
                true
            } else {
                false
            }
        }
    }
}

// Doesn't seem as lawful as the above, still maybe useful for testing
struct NullZipper {}

impl Zipper for NullZipper {
    fn path_exists(&self) -> bool {
        false
    }
    fn is_val(&self) -> bool {
        false
    }
    fn child_count(&self) -> usize {
        0
    }
    fn child_mask(&self) -> ByteMask {
        ByteMask::EMPTY
    }
}

impl ZipperMoving for NullZipper {
    fn at_root(&self) -> bool {
        true
    }
    fn reset(&mut self) {}
    fn path(&self) -> &[u8] {
        &[]
    }
    fn val_count(&self) -> usize {
        0
    }
    fn descend_to<K: AsRef<[u8]>>(&mut self, _k: K) {}
    fn descend_to_byte(&mut self, _k: u8) {}
    fn descend_indexed_byte(&mut self, _idx: usize) -> bool {
        false
    }
    fn descend_first_byte(&mut self) -> bool {
        false
    }
    fn descend_until(&mut self) -> bool {
        false
    }
    fn ascend(&mut self, _steps: usize) -> bool {
        false
    }
    fn ascend_byte(&mut self) -> bool {
        false
    }
    fn ascend_until(&mut self) -> bool {
        false
    }
    fn ascend_until_branch(&mut self) -> bool {
        false
    }
    fn to_next_sibling_byte(&mut self) -> bool {
        false
    }
    fn to_prev_sibling_byte(&mut self) -> bool {
        false
    }
}

impl<V: TrieValue, A: Allocator> WriteZipperPriv<V, A> for NullZipper {
    fn take_focus(&mut self, prune: bool) -> Option<TrieNodeODRc<V, A>> {
        None
    }
    fn take_root_prefix_path(&mut self) -> Vec<u8> {
        unimplemented!()
    }
    fn alloc(&self) -> A {
        unimplemented!()
    }
}

impl<V: TrieValue + 'static, A: Allocator> ZipperWriting<V, A> for NullZipper {
    type ZipperHead<'z>
        = ZipperHead<'z, 'static, V>
    where
        Self: 'z;

    fn get_val_mut(&mut self) -> Option<&mut V> {
        None
    }
    fn get_val_or_set_mut(&mut self, default: V) -> &mut V {
        Box::leak(Box::new(default))
    }
    fn get_val_or_set_mut_with<F>(&mut self, func: F) -> &mut V
    where
        F: FnOnce() -> V,
    {
        Box::leak(Box::new(func()))
    }
    fn set_val(&mut self, _val: V) -> Option<V> {
        None
    }
    fn remove_val(&mut self, _prune: bool) -> Option<V> {
        None
    }
    fn zipper_head<'z>(&'z mut self) -> Self::ZipperHead<'z> {
        todo!()
    }
    fn graft<Z: ZipperInfallibleSubtries<V, A>>(&mut self, _read_zipper: &Z) {}
    fn graft_map(&mut self, _map: PathMap<V, A>) {}
    fn join_into<Z: ZipperInfallibleSubtries<V, A>>(&mut self, _read_zipper: &Z) -> AlgebraicStatus
    where
        V: Lattice,
    {
        AlgebraicStatus::Element
    }
    fn join_map_into(&mut self, _map: PathMap<V, A>) -> AlgebraicStatus
    where
        V: Lattice,
    {
        AlgebraicStatus::Element
    }
    fn join_into_take<Z: ZipperInfallibleSubtries<V, A> + ZipperWriting<V, A>>(
        &mut self,
        _src_zipper: &mut Z,
        prune: bool,
    ) -> AlgebraicStatus
    where
        V: Lattice,
    {
        AlgebraicStatus::Element
    }
    fn join_k_path_into(&mut self, _byte_cnt: usize, _prune: bool) -> bool
    where
        V: Lattice,
    {
        false
    }
    fn meet_k_path_into(&mut self, byte_cnt: usize, prune: bool) -> bool
    where
        V: Lattice,
    {
        false
    }
    fn insert_prefix<K: AsRef<[u8]>>(&mut self, _prefix: K) -> bool {
        false
    }
    fn remove_prefix(&mut self, _n: usize) -> bool {
        false
    }
    fn meet_into<Z: ZipperInfallibleSubtries<V, A>>(
        &mut self,
        _read_zipper: &Z,
        _prune: bool,
    ) -> AlgebraicStatus
    where
        V: Lattice,
    {
        AlgebraicStatus::Element
    }
    fn meet_2<'z, ZA: ZipperInfallibleSubtries<V, A>, ZB: ZipperInfallibleSubtries<V, A>>(
        &mut self,
        _rz_a: &ZA,
        _rz_b: &ZB,
    ) -> AlgebraicStatus
    where
        V: Lattice,
    {
        AlgebraicStatus::Element
    }
    fn subtract_into<Z: ZipperInfallibleSubtries<V, A>>(
        &mut self,
        _read_zipper: &Z,
        _prune: bool,
    ) -> AlgebraicStatus
    where
        V: DistributiveLattice,
    {
        AlgebraicStatus::Element
    }
    fn restrict<Z: ZipperInfallibleSubtries<V, A>>(&mut self, _read_zipper: &Z) -> AlgebraicStatus {
        AlgebraicStatus::Element
    }
    fn restricting<Z: ZipperInfallibleSubtries<V, A>>(&mut self, _read_zipper: &Z) -> bool {
        false
    }
    fn remove_branches(&mut self, prune: bool) -> bool {
        false
    }
    fn take_map(&mut self, prune: bool) -> Option<PathMap<V, A>> {
        None
    }
    fn remove_unmasked_branches(&mut self, _mask: ByteMask, prune: bool) {}
    fn create_path(&mut self) -> bool {
        false
    }
    fn prune_path(&mut self) -> usize {
        0
    }
    fn prune_ascend(&mut self) -> usize {
        0
    }
}
