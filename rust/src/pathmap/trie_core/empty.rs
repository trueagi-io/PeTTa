
use core::fmt::Debug;
use std::collections::HashMap;

use super::super::alloc::Allocator;
use super::node::*;
use super::super::ring::*;
use super::super::utils::ByteMask;

#[derive(Clone, Copy, Default, Debug)]
pub struct EmptyNode;

impl<V: Clone + Send + Sync, A: Allocator> TrieNode<V, A> for EmptyNode {
    fn node_key_overlap(&self, _key: &[u8]) -> usize {
        0
    }
    fn node_contains_partial_key(&self, _key: &[u8]) -> bool {
        false
    }
    fn node_get_child(&self, _key: &[u8]) -> Option<(usize, &TrieNodeODRc<V, A>)> {
        None
    }
    fn node_get_child_mut(&mut self, _key: &[u8]) -> Option<(usize, &mut TrieNodeODRc<V, A>)> {
        None
    }
    fn node_replace_child(&mut self, _key: &[u8], _new_node: TrieNodeODRc<V, A>) {
        unreachable!() //Should not be called unless it's known that the node being replaced exists
    }
    fn node_get_payloads<'node, 'res>(&'node self, _keys: &[(&[u8], bool)], _results: &'res mut [(usize, PayloadRef<'node, V, A>)]) -> bool {
        true
    }
    fn node_contains_val(&self, _key: &[u8]) -> bool {
        false
    }
    fn node_get_val(&self, _key: &[u8]) -> Option<&V> {
        None
    }
    fn node_remove_val(&mut self, _key: &[u8], _prune: bool) -> Option<V> {
        unreachable!()
    }
    fn node_create_dangling(&mut self, _key: &[u8]) -> Result<(bool, bool), TrieNodeODRc<V, A>> {
        unreachable!()
    }
    fn node_remove_dangling(&mut self, _key: &[u8]) -> usize {
        unreachable!()
    }
    fn node_get_val_mut(&mut self, _key: &[u8]) -> Option<&mut V> {
        None
    }
    fn node_set_val(&mut self, _key: &[u8], _val: V) -> Result<(Option<V>, bool), TrieNodeODRc<V, A>> {
        unreachable!() //we should head this off upstream
    }
    fn node_set_branch(&mut self, _key: &[u8], _new_node: TrieNodeODRc<V, A>) -> Result<bool, TrieNodeODRc<V, A>> {
        unreachable!() //we should head this off upstream
    }
    fn node_remove_all_branches(&mut self, _key: &[u8], _prune: bool) -> bool {
        false
    }
    fn node_remove_unmasked_branches(&mut self, _key: &[u8], _mask: ByteMask, _prune: bool) {}
    fn node_is_empty(&self) -> bool { true }
    fn new_iter_token(&self) -> u128 {
        0
    }
    fn iter_token_for_path(&self, _key: &[u8]) -> u128 {
        0
    }
    fn next_items(&self, _token: u128) -> (u128, &[u8], Option<&TrieNodeODRc<V, A>>, Option<&V>) {
        (NODE_ITER_FINISHED, &[], None, None)
    }
    fn node_val_count(&self, _cache: &mut HashMap<u64, usize>) -> usize {
        0
    }
    fn node_goat_val_count(&self) -> usize {
        0
    }
    fn node_child_iter_start(&self) -> (u64, Option<&TrieNodeODRc<V, A>>) {
        (0, None)
    }
    fn node_child_iter_next(&self, _token: u64) -> (u64, Option<&TrieNodeODRc<V, A>>) {
        (0, None)
    }
    #[cfg(feature = "counters")]
    fn item_count(&self) -> usize {
        0
    }
    fn node_first_val_depth_along_key(&self, _key: &[u8]) -> Option<usize> {
        None
    }
    fn nth_child_from_key(&self, _key: &[u8], _n: usize) -> (Option<u8>, Option<TaggedNodeRef<'_, V, A>>) {
        (None, None)
    }
    fn first_child_from_key(&self, _key: &[u8]) -> (Option<&[u8]>, Option<TaggedNodeRef<'_, V, A>>) {
        (None, None)
    }
    fn count_branches(&self, _key: &[u8]) -> usize {
        0
    }
    fn node_branches_mask(&self, _key: &[u8]) -> ByteMask {
        ByteMask::EMPTY
    }
    fn prior_branch_key<'key>(&self, _key: &'key [u8]) -> &'key [u8] {
        &[]
    }
    fn get_sibling_of_child(&self, _key: &[u8], _next: bool) -> (Option<u8>, Option<TaggedNodeRef<'_, V, A>>) {
        (None, None)
    }
    fn get_node_at_key(&self, _key: &[u8]) -> AbstractNodeRef<'_, V, A> {
        AbstractNodeRef::None
    }
    fn take_node_at_key(&mut self, _key: &[u8], _prune: bool) -> Option<TrieNodeODRc<V, A>> {
        None
    }
    fn pjoin_dyn(&self, other: TaggedNodeRef<V, A>) -> AlgebraicResult<TrieNodeODRc<V, A>> where V: Lattice {
        if other.node_is_empty() {
            AlgebraicResult::None
        } else {
            AlgebraicResult::Identity(COUNTER_IDENT)
        }
    }
    fn join_into_dyn(&mut self, other: TrieNodeODRc<V, A>) -> (AlgebraicStatus, Result<(), TrieNodeODRc<V, A>>) where V: Lattice {
        if other.as_tagged().node_is_empty() {
            (AlgebraicStatus::None, Ok(()))
        } else {
            (AlgebraicStatus::Element, Err(other.clone()))
        }
    }
    fn drop_head_dyn(&mut self, _byte_cnt: usize) -> Option<TrieNodeODRc<V, A>> where V: Lattice {
        None
    }
    fn pmeet_dyn(&self, other: TaggedNodeRef<V, A>) -> AlgebraicResult<TrieNodeODRc<V, A>> where V: Lattice {
        if other.node_is_empty() {
            AlgebraicResult::Identity(SELF_IDENT | COUNTER_IDENT)
        } else {
            AlgebraicResult::Identity(SELF_IDENT)
        }
    }
    fn psubtract_dyn(&self, _other: TaggedNodeRef<V, A>) -> AlgebraicResult<TrieNodeODRc<V, A>> where V: DistributiveLattice {
        AlgebraicResult::None
    }
    fn prestrict_dyn(&self, _other: TaggedNodeRef<V, A>) -> AlgebraicResult<TrieNodeODRc<V, A>> {
        AlgebraicResult::None
    }
    fn clone_self(&self) -> TrieNodeODRc<V, A> {
        unreachable!() //If we end up hitting this, we should change it at the call site
    }
}

impl<V: Clone + Send + Sync, A: Allocator> TrieNodeDowncast<V, A> for EmptyNode {
    #[inline]
    fn tag(&self) -> usize {
        EMPTY_NODE_TAG
    }
    fn as_tagged(&self) -> TaggedNodeRef<'_, V, A> {
        unreachable!()
    }
    #[cfg(not(feature="slim_ptrs"))]
    fn as_tagged_mut(&mut self) -> TaggedNodeRefMut<'_, V, A> {
        unreachable!()
    }
    fn convert_to_cell_node(&mut self) -> TrieNodeODRc<V, A> {
        unreachable!() //If we end up hitting this, we should change it at the call site
    }
}
