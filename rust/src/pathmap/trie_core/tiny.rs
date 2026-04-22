//! [TinyRefNode] is a 16 byte pointer into another node (mainly a LineListNode) that implements the TrieNode trait.
//! It is used when it's necessary to have a node type to refer to the a point in the trie that exists within
//! another node.  For example, when describing the source for a `graft` operation.
//!
//! The purpose of this node type is to allow for a zero-alloc way to create a pointer into a location
//! in the middle of another node that behaves as a node unto itself.
//!
//! `TinyRefNode` is fundamentall read-only, and will panic in any attempt to write to this node type.

use core::mem::MaybeUninit;
use core::fmt::{Debug, Formatter};
use std::collections::HashMap;

use fast_slice_utils::{find_prefix_overlap, starts_with};
use super::super::utils::ByteMask;
use super::super::alloc::Allocator;
use super::node::*;
use super::super::ring::*;

/// A borrowed reference to a payload with a key stored elsewhere, contained in 16 Bytes
#[derive(Clone, Copy)]
pub struct TinyRefNode<'a, V: Clone + Send + Sync, A: Allocator> {
    /// bit 7 = used
    /// bit 6 = is_child
    /// bit 5 to bit 0 = key_len
    key_bytes: [MaybeUninit<u8>; 7],
    header: u8,
    payload: &'a ValOrChildUnion<V, A>,
    pub(crate) alloc: A,
}

impl<V: Clone + Send + Sync, A: Allocator> Debug for TinyRefNode<'_, V, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let converted = self.clone().into_full().unwrap();
        write!(f, "TinyRefNode -> {converted:?}")
    }
}

impl<'a, V: Clone + Send + Sync, A: Allocator> TinyRefNode<'a, V, A> {

    pub fn new_in(is_child: bool, key: &[u8], payload: &'a ValOrChildUnion<V, A>, alloc: A) -> Self {
        let mut new_node = Self {
            header: Self::header(is_child, key.len()),
            key_bytes: [MaybeUninit::uninit(); 7],
            payload,
            alloc,
        };
        unsafe{ core::ptr::copy_nonoverlapping(key.as_ptr(), new_node.key_bytes.as_mut_ptr().cast(), key.len()); }
        new_node
    }

    /// Turn the TinyRefNode into a LineListNode by cloning the payload
    pub fn into_list_node(&self) -> Option<super::line_list::LineListNode<V, A>> {
        self.clone_payload().map(|payload| {
            let mut new_node = super::line_list::LineListNode::new_in(self.alloc.clone());
            unsafe{ new_node.set_payload_owned::<0>(self.key(), payload); }
            debug_assert!(super::line_list::validate_node(&new_node));
            new_node
        })
    }

    #[cfg(feature = "bridge_nodes")]
    /// Turn the TinyRefNode into a BridgeNode by cloning the payload
    pub fn into_bridge_node(&self) -> Option<super::bridge::BridgeNode<V, A>> {
        let is_child = self.is_child_ptr();
        let payload: ValOrChildUnion<V> = if is_child {
            unsafe{ &*self.payload.child }.clone().into()
        } else {
            unsafe{ &**self.payload.val }.clone().into()
        };
        Some(super::bridge::BridgeNode::new(self.key(), is_child, payload))
    }

    #[cfg(not(feature = "bridge_nodes"))]
    pub fn into_full(&self) -> Option<super::line_list::LineListNode<V, A>> {
        self.into_list_node()
    }

    #[cfg(feature = "bridge_nodes")]
    pub fn into_full(&self) -> Option<super::bridge::BridgeNode<V, A>> {
        self.into_bridge_node()
    }

    /// Clones the payload from self
    fn clone_payload(&self) -> Option<ValOrChild<V, A>> {
        if self.node_is_empty() {
            return None;
        } else {
            match self.is_child_ptr() {
                true => {
                    let child = unsafe{ &*self.payload.child }.clone();
                    Some(ValOrChild::Child(child))
                },
                false => {
                    let val = unsafe{ &**self.payload.val }.clone();
                    Some(ValOrChild::Val(val))
                }
            }
        }
    }
    fn header(is_child: bool, key_len: usize) -> u8 {
        debug_assert!(key_len <= 7);
        if is_child {
            ((1 << 7) | (1 << 6) | key_len) as u8
        } else {
            ((1 << 7) | key_len) as u8
        }
    }
    fn is_child_ptr(&self) -> bool {
        self.header & (1 << 6) > 0
    }
    fn is_used_child(&self) -> bool {
        self.header & ((1 << 7) | (1 << 6)) == ((1 << 7) | (1 << 6))
    }
    fn is_used_val(&self) -> bool {
        self.header & ((1 << 7) | (1 << 6)) == (1 << 7)
    }
    fn key_len(&self) -> usize {
        (self.header & 0x3f) as usize
    }
    fn key(&self) -> &[u8] {
        unsafe{ core::slice::from_raw_parts(self.key_bytes.as_ptr().cast(), self.key_len()) }
    }
}

impl<'a, V: Clone + Send + Sync, A: Allocator> TrieNode<V, A> for TinyRefNode<'a, V, A> {
    fn node_key_overlap(&self, key: &[u8]) -> usize {
        find_prefix_overlap(self.key(), key)
    }
    fn node_contains_partial_key(&self, key: &[u8]) -> bool {
        if starts_with(self.key(), key) {
            true
        } else {
            false
        }
    }
    fn node_get_child(&self, key: &[u8]) -> Option<(usize, &TrieNodeODRc<V, A>)> {
        if self.is_used_child() {
            let node_key = self.key();
            let key_len = node_key.len();
            if key.len() >= key_len {
                if node_key == &key[..key_len] {
                    let child = unsafe{ &*self.payload.child };
                    return Some((key_len, child))
                }
            }
        }
        None
    }
    fn node_get_child_mut(&mut self, _key: &[u8]) -> Option<(usize, &mut TrieNodeODRc<V, A>)> { unreachable!() }
    fn node_replace_child(&mut self, _key: &[u8], _new_node: TrieNodeODRc<V, A>) { unreachable!() }
    fn node_get_payloads<'node, 'res>(&'node self, keys: &[(&[u8], bool)], results: &'res mut [(usize, PayloadRef<'node, V, A>)]) -> bool {
        if self.node_is_empty() {
            return true
        }
        let mut requested_contained_item = false; // This node type only has one item
        let self_key = self.key();
        debug_assert!(results.len() >= keys.len());
        for ((key, expect_val), (result_key_len, payload_ref)) in keys.into_iter().zip(results.into_iter()) {
            if starts_with(key, self_key) {
                let self_key_len = self_key.len();
                if self.is_child_ptr() {
                    if !*expect_val || self_key_len < key.len() {
                        requested_contained_item = true;
                        *result_key_len = self_key_len;
                        *payload_ref = PayloadRef::Child(unsafe{ &*self.payload.child });
                    }
                } else {
                    if *expect_val && self_key_len == key.len() {
                        requested_contained_item = true;
                        *result_key_len = self_key_len;
                        *payload_ref = PayloadRef::Val(unsafe{ &**self.payload.val });
                    }
                }
            }
        }
        requested_contained_item
    }
    fn node_contains_val(&self, key: &[u8]) -> bool {
        if self.is_used_val() {
            let node_key = self.key();
            if node_key == key {
                return true;
            }
        }
        false
    }
    fn node_get_val(&self, key: &[u8]) -> Option<&V> {
        if self.is_used_val() {
            let node_key = self.key();
            if node_key == key {
                let val = unsafe{ &**self.payload.val };
                return Some(val);
            }
        }
        None
    }
    fn node_remove_val(&mut self, _key: &[u8], _prune: bool) -> Option<V> { unreachable!() }
    fn node_create_dangling(&mut self, _key: &[u8]) -> Result<(bool, bool), TrieNodeODRc<V, A>> { unreachable!() }
    fn node_remove_dangling(&mut self, _key: &[u8]) -> usize { unreachable!() }
    fn node_get_val_mut(&mut self, _key: &[u8]) -> Option<&mut V> { unreachable!() }
    fn node_set_val(&mut self, key: &[u8], val: V) -> Result<(Option<V>, bool), TrieNodeODRc<V, A>> {
        let mut replacement_node = self.into_full().unwrap();
        replacement_node.node_set_val(key, val).unwrap_or_else(|_| panic!());
        Err(TrieNodeODRc::new_in(replacement_node, self.alloc.clone()))
    }
    fn node_set_branch(&mut self, key: &[u8], new_node: TrieNodeODRc<V, A>) -> Result<bool, TrieNodeODRc<V, A>> {
        let mut replacement_node = self.into_full().unwrap();
        replacement_node.node_set_branch(key, new_node).unwrap_or_else(|_| panic!());
        Err(TrieNodeODRc::new_in(replacement_node, self.alloc.clone()))
    }
    fn node_remove_all_branches(&mut self, _key: &[u8], _prune: bool) -> bool { unreachable!() }
    fn node_remove_unmasked_branches(&mut self, _key: &[u8], _mask: ByteMask, _prune: bool) { unreachable!() }
    fn node_is_empty(&self) -> bool {
        self.header & (1 << 7) == 0
    }
    fn new_iter_token(&self) -> u128 { unreachable!() }
    fn iter_token_for_path(&self, _key: &[u8]) -> u128 { unreachable!() }
    fn next_items(&self, _token: u128) -> (u128, &'a[u8], Option<&TrieNodeODRc<V, A>>, Option<&V>) { unreachable!() }
    fn node_val_count(&self, cache: &mut HashMap<u64, usize>) -> usize {
        let temp_node = self.into_full().unwrap();
        temp_node.node_val_count(cache)
    }
    fn node_goat_val_count(&self) -> usize {
        self.into_full().unwrap().node_goat_val_count()
    }
    fn node_child_iter_start(&self) -> (u64, Option<&TrieNodeODRc<V, A>>) {
        if self.is_used_child() {
            (0, Some(unsafe{ &*self.payload.child }))
        } else {
            (0, None)
        }
    }
    fn node_child_iter_next(&self, _token: u64) -> (u64, Option<&TrieNodeODRc<V, A>>) {
        (0, None) //A TinyNode only has, at most, one child
    }
    #[cfg(feature = "counters")]
    fn item_count(&self) -> usize {
        panic!();
    }
    fn node_first_val_depth_along_key(&self, key: &[u8]) -> Option<usize> {
        debug_assert!(key.len() > 0);
        let node_key = self.key();
        if self.is_used_val() && starts_with(key, node_key) {
            Some(node_key.len() - 1)
        } else {
            None
        }
    }
    fn nth_child_from_key(&self, _key: &[u8], _n: usize) -> (Option<u8>, Option<TaggedNodeRef<'_, V, A>>) {
        panic!();
    }
    fn first_child_from_key(&self, _key: &[u8]) -> (Option<&[u8]>, Option<TaggedNodeRef<'_, V, A>>) {
        panic!();
    }
    fn count_branches(&self, _key: &[u8]) -> usize {
        panic!();
    }
    fn node_branches_mask(&self, _key: &[u8]) -> ByteMask {
        panic!();
    }
    fn prior_branch_key<'key>(&self, _key: &'key [u8]) -> &'key [u8] {
        panic!();
    }
    fn get_sibling_of_child(&self, _key: &[u8], _next: bool) -> (Option<u8>, Option<TaggedNodeRef<'_, V, A>>) {
        panic!();
    }
    fn get_node_at_key(&self, key: &[u8]) -> AbstractNodeRef<'_, V, A> {
        //I don't think there is a set of circumstances that can give us an empty TinyRefNode
        debug_assert!(!self.node_is_empty());

        //Zero-length key means borrow this node
        if key.len() == 0 {
            return AbstractNodeRef::BorrowedDyn(self.as_tagged())
        }

        //Exact match with a path to a child node means return that node
        let node_key = self.key();
        if self.is_used_child() && node_key == key {
            return AbstractNodeRef::BorrowedRc(unsafe{ &*self.payload.child })
        }

        //Otherwise check to see if we need to make a sub-node.
        if node_key.len() > key.len() && starts_with(node_key, key) {
            let new_key = &node_key[key.len()..];
            let ref_node = TinyRefNode::new_in(self.is_child_ptr(), new_key, self.payload, self.alloc.clone());
            return AbstractNodeRef::BorrowedTiny(ref_node)
        }

        //The key must specify a path the node doesn't contains
        AbstractNodeRef::None
    }
    fn take_node_at_key(&mut self, _key: &[u8], _prune: bool) -> Option<TrieNodeODRc<V, A>> { unreachable!() }
    fn pjoin_dyn(&self, other: TaggedNodeRef<V, A>) -> AlgebraicResult<TrieNodeODRc<V, A>> where V: Lattice {
        //TODO, I can streamline this quite a lot, but for now I'll just up-convert to a ListNode to test
        // the basic premise of the TinyRefNode
        self.into_full().unwrap().pjoin_dyn(other)
    }
    fn join_into_dyn(&mut self, _other: TrieNodeODRc<V, A>) -> (AlgebraicStatus, Result<(), TrieNodeODRc<V, A>>) where V: Lattice { unreachable!() }
    fn drop_head_dyn(&mut self, _byte_cnt: usize) -> Option<TrieNodeODRc<V, A>> where V: Lattice { unreachable!() }
    fn pmeet_dyn(&self, other: TaggedNodeRef<V, A>) -> AlgebraicResult<TrieNodeODRc<V, A>> where V: Lattice {
        //TODO, is this worth bespoke code to save some cycles?
        self.into_full().unwrap().pmeet_dyn(other)
    }
    fn psubtract_dyn(&self, other: TaggedNodeRef<V, A>) -> AlgebraicResult<TrieNodeODRc<V, A>> where V: DistributiveLattice {
        //TODO, is this worth bespoke code to save some cycles?
        self.into_full().unwrap().psubtract_dyn(other)
    }
    fn prestrict_dyn(&self, other: TaggedNodeRef<V, A>) -> AlgebraicResult<TrieNodeODRc<V, A>> {
        //TODO, is this worth bespoke code to save some cycles?
        self.into_full().unwrap().prestrict_dyn(other)
    }
    fn clone_self(&self) -> TrieNodeODRc<V, A> {
        TrieNodeODRc::new_in(self.clone(), self.alloc.clone())
    }
}

impl<V: Clone + Send + Sync, A: Allocator> TrieNodeDowncast<V, A> for TinyRefNode<'_, V, A> {
    #[inline]
    fn tag(&self) -> usize {
        TINY_REF_NODE_TAG
    }
    #[inline]
    fn as_tagged(&self) -> TaggedNodeRef<'_, V, A> {
        TaggedNodeRef::from_tiny(self)
    }
    #[cfg(not(feature="slim_ptrs"))]
    fn as_tagged_mut(&mut self) -> TaggedNodeRefMut<'_, V, A> {
        unreachable!()
    }
    fn convert_to_cell_node(&mut self) -> TrieNodeODRc<V, A> {
        unreachable!()
    }
}

#[cfg(all(test, feature = "pathmap-internal-tests"))]
mod tests {
    use super::*;
    use super::alloc::GlobalAlloc;

    #[test]
    fn test_tiny_node() {
        //Confirm TinyRefNode is 16 bytes
        assert_eq!(std::mem::size_of::<TinyRefNode::<(), GlobalAlloc>>(), 16);
    }
}