
use core::mem::{MaybeUninit, ManuallyDrop};
use core::fmt::{Debug, Formatter};
use std::collections::HashMap;

use super::node::*;
use super::super::ring::*;
use super::dense_byte::{DenseByteNode, CellByteNode, test_bit_in_mask};
use super::tiny::TinyRefNode;
use super::super::utils::starts_with;

/// A node type that only has a single value or onward link
pub struct BridgeNode<V> {
    key_bytes: [MaybeUninit<u8>; KEY_BYTES_CNT],
    /// bit 7 = used
    /// bit 6 = is_child
    /// bit 5 to bit 0 = key_len
    header: u8,
    payload: ValOrChildUnion<V>
}

const KEY_BYTES_CNT: usize = 31;

impl<V> Drop for BridgeNode<V> {
    fn drop(&mut self) {
        self.drop_payload()
    }
}

impl<V: Clone> Clone for BridgeNode<V> {
    fn clone(&self) -> Self {
        Self {
            header: self.header,
            key_bytes: self.key_bytes,
            payload: self.clone_payload(),
        }
    }
}

impl<V> Debug for BridgeNode<V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let key = if !self.is_empty() {
            let key = self.key();
            match std::str::from_utf8(key) {
                Ok(str) => str.to_string(),
                Err(_) => format!("{key:?}")
            }
        } else {
            "".to_string()
        };
        write!(f, "BridgeNode (occupied={} is_child={} key={:?})", !self.is_empty(), self.is_child_ptr(), key)
    }
}

impl<V> BridgeNode<V> {
    fn header(is_child: bool, key_len: usize) -> u8 {
        debug_assert!(key_len <= KEY_BYTES_CNT);
        if is_child {
            ((1 << 7) | (1 << 6) | key_len) as u8
        } else {
            ((1 << 7) | key_len) as u8
        }
    }
    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.header & (1 << 7) == 0
    }
    #[inline(always)]
    pub fn is_child_ptr(&self) -> bool {
        self.header & (1 << 6) > 0
    }
    #[inline(always)]
    fn is_used_child(&self) -> bool {
        self.header & ((1 << 7) | (1 << 6)) == ((1 << 7) | (1 << 6))
    }
    #[inline(always)]
    fn is_used_val(&self) -> bool {
        self.header & ((1 << 7) | (1 << 6)) == (1 << 7)
    }
    #[inline(always)]
    pub fn key_len(&self) -> usize {
        (self.header & 0x3f) as usize
    }
    #[inline(always)]
    pub fn key(&self) -> &[u8] {
        unsafe{ core::slice::from_raw_parts(self.key_bytes.as_ptr().cast(), self.key_len()) }
    }
    #[inline(always)]
    fn drop_payload(&mut self) {
        if self.is_used_child() {
            unsafe{ ManuallyDrop::drop(&mut self.payload.child) }
        }
        if self.is_used_val() {
            unsafe{ ManuallyDrop::drop(&mut self.payload.val) }
        }
    }
}

impl<V: Clone> BridgeNode<V> {
    pub fn new_empty() -> Self {
        Self {
            header: 0,
            key_bytes: [MaybeUninit::uninit(); KEY_BYTES_CNT],
            payload: ValOrChildUnion{ _unused: () }
        }
    }
    /// Clone the payload from self
    pub fn clone_payload(&self) -> ValOrChildUnion<V> {
        debug_assert!(!self.is_empty());
        if self.is_child_ptr() {
            unsafe{ &*self.payload.child }.clone().into()
        } else {
            unsafe{ &**self.payload.val }.clone().into()
        }
    }
    /// Takes the payload from self, leaving self empty, but with `key()`` and `is_child_ptr()` continuing
    /// to return the old information
    pub fn take_payload(&mut self) -> ValOrChildUnion<V> {
        debug_assert_eq!(self.is_empty(), false);
        self.header &= !(1 << 7);
        core::mem::take(&mut self.payload)
    }
}

impl<V: Clone + Send + Sync> BridgeNode<V> {
    pub fn new(key: &[u8], is_child: bool, payload: ValOrChildUnion<V>) -> Self {
        debug_assert!(key.len() > 0);
        if key.len() <= KEY_BYTES_CNT {
            let mut new_node = Self {
                header: Self::header(is_child, key.len()),
                key_bytes: [MaybeUninit::uninit(); KEY_BYTES_CNT],
                payload
            };
            unsafe{ core::ptr::copy_nonoverlapping(key.as_ptr(), new_node.key_bytes.as_mut_ptr().cast(), key.len()); }
            new_node
        } else {
            let child_node = Self::new(&key[KEY_BYTES_CNT..], is_child, payload);
            let mut new_node = Self {
                header: Self::header(true, KEY_BYTES_CNT),
                key_bytes: [MaybeUninit::uninit(); KEY_BYTES_CNT],
                payload: TrieNodeODRc::new(child_node).into(),
            };
            unsafe{ core::ptr::copy_nonoverlapping(key.as_ptr(), new_node.key_bytes.as_mut_ptr().cast(), KEY_BYTES_CNT); }
            new_node
        }
    }
    fn set_payload(&mut self, key: &[u8], is_child: bool, payload: ValOrChildUnion<V>) -> bool {
        debug_assert!(key.len() > 0);
        debug_assert!(self.is_empty());
        if key.len() <= KEY_BYTES_CNT {
            self.header = Self::header(is_child, key.len());
            self.payload = payload;
            unsafe{ core::ptr::copy_nonoverlapping(key.as_ptr(), self.key_bytes.as_mut_ptr().cast(), key.len()); }
            false
        } else {
            let child = Self::new(&key[KEY_BYTES_CNT..], is_child, payload);
            self.header = Self::header(true, KEY_BYTES_CNT);
            self.payload = TrieNodeODRc::new(child).into();
            unsafe{ core::ptr::copy_nonoverlapping(key.as_ptr(), self.key_bytes.as_mut_ptr().cast(), KEY_BYTES_CNT); }
            true
        }
    }
    fn splice_new_payload<const IS_CHILD: bool>(&mut self, key: &[u8], mut new_payload: ValOrChildUnion<V>) -> Result<(Option<V>, bool), TrieNodeODRc<V>> {
        debug_assert_eq!(self.is_empty(), false);
        let node_key = self.key();
        let mut overlap = find_prefix_overlap(key, node_key);
        if overlap > 0 {
            //If the keys are an exact match and the payload is the same type, replace the payload
            if overlap == node_key.len() && overlap == key.len() && IS_CHILD == self.is_child_ptr() {
                core::mem::swap(&mut new_payload, &mut self.payload);
                return Ok((Some(unsafe{ new_payload.into_val() }), false))
            }

            //Make sure we have some key to work with, for the new split node
            if overlap == node_key.len() || overlap == key.len() {
                overlap -= 1;
            }

            //Make a new node containing what's left of self and the newly added payload
            let mut replacement_node = DenseByteNode::<V>::with_capacity(2);
            let self_payload = self.take_payload();
            replacement_node.add_payload(&self.key()[overlap..], self.is_child_ptr(), self_payload);
            replacement_node.add_payload(&key[overlap..], IS_CHILD, new_payload);

            //If we still have some overlap, split this node's key, If not, replace this node entirely
            if overlap > 0 {
                self.set_payload(&key[..overlap], true, TrieNodeODRc::new(replacement_node).into());
                Ok((None, true))
            } else {
                Err(TrieNodeODRc::new(replacement_node))
            }
        } else {
            //We have no overlap, so we should replace this node
            let mut replacement_node = DenseByteNode::<V>::with_capacity(2);
            let self_payload = self.take_payload();
            replacement_node.add_payload(self.key(), self.is_child_ptr(), self_payload);
            replacement_node.add_payload(key, IS_CHILD, new_payload);
            Err(TrieNodeODRc::new(replacement_node))
        }
    }
    fn merge_bridge_nodes(&self, other: &BridgeNode<V>) -> TrieNodeODRc<V> where V: Lattice {
        debug_assert!(!self.is_empty());
        debug_assert!(!other.is_empty());

        let self_key = self.key();
        let self_is_child = self.is_child_ptr();
        let other_key = other.key();
        let other_is_child = other.is_child_ptr();

        let mut overlap = find_prefix_overlap(self_key, other_key);
        if overlap > 0 {
            //If the keys are an exact match and the payload is the same type, merge the payloads
            if overlap == self_key.len() && overlap == other_key.len() && self_is_child == other_is_child {
                if self_is_child {
                    let self_child = unsafe{ &*self.payload.child };
                    let other_child = unsafe{ &*other.payload.child };
                    let new_child = self_child.borrow().join_dyn(other_child.borrow());
                    let new_node = Self::new(self_key, true, new_child.into());
                    return TrieNodeODRc::new(new_node);
                } else {
                    let self_val = unsafe{ &**self.payload.val };
                    let other_val = unsafe{ &**other.payload.val };
                    let new_val = self_val.join(other_val);
                    let new_node = Self::new(self_key, false, new_val.into());
                    return TrieNodeODRc::new(new_node);
                }
            }

            //Make sure we have some key to work with, for the new split node
            if overlap == self_key.len() || overlap == other_key.len() {
                overlap -= 1;
            }

            //Make a new node containing the unique parts of self and the other payload
            let mut split_node = DenseByteNode::<V>::with_capacity(2);
            split_node.merge_payload(&self_key[overlap..], self_is_child, self.clone_payload());
            split_node.merge_payload(&other_key[overlap..], other_is_child, other.clone_payload());

            //If we still have some overlap, make a preface BridgeNode, If not, the split_node is the result
            if overlap > 0 {
                let new_node = Self::new(&self_key[..overlap], true, TrieNodeODRc::new(split_node).into());
                return TrieNodeODRc::new(new_node)
            } else {
                return TrieNodeODRc::new(split_node)
            }
        } else {
            //We have no overlap, so we should replace this node
            let mut split_node = DenseByteNode::<V>::with_capacity(2);
            split_node.merge_payload(self_key, self_is_child, self.clone_payload());
            split_node.merge_payload(other_key, other_is_child, other.clone_payload());
            return TrieNodeODRc::new(split_node)
        }
    }

    /// Converts the node to a CellByteNode, transplanting the contents and leaving `self` empty
    fn convert_to_cell_node(&mut self, capacity: usize) -> TrieNodeODRc<V> where V: Clone {
        debug_assert!(!self.is_empty());
        let mut replacement_node = CellByteNode::<V>::with_capacity(capacity);

        //Transplant the key / value to the new node
        if !self.is_empty() {
            let mut payload = ValOrChildUnion{ _unused: () };
            core::mem::swap(&mut payload, &mut self.payload);
            let key = self.key();
            //DenseByteNodes hold one byte keys, so if the key is more than 1 byte we need to
            // make an intermediate node to hold the rest of the key
            if key.len() > 1 {
                let child_node = Self::new(&key[1..], self.is_child_ptr(), payload);
                replacement_node.set_child(key[0], TrieNodeODRc::new(child_node));
            } else {
                if self.is_child_ptr() {
                    replacement_node.set_child(key[0], unsafe{ payload.into_child() });
                } else {
                    replacement_node.set_val(key[0], unsafe{ payload.into_val() });
                }
            }
        }

        //Clear self.header, so we don't double-free anything when this old node gets dropped
        self.header = 0;

        TrieNodeODRc::new(replacement_node)
    }
}

impl<V: Clone + Send + Sync> TrieNode<V> for BridgeNode<V> {
    fn node_contains_partial_key(&self, key: &[u8]) -> bool {
        debug_assert!(!self.is_empty());
        if self.key().starts_with(key) {
            true
        } else {
            false
        }
    }
    fn node_get_child(&self, key: &[u8]) -> Option<(usize, &dyn TrieNode<V>)> {
        if self.is_used_child() {
            let node_key = self.key();
            let key_len = node_key.len();
            if key.len() >= key_len {
                if node_key == &key[..key_len] {
                    let child = unsafe{ &*self.payload.child }.borrow();
                    return Some((key_len, child))
                }
            }
        }
        None
    }
    fn node_get_child_mut(&mut self, key: &[u8]) -> Option<(usize, &mut TrieNodeODRc<V>)> {
        if self.is_used_child() {
            let node_key = self.key();
            let key_len = node_key.len();
            if key.len() >= key_len {
                if node_key == &key[..key_len] {
                    let child = unsafe{ &mut *self.payload.child };
                    return Some((key_len, child))
                }
            }
        }
        None
    }
    fn node_replace_child(&mut self, _key: &[u8], _new_node: TrieNodeODRc<V>) -> &mut dyn TrieNode<V> { unreachable!() }
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
    fn node_remove_val(&mut self, key: &[u8]) -> Option<V> {
        debug_assert_eq!(self.is_empty(), false);
        if !self.is_child_ptr() && self.key() == key {
            let payload = self.take_payload();
            Some(unsafe{ payload.into_val() })
        } else {
            None
        }
    }
    fn node_get_val_mut(&mut self, key: &[u8]) -> Option<&mut V> {
        if self.is_used_val() {
            let node_key = self.key();
            if node_key == key {
                let val = unsafe{ &mut **self.payload.val };
                return Some(val);
            }
        }
        None
    }
    fn node_set_val(&mut self, key: &[u8], val: V) -> Result<(Option<V>, bool), TrieNodeODRc<V>> {
        if !self.is_empty() {
            self.splice_new_payload::<false>(key, val.into())
        } else {
            let created_subnode = self.set_payload(key, false, val.into());
            Ok((None, created_subnode))
        }
    }
    fn node_set_branch(&mut self, key: &[u8], new_node: TrieNodeODRc<V>) -> Result<bool, TrieNodeODRc<V>> {
        self.splice_new_payload::<true>(key, new_node.into()).map(|(_, created_subnode)| created_subnode)
    }
    fn node_remove_all_branches(&mut self, key: &[u8]) -> bool {
        debug_assert!(!self.is_empty());
        let self_key = self.key();
        if self_key.starts_with(key) && (key.len() < self_key.len() || self.is_child_ptr()) {
            self.drop_payload();
            self.header = 0;
            true
        } else {
            false
        }
    }
    fn node_remove_unmasked_branches(&mut self, key: &[u8], mask: [u64; 4]) {
        debug_assert!(!self.is_empty());
        let self_key = self.key();
        if self_key.starts_with(key) {
            if key.len() < self_key.len() && !test_bit_in_mask(&mask, self_key[key.len()]) {
                self.drop_payload();
                self.header = 0;
                return
            }
            if self.is_child_ptr() {
                let child = unsafe{ &mut *self.payload.child }.make_mut();
                child.node_remove_unmasked_branches(&[], mask)
            }
        }
    }
    fn node_is_empty(&self) -> bool {
        self.is_empty()
    }
    #[inline(always)]
    fn new_iter_token(&self) -> u128 {
        0
    }
    #[inline(always)]
    fn iter_token_for_path(&self, key: &[u8]) -> (u128, &[u8]) {
        let node_key = self.key();
        if key.len() <= node_key.len() {
            let short_key = &node_key[..key.len()];
            if key < short_key {
                return (0, &[])
            }
            if key == short_key {
                return (1, node_key)
            }
        }
        (NODE_ITER_FINISHED, &[])
    }
    #[inline(always)]
    fn next_items(&self, token: u128) -> (u128, &[u8], Option<&TrieNodeODRc<V>>, Option<&V>) {
        if token == 0 {
            let node_key = self.key();
            if self.is_used_child() {
                let child = unsafe{ &*self.payload.child };
                return (1, node_key, Some(child), None)
            }
            if self.is_used_val() {
                let val = unsafe{ &**self.payload.val };
                return (1, node_key, None, Some(val))
            }
        }
        (NODE_ITER_FINISHED, &[], None, None)
    }
    fn node_val_count(&self, cache: &mut HashMap<*const dyn TrieNode<V>, usize>) -> usize {
        debug_assert!(!self.is_empty());
        if self.is_child_ptr() {
            let child = unsafe{ &*self.payload.child }.borrow();
            child.node_val_count(cache)
        } else {
            1
        }
    }
    #[cfg(feature = "counters")]
    fn item_count(&self) -> usize {
        if self.is_empty() {
            0
        } else {
            1
        }
    }
    fn node_first_val_depth_along_key(&self, key: &[u8]) -> Option<usize> {
        debug_assert!(key.len() > 0);
        debug_assert!(!self.is_empty());
        let node_key = self.key();
        if self.is_used_val() && key.starts_with(node_key) {
            Some(node_key.len() - 1)
        } else {
            None
        }
    }
    fn nth_child_from_key(&self, key: &[u8], n: usize) -> (Option<u8>, Option<&dyn TrieNode<V>>) {
        debug_assert!(!self.is_empty());
        if n == 0 {
            let self_key = self.key();
            let key_len = key.len();
            if self_key.len() > key_len && self_key.starts_with(key) {
                let next_byte = self_key[key_len];
                if self.is_child_ptr() {
                    return (Some(next_byte), Some(unsafe{ &*self.payload.child }.borrow()))
                } else {
                    return (Some(next_byte), None)
                }
            }
        }
        (None, None)
    }
    fn first_child_from_key(&self, key: &[u8]) -> (Option<&[u8]>, Option<&dyn TrieNode<V>>) {
        debug_assert!(!self.is_empty());
        let self_key = self.key();
        let key_len = key.len();
        if self_key.len() > key_len && self_key.starts_with(key) {
            let remaining_key = &self_key[key_len..];
            if self.is_child_ptr() {
                (Some(remaining_key), Some(unsafe{ &*self.payload.child }.borrow()))
            } else {
                (Some(remaining_key), None)
            }
        } else {
            (None, None)
        }
    }
    fn count_branches(&self, key: &[u8]) -> usize {
        if !self.is_empty() {
            let self_key = self.key();
            if self_key.starts_with(key) {
                if self.is_child_ptr() || self_key.len() > key.len() {
                    return 1
                }
            }
        }
        0
    }
    fn node_branches_mask(&self, key: &[u8]) -> [u64; 4] {
        debug_assert!(!self.is_empty());
        let self_key = self.key();
        let mut m = [0u64; 4];

        if self_key.len() > key.len() && self_key.starts_with(key) {
            let k = self_key[key.len()];
            m[((k & 0b11000000) >> 6) as usize] = 1u64 << (k & 0b00111111);
        }
        m
    }
    fn is_leaf(&self, key: &[u8]) -> bool {
        debug_assert!(!self.is_empty());
        let self_key = self.key();
        if self_key.starts_with(key) {
            if key.len() < self_key.len() || self.is_child_ptr() {
                return false
            }
        }
        true
    }
    fn prior_branch_key(&self, key: &[u8]) -> &[u8] {
        debug_assert!(key.len() > 0);
        //BridgeNodes never contain internal branches!
        &[]
    }
    fn get_sibling_of_child(&self, _key: &[u8], _next: bool) -> (Option<u8>, Option<&dyn TrieNode<V>>) {
        //BridgeNodes never contain siblings!
        (None, None)
    }
    fn get_node_at_key(&self, key: &[u8]) -> AbstractNodeRef<V> {
        debug_assert!(!self.is_empty());

        //Zero-length key means borrow this node
        if key.len() == 0 {
            return AbstractNodeRef::BorrowedDyn(self)
        }

        //Exact match with a path to a child node means return that node
        let node_key = self.key();
        if self.is_used_child() && node_key == key {
            return AbstractNodeRef::BorrowedRc(unsafe{ &*self.payload.child })
        }

        //Otherwise check to see if we need to make a sub-node
        if node_key.len() > key.len() && node_key.starts_with(key) {
            let new_key = &node_key[key.len()..];
            let ref_node = TinyRefNode::new(self.is_child_ptr(), new_key, &self.payload);
            return AbstractNodeRef::BorrowedTiny(ref_node)
        }

        //The key fails to specify a path contained within the node
        AbstractNodeRef::None
    }
    fn take_node_at_key(&mut self, key: &[u8]) -> Option<TrieNodeODRc<V>> {
        debug_assert!(!self.is_empty());
        let self_key = self.key();
        if starts_with(self_key, key) {
            if self_key.len() == key.len() {
                if self.is_child_ptr() {
                    let self_payload = self.take_payload();
                    Some(unsafe{ self_payload.into_child() })
                } else {
                    None
                }
            } else {
                let self_payload = self.take_payload();
                let self_key = self.key();
                let new_node = Self::new(&self_key[key.len()..], self.is_child_ptr(), self_payload);
                Some(TrieNodeODRc::new(new_node))
            }
        } else {
            None
        }
    }
    fn join_dyn(&self, other: &dyn TrieNode<V>) -> TrieNodeODRc<V> where V: Lattice {
        debug_assert!(!self.is_empty());
        match other.as_tagged() {
            TaggedNodeRef::BridgeNode(other_bridge_node) => {
                self.merge_bridge_nodes(other_bridge_node)
            },
            TaggedNodeRef::LineListNode(_other_list_node) => {
                unimplemented!()
            },
            TaggedNodeRef::DenseByteNode(other_dense_node) => {
                let mut new_dense = other_dense_node.clone();
                new_dense.merge_payload(self.key(), self.is_child_ptr(), self.clone_payload());
                TrieNodeODRc::new(new_dense)
            },
            TaggedNodeRef::TinyRefNode(tiny_node) => {
                tiny_node.join_dyn(self)
            },
            TaggedNodeRef::CellByteNode(other_dense_node) => {
                let mut new_dense = other_dense_node.clone();
                new_dense.merge_payload(self.key(), self.is_child_ptr(), self.clone_payload());
                TrieNodeODRc::new(new_dense)
            },
            TaggedNodeRef::EmptyNode(_) => {
                //GOAT, optimization opportunity.  Could communicate here that the resultant node is a clone
                // so we could just bump the refcount rather than make a new TrieNodeODRc pointer
                TrieNodeODRc::new(self.clone())
            }
        }
    }
    fn join_into_dyn(&mut self, _other: TrieNodeODRc<V>) -> Result<(), TrieNodeODRc<V>> where V: Lattice {
        //NOTE: This method could be implement the bridge->bridge path using `splice_new_payload`
        // and we'd also need a code path for upgrading to another node type, e.g. dense node
        unimplemented!()
    }
    fn drop_head_dyn(&mut self, _byte_cnt: usize) -> Option<TrieNodeODRc<V>> where V: Lattice {
        unimplemented!()
    }
    fn meet_dyn(&self, _other: &dyn TrieNode<V>) -> Option<TrieNodeODRc<V>> where V: Lattice {
        unimplemented!()
    }
    fn psubtract_dyn(&self, _other: &dyn TrieNode<V>) -> (bool, Option<TrieNodeODRc<V>>) where V: DistributiveLattice {
        unimplemented!()
    }
    fn prestrict_dyn(&self, _other: &dyn TrieNode<V>) -> Option<TrieNodeODRc<V>> {
        unimplemented!()
    }
    fn clone_self(&self) -> TrieNodeODRc<V> {
        TrieNodeODRc::new(self.clone())
    }
}

impl<V: Clone + Send + Sync> TrieNodeDowncast<V> for BridgeNode<V> {
    #[inline]
    fn as_tagged(&self) -> TaggedNodeRef<V> {
        TaggedNodeRef::BridgeNode(self)
    }
    #[inline]
    fn as_tagged_mut(&mut self) -> TaggedNodeRefMut<V> {
        TaggedNodeRefMut::BridgeNode(self)
    }
    fn convert_to_cell_node(&mut self) -> TrieNodeODRc<V> {
        self.convert_to_cell_node(2)
    }
}

/// Returns the number of characters shared between two slices
#[inline]
pub(crate) fn find_prefix_overlap(a: &[u8], b: &[u8]) -> usize {
    let mut cnt = 0;
    loop {
        if cnt == a.len() {break}
        if cnt == b.len() {break}
        if a[cnt] != b[cnt] {break}
        cnt += 1;
    }
    cnt
}

#[test]
fn test_bridge_node() {
    //First confirm BridgeNode is 48 bytes
    assert_eq!(std::mem::size_of::<BridgeNode::<()>>(), 48);

    //Test recursive BridgeNode creation
    let payload: ValOrChildUnion<usize> = 42.into();
    let new_node = BridgeNode::<usize>::new(b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz", false, payload);
    assert_eq!(new_node.key(), b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcde");
    assert_eq!(new_node.is_child_ptr(), true);
    let (consumed_bytes, child_node) = new_node.node_get_child(b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz").unwrap();
    assert_eq!(consumed_bytes, KEY_BYTES_CNT);
    let check_val = child_node.node_get_val(b"fghijklmnopqrstuvwxyz").unwrap();
    assert_eq!(*check_val, 42);

    //Pathological case where the key.len() is exactly KEY_BYTES_CNT
    let payload: ValOrChildUnion<usize> = 42.into();
    let new_node = BridgeNode::<usize>::new(b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcde", false, payload);
    assert_eq!(new_node.key().len(), KEY_BYTES_CNT); //To make sure the test remains valid, if KEY_BYTES_CNT is changed
    assert_eq!(new_node.key(), b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcde");
    assert_eq!(new_node.is_child_ptr(), false);
    let check_val = new_node.node_get_val(b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcde").unwrap();
    assert_eq!(*check_val, 42);
}
