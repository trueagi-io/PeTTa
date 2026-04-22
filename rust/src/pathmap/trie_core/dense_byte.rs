
use core::fmt::{Debug, Formatter};
use core::ptr;
use std::collections::HashMap;
use std::hint::unreachable_unchecked;

use super::super::alloc::Allocator;
use super::super::ring::*;
use super::super::utils::ByteMask;

use super::super::utils::BitMask;
use super::node::*;
use super::line_list::LineListNode;

//NOTE: This: `core::array::from_fn(|i| i as u8);` ought to work, but https://github.com/rust-lang/rust/issues/109341
const ALL_BYTES: [u8; 256] = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255];

/// A ByteNode with insides that **cannot** be shared across threads
pub type DenseByteNode<V, A> = ByteNode<OrdinaryCoFree<V, A>, A>;

/// A ByteNode with insides that **can** be shared across threads
pub type CellByteNode<V, A> = ByteNode<CellCoFree<V, A>, A>;

#[repr(C)]
pub struct ByteNode<Cf, A: Allocator> {
    #[cfg(feature = "slim_ptrs")]
    refcnt: std::sync::atomic::AtomicU32,
    pub mask: ByteMask,
    #[cfg(feature = "nightly")]
    values: Vec<Cf, A>,
    #[cfg(not(feature = "nightly"))]
    values: Vec<Cf>,
    alloc: A,
}

#[cfg(not(feature = "nightly"))]
#[repr(transparent)]
struct ValuesVec<Cf, A: Allocator> {
    v: Vec<Cf>,
    phantom: core::marker::PhantomData<A>
}

#[cfg(feature = "nightly")]
#[repr(transparent)]
struct ValuesVec<Cf, A: Allocator> {
    v: Vec<Cf, A>,
}

#[cfg(not(feature = "nightly"))]
impl<Cf, A: Allocator> ValuesVec<Cf, A> {
    fn default_in(_alloc: A) -> Self {
        Self{ v: vec![], phantom: core::marker::PhantomData }
    }
    fn with_capacity_in(capacity: usize, _alloc: A) -> Self {
        Self{ v: Vec::with_capacity(capacity), phantom: core::marker::PhantomData }
    }
}

#[cfg(feature = "nightly")]
impl<Cf, A: Allocator> ValuesVec<Cf, A> {
    fn default_in(alloc: A) -> Self {
        Self{ v: Vec::new_in(alloc) }
    }
    fn with_capacity_in(capacity: usize, alloc: A) -> Self {
        Self{ v: Vec::with_capacity_in(capacity, alloc) }
    }
}

impl<V: Clone + Send + Sync, A: Allocator, Cf: CoFree<V=V, A=A>> Clone for ByteNode<Cf, A> {
    fn clone(&self) -> Self {
        Self {
            #[cfg(feature = "slim_ptrs")]
            refcnt: std::sync::atomic::AtomicU32::new(1),
            mask: self.mask,
            values: self.values.clone(),
            alloc: self.alloc.clone(),
        }
    }
}

impl<V: Clone + Send + Sync, A: Allocator, Cf: CoFree<V=V, A=A>> Debug for ByteNode<Cf, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        //Recursively printing a whole tree will get pretty unwieldy.  Should do something
        // like serialization for inspection using standard tools.
        write!(f, "ByteNode {{count={}", self.values.len())?;
        self.for_each_item(|node, c, i| {
            let cf = node.values.get(i).unwrap();
            let _ = write!(f, ", {c}:(val={} child={})", cf.has_val(), cf.has_rec());
        });
        write!(f, "}}")
    }
}

impl<V: Clone + Send + Sync, A: Allocator, Cf: CoFree<V=V, A=A>> ByteNode<Cf, A> {
    #[inline]
    pub fn new_in(alloc: A) -> Self {
        Self::new_with_fields_in(ByteMask::EMPTY, ValuesVec::default_in(alloc.clone()), alloc)
    }
    #[inline]
    pub fn with_capacity_in(capacity: usize, alloc: A) -> Self {
        Self::new_with_fields_in(ByteMask::EMPTY, ValuesVec::with_capacity_in(capacity, alloc.clone()), alloc)
    }
    #[inline]
    fn new_with_fields_in(mask: ByteMask, values: ValuesVec<Cf, A>, alloc: A) -> Self {
        Self {
            #[cfg(feature = "slim_ptrs")]
            refcnt: std::sync::atomic::AtomicU32::new(1),
            mask,
            values: values.v,
            alloc,
        }
    }
    #[inline]
    pub fn reserve_capacity(&mut self, additional: usize) {
        self.values.reserve(additional)
    }
    /// Adds a new child at the specified key byte.  Replaces and returns an existing branch.
    /// Use [join_child_into] to join with the existing branch
    #[inline]
    pub fn set_child(&mut self, k: u8, node: TrieNodeODRc<V, A>) -> Option<TrieNodeODRc<V, A>> {
        let ix = self.mask.index_of(k) as usize;
        if self.mask.test_bit(k) {
            let cf = unsafe { self.values.get_unchecked_mut(ix) };
            cf.swap_rec(node)
        } else {
            self.mask.set_bit(k);
            let new_cf = CoFree::new(Some(node), None);
            self.values.insert(ix, new_cf);
            None
        }
    }

    /// The same as [set_child] if no child exists in the node at the key.  Otherwise joins the two nodes
    /// together
    #[inline]
    pub fn join_child_into(&mut self, k: u8, node: TrieNodeODRc<V, A>) -> AlgebraicStatus where V: Clone + Lattice {
        let ix = self.mask.index_of(k) as usize;
        if self.mask.test_bit(k) {
            let cf = unsafe { self.values.get_unchecked_mut(ix) };
            match cf.rec_mut() {
                Some(existing_node) => {
                    existing_node.join_into(node)
                },
                None => {
                    cf.set_rec(node);
                    AlgebraicStatus::Element
                }
            }
        } else {
            self.mask.set_bit(k);
            let new_cf = CoFree::new(Some(node), None);
            self.values.insert(ix, new_cf);
            AlgebraicStatus::Element
        }
    }

    #[inline]
    pub fn set_val(&mut self, k: u8, val: V) -> Option<V> {
        let ix = self.mask.index_of(k) as usize;
        if self.mask.test_bit(k) {
            let cf = unsafe { self.values.get_unchecked_mut(ix) };
            cf.swap_val(val)
        } else {
            self.mask.set_bit(k);
            let new_cf = CoFree::new(None, Some(val));
            self.values.insert(ix, new_cf);
            None
        }
    }

    #[inline]
    pub fn remove_val(&mut self, k: u8, prune: bool) -> Option<V> {
        if self.mask.test_bit(k) {
            let ix = self.mask.index_of(k) as usize;

            let cf = unsafe { self.values.get_unchecked_mut(ix) };
            let result = cf.take_val();

            if prune && !cf.has_rec() {
                self.mask.clear_bit(k);
                self.values.remove(ix);
            }
            result
        } else {
            None
        }
    }

    #[inline]
    pub fn set_dangling_cf(&mut self, k: u8) -> bool {
        let ix = self.mask.index_of(k) as usize;
        if self.mask.test_bit(k) {
            false
        } else {
            self.mask.set_bit(k);
            let new_cf = CoFree::new(None, None);
            self.values.insert(ix, new_cf);
            true
        }
    }

    /// Similar in behavior to [set_val], but will join v with the existing value instead of replacing it
    #[inline]
    pub fn join_val_into(&mut self, k: u8, val: V) -> AlgebraicStatus where V: Lattice {
        let ix = self.mask.index_of(k) as usize;
        if self.mask.test_bit(k) {
            let cf = unsafe { self.values.get_unchecked_mut(ix) };
            match cf.val_mut() {
                Some(existing_val) => {
                    existing_val.join_into(val)
                }
                None => {
                    cf.set_val(val);
                    AlgebraicStatus::Element
                }
            }
        } else {
            self.mask.set_bit(k);
            let new_cf = CoFree::new(None, Some(val));
            self.values.insert(ix, new_cf);
            AlgebraicStatus::Element
        }
    }

    /// Sets the payload (child node or V) at the specified key.  Should not be used in situations where
    /// a the child or value may already exist at the key
    #[inline]
    pub(crate) fn set_payload_owned(&mut self, k: u8, payload: ValOrChild<V, A>) {
        match payload {
            ValOrChild::Child(child) => {
                let _ = self.set_child(k, child);
            },
            ValOrChild::Val(val) => {
                let result = self.set_val(k, val);
                debug_assert!(result.is_none()); //For now we don't want to replace existing nodes
            }
        }
    }

    /// Behavior is the same as [set_payload_owned], if the child or value doens't already exist, otherwise
    /// joins the existing entry with the supplied payload
    #[inline]
    pub(crate) fn join_payload_into(&mut self, k: u8, payload: ValOrChild<V, A>) -> AlgebraicStatus where V: Clone + Lattice {
        match payload {
            ValOrChild::Child(child) => {
                self.join_child_into(k, child)
            },
            ValOrChild::Val(val) => {
                self.join_val_into(k, val)
            }
        }
    }

    /// Internal method to remove a CoFree from the node
    #[inline]
    fn remove(&mut self, k: u8) -> Option<Cf> {
        if self.mask.test_bit(k) {
            let ix = self.mask.index_of(k) as usize;
            let v = self.values.remove(ix);
            self.mask.clear_bit(k);
            return Some(v);
        }
        None
    }

    #[inline]
    fn get(&self, k: u8) -> Option<&Cf> {
        if self.mask.test_bit(k) {
            let ix = self.mask.index_of(k) as usize;
            // println!("pos ix {} {} {:b}", pos, ix, self.mask);
            unsafe { Some(self.values.get_unchecked(ix)) }
        } else {
            None
        }
    }

    #[inline]
    fn get_mut(&mut self, k: u8) -> Option<&mut Cf> {
        if self.mask.test_bit(k) {
            let ix = self.mask.index_of(k) as usize;
            unsafe { Some(self.values.get_unchecked_mut(ix)) }
        } else {
            None
        }
    }

    #[inline]
    fn get_child_mut(&mut self, k: u8) -> Option<&mut TrieNodeODRc<V, A>> {
        self.get_mut(k).and_then(|cf| cf.rec_mut())
    }

    #[inline]
    pub unsafe fn get_unchecked(&self, k: u8) -> &Cf {
        let ix = self.mask.index_of(k) as usize;
        // println!("pos ix {} {} {:b}", pos, ix, self.mask);
        unsafe{ self.values.get_unchecked(ix) }
    }

    #[inline]
    unsafe fn get_unchecked_mut(&mut self, k: u8) -> &mut Cf {
        let ix = self.mask.index_of(k) as usize;
        // println!("pos ix {} {} {:b}", pos, ix, self.mask);
        unsafe{ self.values.get_unchecked_mut(ix) }
    }

    #[inline]
    fn is_empty(&self) -> bool {
        self.mask.is_empty_mask()
    }
    /// Iterates the entries in `self`, calling `func` for each entry
    /// The arguments to `func` are: `func(self, key_byte, n)`, where `n` is the number of times
    /// `func` has been called prior.  This corresponds to index of the `CoFree` in the `values` vec
    #[inline]
    fn for_each_item<F: FnMut(&Self, u8, usize)>(&self, mut func: F) {
        let mut n = 0;
        for i in 0..4 {
            let mut lm = self.mask.0[i];
            while lm != 0 {
                let index = lm.trailing_zeros();

                let key_byte = 64*(i as u8) + (index as u8);
                func(self, key_byte, n);
                n += 1;

                lm ^= 1u64 << index;
            }
        }
    }
}

impl<V: Clone + Send + Sync, A: Allocator, Cf: CoFree<V=V, A=A>> ByteNode<Cf, A> where Self: TrieNodeDowncast<V, A> {

    /// Internal method to subtract nodes of an abstract type from the node
    fn psubtract_abstract(&self, other: &dyn TrieNode<V, A>) -> AlgebraicResult<TrieNodeODRc<V, A>> where V: Clone + DistributiveLattice {
        let mut is_identity = true;
        let mut new_node = Self::new_in(self.alloc.clone());

        //Go over each populated entry in the node
        self.for_each_item(|self_node, key_byte, cf_idx| {
            if other.node_contains_partial_key(&[key_byte]) {
                let cf = unsafe{ self_node.values.get_unchecked(cf_idx) };
                let mut new_cf = Cf::new(None, None);

                //If there is a value at this key_byte, and the other node contains a value, subtract them
                if let Some(self_val) = cf.val() {
                    if let Some(other_val) = other.node_get_val(&[key_byte]) {
                        match self_val.psubtract(other_val) {
                            AlgebraicResult::None => { is_identity = false; },
                            AlgebraicResult::Identity(mask) => {
                                debug_assert_eq!(mask, SELF_IDENT); //subtract is not commutative
                                new_cf.set_val(self_val.clone());
                            },
                            AlgebraicResult::Element(e) => {
                                is_identity = false;
                                new_cf.set_val(e);
                            },
                        }
                    }
                }

                //If there is an onward link, see if there is a matching link in other, and subtract them
                if let Some(self_child) = cf.rec() {
                    let self_child_tagged = self_child.as_tagged();
                    if !self_child_tagged.node_is_empty() {
                        let other_child = other.get_node_at_key(&[key_byte]);
                        match other_child.try_as_tagged() {
                            Some(other_child) => {
                                match self_child_tagged.psubtract_dyn(other_child) {
                                    AlgebraicResult::None => { is_identity = false; }
                                    AlgebraicResult::Identity(mask) => {
                                        debug_assert_eq!(mask, SELF_IDENT); //subtract is not commutative
                                        new_cf.set_rec(self_child.clone());
                                    },
                                    AlgebraicResult::Element(e) => {
                                        is_identity = false;
                                        new_cf.set_rec(e);
                                    },
                                }
                            },
                            None => {
                                new_cf.set_rec(self_child.clone())
                            }
                        }
                    }
                }

                //If we ended up with a value or a link in the CF, insert it into a new node
                if new_cf.has_rec() || new_cf.has_val() {
                    new_node.mask.set_bit(key_byte);
                    new_node.values.push(new_cf);
                }
            } else {
                new_node.mask.set_bit(key_byte);
                let cf = unsafe{ self_node.values.get_unchecked(cf_idx) };
                new_node.values.push(cf.clone());
            }
        });
        if new_node.is_empty() {
            AlgebraicResult::None
        } else {
            if is_identity {
                //NOTE: we end up throwing away a totally formed `new_node` here, but that's a much
                // better outcome than having two copies of the same node in the trie
                AlgebraicResult::Identity(SELF_IDENT)
            } else {
                AlgebraicResult::Element(TrieNodeODRc::new_in(new_node, self.alloc.clone()))
            }
        }
    }

    /// Internal method to restrict using nodes of an abstract type
    fn prestrict_abstract(&self, other: &dyn TrieNode<V, A>) -> AlgebraicResult<TrieNodeODRc<V, A>> where V: Clone {
        let mut is_identity = true;
        let mut new_node = Self::new_in(self.alloc.clone());

        //Go over each populated entry in the node
        self.for_each_item(|self_node, key_byte, cf_idx| {
            if other.node_contains_partial_key(&[key_byte]) {
                let cf = unsafe{ self_node.values.get_unchecked(cf_idx) };

                //If there is a comparable value in other, keep the whole cf
                if let Some(_) = other.node_get_val(&[key_byte]) {
                    new_node.mask.set_bit(key_byte);
                    new_node.values.push(cf.clone());
                } else {

                    //If there is an onward link in the CF and other node, continue the restriction recursively
                    if let Some(self_child) = cf.rec() {
                        let other_child = other.get_node_at_key(&[key_byte]);
                        match other_child.try_as_tagged() {
                            Some(other_child) => {
                                let mut new_cf = Cf::new(None, None);
                                match self_child.as_tagged().prestrict_dyn(other_child) {
                                    AlgebraicResult::None => { is_identity = false; }
                                    AlgebraicResult::Identity(mask) => {
                                        debug_assert_eq!(mask, SELF_IDENT); //restrict is not commutative
                                        new_cf.set_rec(self_child.clone());
                                    },
                                    AlgebraicResult::Element(e) => {
                                        is_identity = false;
                                        new_cf.set_rec(e);
                                    },
                                }
                                if new_cf.has_rec() {
                                    new_node.mask.set_bit(key_byte);
                                    new_node.values.push(new_cf);
                                }
                            },
                            None => {
                                is_identity = false;
                            }
                        }
                    }
                }
            } else {
                is_identity = false;
            }
        });
        if new_node.is_empty() {
            AlgebraicResult::None
        } else {
            if is_identity {
                AlgebraicResult::Identity(SELF_IDENT)
            } else {
                AlgebraicResult::Element(TrieNodeODRc::new_in(new_node, self.alloc.clone()))
            }
        }
    }

    /// Merges the entries in the ListNode into the ByteNode
    pub fn merge_from_list_node(&mut self, list_node: &LineListNode<V, A>) -> AlgebraicStatus where V: Clone + Lattice {
        let self_was_empty = self.is_empty();
        self.reserve_capacity(2);

        let slot0_status = if list_node.is_used::<0>() {
            let key = unsafe{ list_node.key_unchecked::<0>() };
            let payload = list_node.clone_payload::<0>().unwrap();
            if key.len() > 1 {
                let mut child_node = LineListNode::<V, A>::new_in(self.alloc.clone());
                unsafe{ child_node.set_payload_owned::<0>(&key[1..], payload); }
                self.join_child_into(key[0], TrieNodeODRc::new_in(child_node, self.alloc.clone()))
            } else {
                self.join_payload_into(key[0], payload)
            }
        } else {
            if self_was_empty {
                AlgebraicStatus::None
            } else {
                AlgebraicStatus::Identity
            }
        };

        let slot1_status = if list_node.is_used::<1>() {
            let key = unsafe{ list_node.key_unchecked::<1>() };
            let payload = list_node.clone_payload::<1>().unwrap();
            if key.len() > 1 {
                let mut child_node = LineListNode::<V, A>::new_in(self.alloc.clone());
                unsafe{ child_node.set_payload_owned::<0>(&key[1..], payload); }
                self.join_child_into(key[0], TrieNodeODRc::new_in(child_node, self.alloc.clone()))
            } else {
                self.join_payload_into(key[0], payload)
            }
        } else {
            if self_was_empty {
                AlgebraicStatus::None
            } else {
                AlgebraicStatus::Identity
            }
        };

        //Note: (true, true) makes sense in the context of a join implementation because when the rec_status or
        // val_status is None, it can only have gotten that way because the respective field was already None.
        // This is because Join will never convert Some into None, but this logic isn't portable to other ops
        slot0_status.merge(slot1_status, true, true)
    }
}

impl<V: Clone + Send + Sync, A: Allocator> CellByteNode<V, A> {

    /// Ensures that a CoFree exists for the specified key, and returns a reference to the node and
    /// value option
    ///
    /// This enables a WriteZipper to modify a specific CoFree without touching the DenseByteNode
    /// that contains it, and therefore multiple WriteZippers can be rooted at the same parent, so
    /// long as the first byte of each path is unique
    #[inline]
    pub(crate) fn prepare_cf(&mut self, k: u8) -> (&mut TrieNodeODRc<V, A>, &mut Option<V>) {
        let alloc = self.alloc.clone();
        match self.mask.test_bit(k) {
            true => {},
            false => {
                let ix = self.mask.index_of(k) as usize;
                self.mask.set_bit(k);
                let new_cf = CellCoFree::new(None, None);
                self.values.insert(ix, new_cf);
            }
        }
        let cf = self.get_mut(k).unwrap();
        let (rec, val) = cf.both_mut_refs();
        let rec = match rec {
            Some(rec) => rec,
            None => {
                *rec = Some(TrieNodeODRc::new_allocated_in(0, 0, alloc));
                rec.as_mut().unwrap()
            }
        };
        (rec, val)
    }
}

impl<V: Clone + Send + Sync, A: Allocator, Cf: CoFree<V=V, A=A>> ByteNode<Cf, A> {
    /// Internal method to recursively create all parents to support a value or branch at a given path
    #[cfg(feature = "all_dense_nodes")]
    #[inline]
    pub(crate) fn create_parent_path(&mut self, path: &[u8]) -> &mut DenseByteNode<V, A> {
        let alloc = self.alloc.clone();
        let new_node = DenseByteNode::new_in(alloc.clone());
        self.set_child(path[0], TrieNodeODRc::new_in(new_node, alloc.clone()));
        let mut cur = self.get_child_mut(path[0]).unwrap().make_mut().into_dense().unwrap();
        for i in 1..path.len() - 1 {
            let new_node = DenseByteNode::new_in(alloc.clone());
            cur.set_child(path[i], TrieNodeODRc::new_in(new_node, alloc.clone()));
            cur = cur.get_child_mut(path[i]).unwrap().make_mut().into_dense().unwrap();
        }
        cur
    }
    #[cfg(feature = "bridge_nodes")]
    /// Adds a payload (value or CF link) to a given long key, creating BridgeNodes along the way.  If the
    /// value of onward link already exists at the specified byte, it will be replaced
    pub(crate) fn add_payload(&mut self, key: &[u8], is_child: bool, payload: ValOrChildUnion<V>) {
        debug_assert!(key.len() > 0);
        //DenseByteNodes hold one byte keys, so if the key is more than 1 byte we need to
        // make an intermediate node to hold the rest of the key
        if key.len() > 1 {
            let bridge_node = super::bridge::BridgeNode::new(&key[1..], is_child, payload);
            self.set_child(key[0], TrieNodeODRc::new(bridge_node));
        } else {
            if is_child {
                let child_node = unsafe{ payload.into_child() };
                self.set_child(key[0], child_node);
            } else {
                let val = unsafe{ payload.into_val() };
                self.set_val(key[0], val);
            }
        }
    }
    #[cfg(feature = "bridge_nodes")]
    pub(crate) fn merge_payload(&mut self, key: &[u8], is_child: bool, payload: ValOrChildUnion<V>) where V: Lattice {
        debug_assert!(key.len() > 0);
        //DenseByteNodes hold one byte keys, so if the key is more than 1 byte we need to
        // make an intermediate node to hold the rest of the key
        if key.len() > 1 {
            let bridge_node = super::bridge::BridgeNode::new(&key[1..], is_child, payload);
            self.join_child_into(key[0], TrieNodeODRc::new(bridge_node));
        } else {
            if is_child {
                let child_node = unsafe{ payload.into_child() };
                self.join_child_into(key[0], child_node);
            } else {
                let val = unsafe{ payload.into_val() };
                self.join_val_into(key[0], val);
            }
        }
    }
}

impl<V: Clone + Send + Sync, A: Allocator, Cf: CoFree<V=V, A=A>> TrieNode<V, A> for ByteNode<Cf, A>
    where ByteNode<Cf, A>: TrieNodeDowncast<V, A>
{
    #[inline(always)]
    fn node_key_overlap(&self, key: &[u8]) -> usize {
        if self.mask.test_bit(key[0]) {
            1
        } else {
            0
        }
    }
    #[inline(always)]
    fn node_contains_partial_key(&self, key: &[u8]) -> bool {
        debug_assert!(key.len() > 0);
        if key.len() == 1 {
            self.mask.test_bit(key[0])
        } else {
            false
        }
    }
    #[inline(always)]
    fn node_get_child(&self, key: &[u8]) -> Option<(usize, &TrieNodeODRc<V, A>)> {
        self.get(key[0]).and_then(|cf|
            cf.rec().map(|child_node| {
                (1, child_node)
            })
        )
    }
    fn node_get_child_mut(&mut self, key: &[u8]) -> Option<(usize, &mut TrieNodeODRc<V, A>)> {
        debug_assert!(key.len() > 0);
        self.get_child_mut(key[0]).map(|child_node_ptr| {
            (1, child_node_ptr)
        })
    }
    fn node_replace_child(&mut self, key: &[u8], new_node: TrieNodeODRc<V, A>) {
        debug_assert!(key.len() == 1);
        let cf = self.get_mut(key[0]).unwrap();
        *cf.rec_mut().unwrap() = new_node;
    }
    fn node_get_payloads<'node, 'res>(&'node self, keys: &[(&[u8], bool)], results: &'res mut [(usize, PayloadRef<'node, V, A>)]) -> bool {
        //DISCUSSION: This function appears overly complicated primarily because it needs to track
        // whether or not a both the val and the rec each cofree are requested, but we don't have a bitmask
        // in advance that records vals and rec links separately.  Since we don't want nested loops, we leverage
        // the fact that a rec must be requested before a val, to stash the val for the next trip through the
        // loop.  The loop body therefore is an annoying state-machine.  But at least it's not that much code.

        //Becomes true if only half of `(Some, Some)` CoFree is requested, without requesting the other half
        // This flag never gets unset once it gets set
        let mut unrequested_cofree_half = false;
        //Temporary state that bridges across multiple requests into a `(Some, Some)` CoFree, by holding the
        // val until it's requested, leveraging the fact that values are requested after rec links
        let mut stashed_val: Option<&V> = None;
        //Tracks whether the current CoFree's val has been taken.  So, `last_byte` toggles to `Some` and stays
        // at `Some` until we move onto a different CF, while `stashed_val` toggles to `Some`, and toggles back
        // as soon as the value is requested.
        let mut last_byte: Option<u8> = None;
        //Tracks which CoFrees have yet to be requested from the node
        let mut requested_mask = ByteMask::from(self.mask);

        debug_assert!(results.len() >= keys.len());
        for ((key, expect_val), (result_key_len, payload_ref)) in keys.into_iter().zip(results.iter_mut()) {
            if key.len() > 0 {
                let byte = key[0];

                //Check to see if we had a Val from the CoFree that we aren't going to request
                match &last_byte {
                    Some(prev_byte) => {
                        if byte != *prev_byte {
                            if stashed_val.is_some() {
                                unrequested_cofree_half = true;
                            }
                            stashed_val = None;
                            last_byte = None;
                        }
                    },
                    None => {}
                }

                //Check to see if this trip through the loop is the request for the stashed val
                match stashed_val {
                    Some(val) => {
                        if key.len() == 1 && *expect_val {
                            *result_key_len = 1;
                            *payload_ref = PayloadRef::Val(val);
                            stashed_val = None;
                            continue;
                        }
                    },
                    None => {}
                }

                requested_mask.clear_bit(byte);
                match self.get(byte) {
                    Some(cf) => {
                        //A key longer than 1 byte or an explicit request for a rec link can be answered with a Child
                        if key.len() > 1 || !*expect_val {
                            match cf.rec() {
                                Some(rec) => {
                                    *result_key_len = 1;
                                    *payload_ref = PayloadRef::Child(rec);
                                },
                                None => {}
                            }
                        }
                        match cf.val() {
                            Some(val) => {
                                //Answer an explicit request for this val, or stash the val for 
                                if key.len() == 1 && *expect_val {
                                    debug_assert!(stashed_val.is_none());
                                    *result_key_len = 1;
                                    *payload_ref = PayloadRef::Val(val);
                                } else {
                                    if last_byte.is_none() {
                                        stashed_val = Some(val);
                                        last_byte = Some(byte);
                                    }
                                }
                            },
                            None => {}
                        }
                    },
                    None => {}
                }
            }
        }

        !unrequested_cofree_half && stashed_val.is_none() && requested_mask.is_empty_mask()
    }
    fn node_contains_val(&self, key: &[u8]) -> bool {
        if key.len() == 1 {
            match self.get(key[0]) {
                Some(cf) => cf.has_val(),
                None => false
            }
        } else {
            false
        }
    }
    fn node_get_val(&self, key: &[u8]) -> Option<&V> {
        if key.len() == 1 {
            self.get(key[0]).and_then(|cf| cf.val() )
        } else {
            None
        }
    }
    fn node_get_val_mut(&mut self, key: &[u8]) -> Option<&mut V> {
        if key.len() == 1 {
            self.get_mut(key[0]).and_then(|cf| cf.val_mut() )
        } else {
            None
        }
    }
    fn node_set_val(&mut self, key: &[u8], val: V) -> Result<(Option<V>, bool), TrieNodeODRc<V, A>> {
        debug_assert!(key.len() > 0);
        #[cfg(not(feature = "all_dense_nodes"))]
        {
            //Split a new node to hold everything after the first byte of the key
            if key.len() > 1 {
                #[cfg(not(feature = "bridge_nodes"))]
                {
                    let mut child = super::line_list::LineListNode::new_in(self.alloc.clone());
                    child.node_set_val(&key[1..], val).unwrap_or_else(|_| panic!());
                    self.set_child(key[0], TrieNodeODRc::new_in(child, self.alloc.clone()));
                }
                #[cfg(feature = "bridge_nodes")]
                {
                    let child = super::bridge::BridgeNode::new(&key[1..], false, val.into());
                    self.set_child(key[0], TrieNodeODRc::new(child));
                }
                Ok((None, true))
            } else {
                Ok((self.set_val(key[0], val), false))
            }
        }

        #[cfg(feature = "all_dense_nodes")]
        {
            if key.len() > 1 {
                let last_node = self.create_parent_path(key);
                Ok((last_node.set_val(key[key.len()-1], val), true))
            } else {
                Ok((self.set_val(key[key.len()-1], val), false))
            }
        }
    }
    fn node_remove_val(&mut self, key: &[u8], prune: bool) -> Option<V> {
        if key.len() == 1 {
            self.remove_val(key[0], prune)
        } else {
            None
        }
    }
    fn node_create_dangling(&mut self, key: &[u8]) -> Result<(bool, bool), TrieNodeODRc<V, A>> {
        debug_assert!(key.len() > 0);
        #[cfg(not(feature = "all_dense_nodes"))]
        {
            //Split a new node to hold everything after the first byte of the key
            if key.len() > 1 {
                {
                    let mut child = super::line_list::LineListNode::new_in(self.alloc.clone());
                    child.node_create_dangling(&key[1..]).unwrap_or_else(|_| panic!());
                    self.set_child(key[0], TrieNodeODRc::new_in(child, self.alloc.clone()));
                }
                Ok((true, true))
            } else {
                Ok((self.set_dangling_cf(key[0]), false))
            }
        }

        #[cfg(feature = "all_dense_nodes")]
        {
            if key.len() > 1 {
                let last_node = self.create_parent_path(key);
                Ok((last_node.set_dangling_cf(key[key.len()-1]), true))
            } else {
                Ok((self.set_dangling_cf(key[key.len()-1]), false))
            }
        }
    }
    fn node_remove_dangling(&mut self, key: &[u8]) -> usize {
        debug_assert!(key.len() > 0);
        if key.len() == 1 {
            let k = key[0];
            if self.mask.test_bit(k) {
                let ix = self.mask.index_of(k) as usize;
                let cf = unsafe { self.values.get_unchecked_mut(ix) };
                if !cf.has_rec() && !cf.has_val() {
                    self.mask.clear_bit(k);
                    self.values.remove(ix);
                    return 1
                }
                //Clean up empty nodes too, which may have been left by a ZipperHead
                match cf.rec() {
                    Some(node) => if node.as_tagged().node_is_empty() {
                        if cf.has_val() {
                            cf.set_rec_option(None);
                        } else {
                            self.mask.clear_bit(k);
                            self.values.remove(ix);
                            return 1
                        }
                    },
                    None => {}
                }
            }
        }
        0
    }
    fn node_set_branch(&mut self, key: &[u8], new_node: TrieNodeODRc<V, A>) -> Result<bool, TrieNodeODRc<V, A>> {
        debug_assert!(key.len() > 0);
        #[cfg(not(feature = "all_dense_nodes"))]
        {
            //Make a new ListNode to hold everything after the first byte of the key
            if key.len() > 1 {
                #[cfg(not(feature = "bridge_nodes"))]
                {
                    let mut child = super::line_list::LineListNode::new_in(self.alloc.clone());
                    child.node_set_branch(&key[1..], new_node).unwrap_or_else(|_| panic!());
                    self.set_child(key[0], TrieNodeODRc::new_in(child, self.alloc.clone()));
                }
                #[cfg(feature = "bridge_nodes")]
                {
                    let child = super::bridge::BridgeNode::new(&key[1..], true, new_node.into());
                    self.set_child(key[0], TrieNodeODRc::new(child));
                }
                Ok(true)
            } else {
                self.set_child(key[0], new_node);
                Ok(false)
            }
        }

        #[cfg(feature = "all_dense_nodes")]
        {
            if key.len() > 1 {
                let last_node = self.create_parent_path(key);
                last_node.set_child(key[key.len()-1], new_node);
                Ok(true)
            } else {
                self.set_child(key[key.len()-1], new_node);
                Ok(false)
            }
        }
    }
    fn node_remove_all_branches(&mut self, key: &[u8], prune: bool) -> bool {
        if key.len() > 1 {
            return false;
        }
        debug_assert_eq!(key.len(), 1);
        let k = key[0];
        if self.mask.test_bit(k) {
            let ix = self.mask.index_of(k) as usize;
            let cf = unsafe { self.values.get_unchecked_mut(ix) };
            match (cf.has_rec(), cf.has_val()) {
                (true, true) => {
                    cf.set_rec_option(None);
                    true
                },
                (true, false) => {
                    if prune {
                        self.values.remove(ix);
                        self.mask.clear_bit(k);
                    } else {
                        cf.set_rec_option(None);
                    }
                    true
                },
                (false, _) => {
                    false
                },
            }
        } else {
            false
        }
    }
    fn node_is_empty(&self) -> bool {
        self.values.len() == 0
    }
    #[inline(always)]
    fn new_iter_token(&self) -> u128 {
        self.mask.0[0] as u128
    }
    #[inline(always)]
    fn iter_token_for_path(&self, key: &[u8]) -> u128 {
        if key.len() != 1 {
            self.new_iter_token()
        } else {
            let k = *unsafe{ key.get_unchecked(0) } as usize;
            let idx = (k & 0b11000000) >> 6;
            let bit_i = k & 0b00111111;
            debug_assert!(idx < 4);
            let mask: u64 = if bit_i+1 < 64 {
                (0xFFFFFFFFFFFFFFFF << bit_i+1) & unsafe{ self.mask.0.get_unchecked(idx) }
            } else {
                0
            };
            ((idx as u128) << 64) | (mask as u128)
        }
    }
    #[inline(always)]
    fn next_items(&self, token: u128) -> (u128, &[u8], Option<&TrieNodeODRc<V, A>>, Option<&V>) {
        let mut i = (token >> 64) as u8;
        let mut w = token as u64;
        loop {
            if w != 0 {
                let wi = w.trailing_zeros() as u8;
                w ^= 1u64 << wi;
                let k = i*64 + wi;

                let new_token = ((i as u128) << 64) | (w as u128);
                let cf = unsafe{ self.get_unchecked(k) };
                let k = k as usize;
                return (new_token, &ALL_BYTES[k..=k], cf.rec(), cf.val())

            } else if i < 3 {
                i += 1;

                w = unsafe { *self.mask.0.get_unchecked(i as usize) };
            } else {
                return (NODE_ITER_FINISHED, &[], None, None)
            }
        }
    }
    fn node_val_count(&self, cache: &mut HashMap<u64, usize>) -> usize {
        //Discussion: These two implementations do the same thing but with a slightly different ordering of
        // the operations.  In `all_dense_nodes`, the "Branchy" impl wins.  But in a mixed-node setting, the
        // IMPL B is the winner.  My suspicion is that the ListNode's heavily branching structure leads to
        // underutilization elsewhere in the CPU so we get better instruction parallelism with IMPL B.

        //IMPL A "Branchy"
        // let mut result = 0;
        // for cf in self.values.iter() {
        //     if cf.value.is_some() {
        //         result += 1;
        //     }
        //     match &cf.rec {
        //         Some(rec) => result += rec.borrow().node_subtree_len(),
        //         None => {}
        //     }
        // }
        // result

        //IMPL B "Arithmetic"
        return self.values.iter().rfold(0, |t, cf| {
            t + cf.has_val() as usize + cf.rec().map(|r| val_count_below_node(r, cache)).unwrap_or(0)
        });
    }
    fn node_goat_val_count(&self) -> usize {
        return self.values.iter().rfold(0, |t, cf| {
            t + cf.has_val() as usize
        });
    }
    fn node_child_iter_start(&self) -> (u64, Option<&TrieNodeODRc<V, A>>) {
        for (pos, cf) in self.values.iter().enumerate() {
            match cf.rec() {
                Some(child) => {
                    return ((pos+1) as u64, Some(child))
                },
                None => {}
            }
        }
        (0, None)
    }
    fn node_child_iter_next(&self, token: u64) -> (u64, Option<&TrieNodeODRc<V, A>>) {
        for (pos, cf) in self.values[(token as usize)..].iter().enumerate() {
            match cf.rec() {
                Some(child) => {
                    return ((pos+1) as u64 + token, Some(child))
                },
                None => {}
            }
        }
        (0, None)
    }
    #[cfg(feature = "counters")]
    fn item_count(&self) -> usize {
        let mut cnt = 0;
        for cf in self.values.iter() {
            cnt += cf.has_rec() as usize;
            cnt += cf.has_val() as usize;
        }
        cnt
    }
    fn node_first_val_depth_along_key(&self, key: &[u8]) -> Option<usize> {
        debug_assert!(key.len() > 0);
        self.get(key[0]).and_then(|cf| {
            if cf.has_val() {
                Some(0)
            } else {
                None
            }
        })
    }
    fn nth_child_from_key(&self, key: &[u8], n: usize) -> (Option<u8>, Option<TaggedNodeRef<'_, V, A>>) {
        if key.len() > 0 {
            return (None, None)
        }

        // NOTE: This code was originally written to support indexing from the front or the back of
        // the list.  However, this capability can't be exposed in the higher-level interface because
        // index stability can't be guaranteed in many (any) node implementations, and index ordering
        // guarantees without cardinality don't provide muuch use and often come with an unnecessary cost
        //
        // However, This capability may be used again in the future, so I made "FORWARD" a const instead
        // of an argument for now.
        //
        // If 'forward == true` then `n` counts forward from the first element, oterwise it counts
        // backward from the last
        const FORWARD: bool = true;

        if n >= self.values.len() {
            return (None, None)
        }

        if FORWARD {
            (self.mask.indexed_bit::<FORWARD>(n), self.values[n].rec().map(|cf| cf.as_tagged()))
        } else {
            let idx = self.values.len() - n - 1;
            (self.mask.indexed_bit::<FORWARD>(n), self.values[idx].rec().map(|cf| cf.as_tagged()))
        }
    }

    fn first_child_from_key(&self, key: &[u8]) -> (Option<&[u8]>, Option<TaggedNodeRef<'_, V, A>>) {
        debug_assert_eq!(key.len(), 0);
        debug_assert!(self.values.len() > 0);

        let cf = unsafe{ self.values.get_unchecked(0) };
        let prefix = self.mask.indexed_bit::<true>(0).unwrap() as usize;
        (Some(&ALL_BYTES[prefix..=prefix]), cf.rec().map(|cf| cf.as_tagged()))
    }

    fn node_remove_unmasked_branches(&mut self, key: &[u8], mask: ByteMask, _prune: bool) {
        debug_assert!(key.len() == 0);
        // in the future we can use `drain_filter`, but that's experimental
        let mut lead = 0;
        let mut differs = false;
        let mut c = 0;
        let mvalues = self.values.as_ptr().cast_mut();

        unsafe {
            for i in 0..4 {
                let mut lm = self.mask.0[i];
                while lm != 0 {
                    let index = lm.trailing_zeros();
                    if ((1u64 << index) & mask.0[i]) != 0 {
                        if differs { ptr::copy_nonoverlapping(mvalues.add(c), mvalues.add(lead), 1); }
                        lead += 1;
                    } else {
                        ptr::drop_in_place(mvalues.add(c));
                        differs = true;
                    }
                    lm ^= 1u64 << index;
                    c += 1;
                }
            }

            self.values.set_len(lead);
        }
        self.mask &= mask;
    }

    #[inline(always)]
    fn node_branches_mask(&self, key: &[u8]) -> ByteMask {
        match key.len() {
            0 => self.mask,
            _ => {
                //There are two ways we could get a length >= 1 key passed in. 1. The entry is a lone value (no children in the CF) or 2. The entry doesn't exist.  Either way, there are no onward child paths
                debug_assert!({
                    match self.get(key[0]).and_then(|cf| cf.rec()) {
                        Some(child_node) => child_node.as_tagged().node_is_empty(),
                        None => true,
                    }
                });
                ByteMask::EMPTY
            },
        }
    }

    #[inline(always)]
    fn count_branches(&self, key: &[u8]) -> usize {
        match key.len() {
            0 => self.values.len(),
            _ => {
                //There are two ways we could get a length >=1 key passed in. 1. The entry is a lone value (no children in the CF) or 2. The entry doesn't exist.  Either way, there are no onward child paths
                debug_assert!({
                    match self.get(key[0]).and_then(|cf| cf.rec()) {
                        Some(child_node) => child_node.as_tagged().node_is_empty(),
                        None => true,
                    }
                });
                0
            }
        }
    }

    fn prior_branch_key<'key>(&self, key: &'key [u8]) -> &'key [u8] {
        debug_assert!(key.len() >= 1);
        if key.len() == 1 {
            &[]
        } else {
            let k = key[0];
            if self.mask.test_bit(k) {
                &key[0..1]
            } else {
                &[]
            }
        }
    }

    fn get_sibling_of_child(&self, key: &[u8], next: bool) -> (Option<u8>, Option<TaggedNodeRef<'_, V, A>>) {
        if key.len() != 1 {
            return (None, None)
        }
        let k = key[0];
        let mut mask_i = ((k & 0b11000000) >> 6) as usize;
        let bit_i = k & 0b00111111;
        // println!("k {k}");

        let mut n = bit_sibling(bit_i, self.mask.0[mask_i], !next);
        // println!("{} {bit_i} {mask_i}", n == bit_i);
        if n == bit_i { // outside of word
            loop {
                if next { mask_i += 1 } else { mask_i -= 1 };
                if !(mask_i < 4) { return (None, None) }
                if self.mask.0[mask_i] == 0 { continue }
                n = self.mask.0[mask_i].trailing_zeros() as u8; break;
            }
        }

        // println!("{} {bit_i} {mask_i}", n == bit_i);
        // println!("{:?}", parent.items().map(|(k, _)| k).collect::<Vec<_>>());
        let sibling_key_char = n | ((mask_i << 6) as u8);
        // println!("candidate {}", sk);
        debug_assert!(self.mask.test_bit(sibling_key_char));
        let cf = unsafe{ self.get_unchecked(sibling_key_char) };
        (Some(sibling_key_char), cf.rec().map(|node| node.as_tagged()))
    }

    fn get_node_at_key(&self, key: &[u8]) -> AbstractNodeRef<'_, V, A> {
        if key.len() < 2 {
            if key.len() == 0 {
                if !self.node_is_empty() {
                    AbstractNodeRef::BorrowedDyn(self.as_tagged())
                } else {
                    AbstractNodeRef::None
                }
            } else {
                match self.get(key[0]).and_then(|cf| cf.rec()) {
                    Some(link) => AbstractNodeRef::BorrowedRc(link),
                    None => AbstractNodeRef::None
                }
            }
        } else {
            AbstractNodeRef::None
        }
    }

    fn take_node_at_key(&mut self, key: &[u8], prune: bool) -> Option<TrieNodeODRc<V, A>> {
        if key.len() < 2 {
            debug_assert!(key.len() == 1);
            let k = key[0];
            if self.mask.test_bit(k) {
                let ix = self.mask.index_of(k) as usize;

                let cf = unsafe { self.values.get_unchecked_mut(ix) };
                let result = cf.take_rec();

                if prune && !cf.has_val() {
                    self.mask.clear_bit(k);
                    self.values.remove(ix);
                }
                result
            } else {
                None
            }
        } else {
            None
        }
    }

    fn pjoin_dyn(&self, other: TaggedNodeRef<V, A>) -> AlgebraicResult<TrieNodeODRc<V, A>> where V: Lattice {
        match other.tag() {
            DENSE_BYTE_NODE_TAG => {
                let other_dense_node = unsafe{ other.as_dense_unchecked() };
                self.pjoin(other_dense_node).map(|new_node| TrieNodeODRc::new_in(new_node, self.alloc.clone()))
            },
            LINE_LIST_NODE_TAG => {
                let other_list_node = unsafe{ other.as_list_unchecked() };
                let mut new_node = self.clone();
                let status = new_node.merge_from_list_node(other_list_node);
                AlgebraicResult::from_status(status, || TrieNodeODRc::new_in(new_node, self.alloc.clone()))
            },
            #[cfg(feature = "bridge_nodes")]
            TaggedNodeRef::BridgeNode(other_bridge_node) => {
                let mut new_node = self.clone();
                debug_assert!(!other_bridge_node.is_empty());
                new_node.merge_payload(other_bridge_node.key(), other_bridge_node.is_child_ptr(), other_bridge_node.clone_payload());
                TrieNodeODRc::new(new_node)
            },
            CELL_BYTE_NODE_TAG => {
                let other_byte_node = unsafe{ other.as_cell_unchecked() };
                self.pjoin(other_byte_node).map(|new_node| TrieNodeODRc::new_in(new_node, self.alloc.clone()))
            },
            TINY_REF_NODE_TAG => {
                let tiny_node = unsafe{ other.as_tiny_unchecked() };
                tiny_node.pjoin_dyn(self.as_tagged())
            }
            EMPTY_NODE_TAG => {
                AlgebraicResult::Identity(SELF_IDENT)
            },
            _ => unsafe{ std::hint::unreachable_unchecked() }
        }
    }

    fn join_into_dyn(&mut self, mut other: TrieNodeODRc<V, A>) -> (AlgebraicStatus, Result<(), TrieNodeODRc<V, A>>) where V: Lattice {
        let other_node = other.make_mut();
        let status = match other_node.tag() {
            DENSE_BYTE_NODE_TAG => {
                let other_dense_node = unsafe{ other_node.into_dense_unchecked() };
                let mut taken_node = DenseByteNode::<V, A>::new_in(self.alloc.clone());
                core::mem::swap(other_dense_node, &mut taken_node);
                self.join_into(taken_node)
            },
            LINE_LIST_NODE_TAG => {
                let other_list_node = unsafe{ other_node.into_list_unchecked() };
                //GOAT, optimization opportunity to take the contents from the list, rather than cloning
                // them, to turn around and drop the ListNode and free them / decrement the refcounts
                self.merge_from_list_node(other_list_node)
            },
            #[cfg(feature = "bridge_nodes")]
            TaggedNodeRefMut::BridgeNode(_other_bridge_node) => {
                unimplemented!()
            },
            CELL_BYTE_NODE_TAG => {
                let other_byte_node = unsafe{ other_node.into_cell_unchecked() };
                let mut taken_node = CellByteNode::<V, A>::new_in(self.alloc.clone());
                core::mem::swap(other_byte_node, &mut taken_node);
                self.join_into(taken_node)
            },
            _ => { unsafe{ unreachable_unchecked() } }
        };
        (status, Ok(()))
    }

    fn drop_head_dyn(&mut self, byte_cnt: usize) -> Option<TrieNodeODRc<V, A>> where V: Lattice {
        match self.values.len() {
            0 => { None },
            1 => {
                //WARNING: Don't be tempted to swap the node itself with its first child.  This feels like it
                // might be an optimization, but it would be a memory leak because the other node will now
                // hold an Rc to itself.
                match self.values.pop().unwrap().into_rec() {
                    Some(mut child) => {
                        if byte_cnt > 1 {
                            child.make_mut().drop_head_dyn(byte_cnt-1)
                        } else {
                            Some(child)
                        }
                    },
                    None => None
                }
            },
            _ => {
                let mut new_node = Self::new_in(self.alloc.clone());
                while let Some(cf) = self.values.pop() {
                    let child = if byte_cnt > 1 {
                        cf.into_rec().and_then(|mut child| child.make_mut().drop_head_dyn(byte_cnt-1))
                    } else {
                        cf.into_rec()
                    };
                    match child {
                        Some(child) => {
                            let (_status, result) = new_node.join_into_dyn(child);
                            debug_assert!(result.is_ok());
                        },
                        None => {}
                    }
                }

                if !new_node.is_empty() {
                    Some(TrieNodeODRc::new_in(new_node, self.alloc.clone()))
                } else {
                    None
                }
            }
        }
    }

    fn pmeet_dyn(&self, other: TaggedNodeRef<V, A>) -> AlgebraicResult<TrieNodeODRc<V, A>> where V: Lattice {
        match other.tag() {
            DENSE_BYTE_NODE_TAG => {
                let other_dense_node = unsafe { other.as_dense_unchecked() };
                self.pmeet(other_dense_node).map(|new_node| TrieNodeODRc::new_in(new_node, self.alloc.clone()))
            },
            LINE_LIST_NODE_TAG => {
                let other_list_node = unsafe { other.as_list_unchecked() };
                other_list_node.pmeet_dyn(self.as_tagged()).invert_identity()
            },
            #[cfg(feature = "bridge_nodes")]
            TaggedNodeRef::BridgeNode(other_bridge_node) => {
                other_bridge_node.pmeet_dyn(self).invert_identity()
            },
            CELL_BYTE_NODE_TAG => {
                let other_byte_node = unsafe { other.as_cell_unchecked() };
                self.pmeet(other_byte_node).map(|new_node| TrieNodeODRc::new_in(new_node, self.alloc.clone()))
            },
            TINY_REF_NODE_TAG => {
                let tiny_node = unsafe { other.as_tiny_unchecked() };
                tiny_node.pmeet_dyn(self.as_tagged()).invert_identity()
            },
            EMPTY_NODE_TAG => AlgebraicResult::None,
            _ => unsafe{ unreachable_unchecked() }
        }
    }

    fn psubtract_dyn(&self, other: TaggedNodeRef<V, A>) -> AlgebraicResult<TrieNodeODRc<V, A>> where V: DistributiveLattice {
        match other.tag() {
            DENSE_BYTE_NODE_TAG => {
                let other_dense_node = unsafe { other.as_dense_unchecked() };
                self.psubtract(other_dense_node).map(|new_node| TrieNodeODRc::new_in(new_node, self.alloc.clone()))
            },
            LINE_LIST_NODE_TAG => {
                let other_list_node = unsafe { other.as_list_unchecked() };
                self.psubtract_abstract(other_list_node)
            },
            #[cfg(feature = "bridge_nodes")]
            TaggedNodeRef::BridgeNode(other_bridge_node) => {
                self.psubtract_abstract(other_bridge_node)
            },
            CELL_BYTE_NODE_TAG => {
                let other_byte_node = unsafe { other.as_cell_unchecked() };
                self.psubtract(other_byte_node).map(|new_node| TrieNodeODRc::new_in(new_node, self.alloc.clone()))
            },
            TINY_REF_NODE_TAG => {
                let tiny_node = unsafe { other.as_tiny_unchecked() };
                self.psubtract_abstract(tiny_node)
            },
            EMPTY_NODE_TAG => AlgebraicResult::Identity(SELF_IDENT),
            _ => unsafe{ unreachable_unchecked() }
        }
    }

    fn prestrict_dyn(&self, other: TaggedNodeRef<V, A>) -> AlgebraicResult<TrieNodeODRc<V, A>> {
        match other.tag() {
            DENSE_BYTE_NODE_TAG => {
                let other_dense_node = unsafe { other.as_dense_unchecked() };
                self.prestrict(other_dense_node).map(|node| TrieNodeODRc::new_in(node, self.alloc.clone()))
            },
            LINE_LIST_NODE_TAG => {
                let other_list_node = unsafe { other.as_list_unchecked() };
                self.prestrict_abstract(other_list_node)
            },
            #[cfg(feature = "bridge_nodes")]
            TaggedNodeRef::BridgeNode(other_bridge_node) => {
                self.prestrict_abstract(other_bridge_node)
            },
            CELL_BYTE_NODE_TAG => {
                let other_byte_node = unsafe { other.as_cell_unchecked() };
                self.prestrict(other_byte_node).map(|node| TrieNodeODRc::new_in(node, self.alloc.clone()))
            },
            TINY_REF_NODE_TAG => {
                let tiny_node = unsafe { other.as_tiny_unchecked() };
                self.prestrict_abstract(tiny_node)
            },
            EMPTY_NODE_TAG => AlgebraicResult::None,
            _ => unsafe{ unreachable_unchecked() }
        }
    }
    fn clone_self(&self) -> TrieNodeODRc<V, A> {
        TrieNodeODRc::new_in(self.clone(), self.alloc.clone())
    }
}

impl<V: Clone + Send + Sync, A: Allocator> TrieNodeDowncast<V, A> for ByteNode<OrdinaryCoFree<V, A>, A> {
    #[inline]
    fn tag(&self) -> usize {
        DENSE_BYTE_NODE_TAG
    }
    #[inline(always)]
    fn as_tagged(&self) -> TaggedNodeRef<'_, V, A> {
        TaggedNodeRef::from_dense(self)
    }
    #[cfg(not(feature="slim_ptrs"))]
    #[inline]
    fn as_tagged_mut(&mut self) -> TaggedNodeRefMut<'_, V, A> {
        TaggedNodeRefMut::DenseByteNode(self)
    }
    fn convert_to_cell_node(&mut self) -> TrieNodeODRc<V, A> {
        let mut replacement_node = CellByteNode::<V, A>::with_capacity_in(self.values.len(), self.alloc.clone());
        debug_assert_eq!(replacement_node.mask, [0u64; 4]);
        core::mem::swap(&mut replacement_node.mask, &mut self.mask);
        let mut values = ValuesVec::default_in(self.alloc.clone());
        core::mem::swap(&mut values.v, &mut self.values);
        for cf in values.v.into_iter() {
            replacement_node.values.push(cf.into())
        }
        TrieNodeODRc::new_in(replacement_node, self.alloc.clone())
    }
}

impl<V: Clone + Send + Sync, A: Allocator> TrieNodeDowncast<V, A> for ByteNode<CellCoFree<V, A>, A> {
    #[inline]
    fn tag(&self) -> usize {
        CELL_BYTE_NODE_TAG
    }
    fn as_tagged(&self) -> TaggedNodeRef<'_, V, A> {
        TaggedNodeRef::from_cell(self)
    }
    #[cfg(not(feature="slim_ptrs"))]
    fn as_tagged_mut(&mut self) -> TaggedNodeRefMut<'_, V, A> {
        TaggedNodeRefMut::CellByteNode(self)
    }
    fn convert_to_cell_node(&mut self) -> TrieNodeODRc<V, A> {
        //Already is a cell_node, and that fact should have been detected before calling this method
        unreachable!()
    }
}

/// returns the position of the next/previous active bit in x
/// if there is no next/previous bit, returns the argument position
/// assumes that pos is active in x
pub(crate) fn bit_sibling(pos: u8, x: u64, next: bool) -> u8 {
    debug_assert_ne!((1u64 << pos) & x, 0);
    if next {
        if pos == 0 { return 0 } // resolves overflow in shift
        let succ = !0u64 >> (64 - pos);
        let m = x & succ;
        if m == 0u64 { pos }
        else { (63 - m.leading_zeros()) as u8 }
    } else {
        let prec = !(!0u64 >> (63 - pos));
        let m = x & prec;
        if m == 0u64 { pos }
        else { m.trailing_zeros() as u8 }
    }
}

pub trait CoFree: Clone + Default + Send + Sync {
    type V: Clone + Send + Sync;
    type A: Allocator;
    fn new(rec: Option<TrieNodeODRc<Self::V, Self::A>>, val: Option<Self::V>) -> Self;
    fn from_cf<OtherCf: CoFree<V=Self::V, A=Self::A>>(cf: OtherCf) -> Self;
    fn rec(&self) -> Option<&TrieNodeODRc<Self::V, Self::A>>;
    fn has_rec(&self) -> bool;
    fn rec_mut(&mut self) -> Option<&mut TrieNodeODRc<Self::V, Self::A>>;
    fn take_rec(&mut self) -> Option<TrieNodeODRc<Self::V, Self::A>>;
    fn into_rec(self) -> Option<TrieNodeODRc<Self::V, Self::A>>;
    fn set_rec(&mut self, node: TrieNodeODRc<Self::V, Self::A>);
    fn set_rec_option(&mut self, rec: Option<TrieNodeODRc<Self::V, Self::A>>);
    fn swap_rec(&mut self, node: TrieNodeODRc<Self::V, Self::A>) -> Option<TrieNodeODRc<Self::V, Self::A>>;
    fn val(&self) -> Option<&Self::V>;
    fn has_val(&self) -> bool;
    fn val_mut(&mut self) -> Option<&mut Self::V>;
    fn set_val(&mut self, val: Self::V);
    fn set_val_option(&mut self, val: Option<Self::V>);
    fn swap_val(&mut self, val: Self::V) -> Option<Self::V>;
    fn take_val(&mut self) -> Option<Self::V>;
    fn both_mut(&mut self) -> (Option<&mut TrieNodeODRc<Self::V, Self::A>>, Option<&mut Self::V>);
    fn into_both(self) -> (Option<TrieNodeODRc<Self::V, Self::A>>, Option<Self::V>);
}

trait CfShared<OtherCf, A: Allocator>: CoFree {
    /// Integrates the results from separate operations on the rec pointer and the value into a single result on
    /// the entire CoFree
    fn combine_algebraic_results(&self, other: &OtherCf, rec: AlgebraicResult<Option<TrieNodeODRc<Self::V, A>>>, val: AlgebraicResult<Option<Self::V>>) -> AlgebraicResult<Self>;
}

#[derive(Clone, Debug)]
pub struct OrdinaryCoFree<V: Clone + Send + Sync, A: Allocator> {
    rec: Option<TrieNodeODRc<V, A>>,
    value: Option<V>
}

impl<V: Clone + Send + Sync, A: Allocator> Default for OrdinaryCoFree<V, A> {
    fn default() -> Self {
        Self {rec: None, value: None}
    }
}

impl<V: Clone + Send + Sync, A: Allocator> OrdinaryCoFree<V, A> {
    fn both_mut_refs(&mut self) -> (&mut Option<TrieNodeODRc<V, A>>, &mut Option<V>) {
        let rec = &mut self.rec;
        let val = &mut self.value;
        (rec, val)
    }
}

impl<V: Clone + Send + Sync, A: Allocator> CoFree for OrdinaryCoFree<V, A> {
    type V = V;
    type A = A;
    fn new(rec: Option<TrieNodeODRc<V, A>>, val: Option<V>) -> Self {
        Self { rec: rec, value: val, }
    }
    fn from_cf<OtherCf: CoFree<V=Self::V, A=Self::A>>(cf: OtherCf) -> Self {
        let (rec, val) = cf.into_both();
        Self { rec: rec, value: val, }
    }
    fn rec(&self) -> Option<&TrieNodeODRc<V, A>> {
        self.rec.as_ref()
    }
    fn has_rec(&self) -> bool {
        self.rec.is_some()
    }
    fn rec_mut(&mut self) -> Option<&mut TrieNodeODRc<V, A>> {
        self.rec.as_mut()
    }
    fn take_rec(&mut self) -> Option<TrieNodeODRc<V, A>> {
        core::mem::take(&mut self.rec)
    }
    fn into_rec(self) -> Option<TrieNodeODRc<V, A>> {
        self.rec
    }
    fn set_rec(&mut self, node: TrieNodeODRc<V, A>) {
        self.rec = Some(node)
    }
    fn set_rec_option(&mut self, rec: Option<TrieNodeODRc<V, A>>) {
        self.rec = rec
    }
    fn swap_rec(&mut self, node: TrieNodeODRc<V, A>) -> Option<TrieNodeODRc<V, A>> {
        let mut old_child = Some(node);
        core::mem::swap(&mut old_child, &mut self.rec);
        old_child
    }
    fn val(&self) -> Option<&V> {
        self.value.as_ref()
    }
    fn has_val(&self) -> bool {
        self.value.is_some()
    }
    fn val_mut(&mut self) -> Option<&mut V> {
        self.value.as_mut()
    }
    fn set_val(&mut self, val: V) {
        self.value = Some(val)
    }
    fn set_val_option(&mut self, val: Option<V>) {
        self.value = val
    }
    fn swap_val(&mut self, val: V) -> Option<V> {
        let mut old_val = Some(val);
        core::mem::swap(&mut old_val, &mut self.value);
        old_val
    }
    fn take_val(&mut self) -> Option<V> {
        core::mem::take(&mut self.value)
    }
    fn both_mut(&mut self) -> (Option<&mut TrieNodeODRc<V, A>>, Option<&mut V>) {
        (self.rec.as_mut(), self.value.as_mut())
    }
    fn into_both(self) -> (Option<TrieNodeODRc<V, A>>, Option<V>) {
        (self.rec, self.value)
    }
}

use maybe_dangling::MaybeDangling;
use core::pin::Pin;

#[derive(Debug)]
pub struct CellCoFree<V: Clone + Send + Sync, A: Allocator>(MaybeDangling<Pin<Box<OrdinaryCoFree<V, A>>>>);

unsafe impl<V: Clone + Send + Sync, A: Allocator> Send for CellCoFree<V, A> {}
unsafe impl<V: Clone + Send + Sync, A: Allocator> Sync for CellCoFree<V, A> {}

impl<V: Clone + Send + Sync, A: Allocator> Default for CellCoFree<V, A> {
    fn default() -> Self {
        Self(MaybeDangling::new(Box::pin(OrdinaryCoFree::new(None, None))))
    }
}

impl<V: Clone + Send + Sync, A: Allocator> Clone for CellCoFree<V, A> {
    fn clone(&self) -> Self {
        let cloned = (&*self.0 as &OrdinaryCoFree<V, A>).clone();
        Self(MaybeDangling::new(Box::pin(cloned)))
    }
}

impl<V: Clone + Send + Sync, A: Allocator> From<OrdinaryCoFree<V, A>> for CellCoFree<V, A> {
    fn from(cf: OrdinaryCoFree<V, A>) -> Self {
        let (rec, val) = cf.into_both();
        Self::new(rec, val)
    }
}

impl<V: Clone + Send + Sync, A: Allocator> CellCoFree<V, A> {
    fn both_mut_refs(&mut self) -> (&mut Option<TrieNodeODRc<V, A>>, &mut Option<V>) {
        unsafe{ self.0.as_mut().get_unchecked_mut() }.both_mut_refs()
    }
}

impl<V: Clone + Send + Sync, A: Allocator> CoFree for CellCoFree<V, A> {
    type V = V;
    type A = A;
    fn new(rec: Option<TrieNodeODRc<V, A>>, val: Option<V>) -> Self {
        let insides = OrdinaryCoFree::new(rec, val);
        Self(MaybeDangling::new(Box::pin(insides)))
    }
    fn from_cf<OtherCf: CoFree<V=Self::V, A=Self::A>>(cf: OtherCf) -> Self {
        let (rec, val) = cf.into_both();
        Self::new(rec, val)
    }
    fn rec(&self) -> Option<&TrieNodeODRc<V, A>> {
        self.0.rec()
    }
    fn has_rec(&self) -> bool {
        self.0.has_rec()
    }
    fn rec_mut(&mut self) -> Option<&mut TrieNodeODRc<V, A>> {
        unsafe{ self.0.as_mut().get_unchecked_mut() }.rec_mut()
    }
    fn take_rec(&mut self) -> Option<TrieNodeODRc<V, A>> {
        unsafe{ self.0.as_mut().get_unchecked_mut() }.take_rec()
    }
    fn into_rec(self) -> Option<TrieNodeODRc<V, A>> {
        unsafe{ Pin::into_inner_unchecked(MaybeDangling::into_inner(self.0)) }.take_rec()
    }
    fn set_rec(&mut self, node: TrieNodeODRc<V, A>) {
        unsafe{ self.0.as_mut().get_unchecked_mut() }.set_rec(node)
    }
    fn set_rec_option(&mut self, rec: Option<TrieNodeODRc<V, A>>) {
        unsafe{ self.0.as_mut().get_unchecked_mut() }.set_rec_option(rec)
    }
    fn swap_rec(&mut self, node: TrieNodeODRc<V, A>) -> Option<TrieNodeODRc<V, A>> {
        unsafe{ self.0.as_mut().get_unchecked_mut() }.swap_rec(node)
    }
    fn val(&self) -> Option<&V> {
        self.0.val()
    }
    fn has_val(&self) -> bool {
        self.0.has_val()
    }
    fn val_mut(&mut self) -> Option<&mut V> {
        unsafe{ self.0.as_mut().get_unchecked_mut() }.val_mut()
    }
    fn take_val(&mut self) -> Option<V> {
        unsafe{ self.0.as_mut().get_unchecked_mut() }.take_val()
    }
    fn set_val(&mut self, val: V) {
        unsafe{ self.0.as_mut().get_unchecked_mut() }.set_val(val)
    }
    fn set_val_option(&mut self, val: Option<V>) {
        unsafe{ self.0.as_mut().get_unchecked_mut() }.set_val_option(val)
    }
    fn swap_val(&mut self, val: V) -> Option<V> {
        unsafe{ self.0.as_mut().get_unchecked_mut() }.swap_val(val)
    }
    fn both_mut(&mut self) -> (Option<&mut TrieNodeODRc<V, A>>, Option<&mut V>) {
        unsafe{ self.0.as_mut().get_unchecked_mut() }.both_mut()
    }
    fn into_both(self) -> (Option<TrieNodeODRc<V, A>>, Option<V>) {
        unsafe{ Pin::into_inner_unchecked(MaybeDangling::into_inner(self.0)) }.into_both()
    }
}

impl<V: Clone + Send + Sync + Lattice, A: Allocator, Cf: CoFree<V=V, A=A>, OtherCf: CoFree<V=V, A=A>> HeteroLattice<OtherCf> for Cf {
    fn pjoin(&self, other: &OtherCf) -> AlgebraicResult<Self> {
        let rec_result = self.rec().pjoin(&other.rec()).flatten();
        let val_result = self.val().pjoin(&other.val()).flatten();
        rec_result.merge(val_result, |which_arg| {
            match which_arg {
                0 => self.rec().cloned(),
                1 => other.rec().cloned(),
                _ => unreachable!()
            }
        }, |which_arg| {
            match which_arg {
                0 => self.val().cloned(),
                1 => other.val().cloned(),
                _ => unreachable!()
            }
        }, |rec, val| AlgebraicResult::Element(Self::new(rec, val)))
    }
    fn join_into(&mut self, other: OtherCf) -> AlgebraicStatus {
        let (other_rec, other_val) = other.into_both();
        let rec_status = match self.rec_mut() {
            Some(self_rec) => match other_rec {
                Some(other_rec) => {
                    let (status, result) = self_rec.make_mut().join_into_dyn(other_rec);
                    match result {
                        Ok(()) => {},
                        Err(replacement_node) => {*self_rec = replacement_node},
                    }
                    status
                },
                None => AlgebraicStatus::Identity,
            },
            None => match other_rec {
                Some(_) => {
                    self.set_rec_option(other_rec);
                    AlgebraicStatus::Element
                },
                None => AlgebraicStatus::None
            }
        };
        let val_status = match self.val_mut() {
            Some(self_val) => match other_val {
                Some(other_val) => self_val.join_into(other_val),
                None => AlgebraicStatus::Identity,
            },
            None => match other_val {
                Some(_) => {
                    self.set_val_option(other_val);
                    AlgebraicStatus::Element
                },
                None => AlgebraicStatus::None
            }
        };
        //Note: (true, true) makes sense in the context of a join implementation because when the rec_status or
        // val_status is None, it can only have gotten that way because the respective CF field was already None.
        // This is because Join will never convert Some into None, but this logic isn't portable to other ops
        rec_status.merge(val_status, true, true)
    }
    fn pmeet(&self, other: &OtherCf) -> AlgebraicResult<Self> {
        //If one or the other cofree is dangling, it's an identity result for the dangling cofree
        let mut identity_flag = 0;
        if !self.has_rec() && !self.has_val() {identity_flag = SELF_IDENT;}
        if !other.has_rec() && !other.has_val() {identity_flag |= COUNTER_IDENT;}
        if identity_flag > 0 {
            return AlgebraicResult::Identity(identity_flag)
        }

        //Otherwise actually work with what the cofrees contain
        let rec = self.rec().pmeet(&other.rec());
        let val = self.val().pmeet(&other.val());
        self.combine_algebraic_results(other, rec, val)
    }
    //GOAT, HeteroLattice will totally disappear when we do the policy refactor
    // fn join_all(_xs: &[&Self]) -> Self where Self: Sized {
    //     unreachable!() //Currently not used
    // }
    fn convert(other: OtherCf) -> Self {
        Self::from_cf(other)
    }
}

impl<V: Clone + Send + Sync + DistributiveLattice, A: Allocator, Cf: CoFree<V=V, A=A>, OtherCf: CoFree<V=V, A=A>> HeteroDistributiveLattice<OtherCf> for Cf {
    fn psubtract(&self, other: &OtherCf) -> AlgebraicResult<Self> where Self: Sized {
        let self_rec = self.rec().filter(|child| !child.as_tagged().node_is_empty());
        let rec = self_rec.psubtract(&other.rec());
        let val = self.val().psubtract(&other.val());
        self.combine_algebraic_results(other, rec, val)
    }
}

impl<V: Clone + Send + Sync, A: Allocator, Cf: CoFree<V=V, A=A>, OtherCf: CoFree<V=V, A=A>> HeteroQuantale<OtherCf> for Cf {
    fn prestrict(&self, other: &OtherCf) -> AlgebraicResult<Self> {
        if other.has_val() { AlgebraicResult::Identity(SELF_IDENT) }
        else {
            match (self.rec(), other.rec()) {
                (Some(l), Some(r)) => {
                    match l.prestrict(r) {
                        AlgebraicResult::Identity(mask) => {
                            debug_assert_eq!(mask, SELF_IDENT); //restrict is not commutative
                            if self.has_val() {
                                //We need to strip off the value of a recursive branch in the lmap,
                                // without a corresponding value in the rmap
                                AlgebraicResult::Element(CoFree::new(Some(l.clone()), None))
                            } else {
                                AlgebraicResult::Identity(SELF_IDENT)
                            }
                        },
                        AlgebraicResult::None => AlgebraicResult::None,
                        AlgebraicResult::Element(node) => AlgebraicResult::Element(CoFree::new(Some(node), None)),
                    }
                }
                _ => { AlgebraicResult::None }
            }
        }
    }
}

impl<V: Clone + Send + Sync, A: Allocator, Cf: CoFree<V=V, A=A>, OtherCf: CoFree<V=V, A=A>> CfShared<OtherCf, A> for Cf {
    #[inline]
    fn combine_algebraic_results(&self, other: &OtherCf, rec: AlgebraicResult<Option<TrieNodeODRc<Self::V, Self::A>>>, val: AlgebraicResult<Option<Self::V>>) -> AlgebraicResult<Self> {
        match (rec, val) {
            (AlgebraicResult::None, AlgebraicResult::None) => AlgebraicResult::None,
            (AlgebraicResult::Identity(rec_mask), AlgebraicResult::Identity(val_mask)) => {
                debug_assert!(rec_mask & (SELF_IDENT | COUNTER_IDENT) > 0);
                debug_assert!(val_mask & (SELF_IDENT | COUNTER_IDENT) > 0);
                let new_mask = rec_mask & val_mask;
                if new_mask > 0 {
                    AlgebraicResult::Identity(new_mask)
                } else {
                    let rec = if rec_mask & SELF_IDENT > 0 {
                        self.rec().cloned()
                    } else {
                        other.rec().cloned()
                    };
                    let val = if val_mask & SELF_IDENT > 0 {
                        self.val().cloned()
                    } else {
                        other.val().cloned()
                    };
                    AlgebraicResult::Element(Self::new(rec, val))
                }
            },
            (AlgebraicResult::None, AlgebraicResult::Identity(val_mask)) => {
                let mut new_mask = val_mask;
                if !self.rec().is_none() {
                    new_mask &= !SELF_IDENT;
                }
                if !other.rec().is_none() {
                    new_mask &= !COUNTER_IDENT;
                }
                if new_mask > 0 {
                    AlgebraicResult::Identity(new_mask)
                } else {
                    AlgebraicResult::Element(Self::new(None, self.val().cloned()))
                }
            },
            (AlgebraicResult::Identity(rec_mask), AlgebraicResult::None) => {
                let mut new_mask = rec_mask;
                if !self.val().is_none() {
                    new_mask &= !SELF_IDENT;
                }
                if !other.val().is_none() {
                    new_mask &= !COUNTER_IDENT;
                }
                if new_mask > 0 {
                    AlgebraicResult::Identity(new_mask)
                } else {
                    AlgebraicResult::Element(Self::new(self.rec().cloned(), None))
                }
            },
            (rec_el, val_el) => {
                let rec = rec_el.flatten().map_into_option(|arg_idx| {
                    match arg_idx {
                        0 => self.rec().cloned(),
                        1 => other.rec().cloned(),
                        _ => unreachable!()
                    }
                });
                let val = val_el.flatten().map_into_option(|arg_idx| {
                    match arg_idx {
                        0 => self.val().cloned(),
                        1 => other.val().cloned(),
                        _ => unreachable!()
                    }
                });
                debug_assert!(rec.is_some() || val.is_some());
                AlgebraicResult::Element(Self::new(rec, val))
            }
        }
    }
}

impl<V: Clone + Send + Sync + Lattice, A: Allocator, Cf: CoFree<V=V, A=A>, OtherCf: CoFree<V=V, A=A>> HeteroLattice<ByteNode<OtherCf, A>> for ByteNode<Cf, A> {
    fn pjoin(&self, other: &ByteNode<OtherCf, A>) -> AlgebraicResult<Self> {
        let jm: ByteMask = self.mask | other.mask; //joined mask
        let mm: ByteMask = self.mask & other.mask; //meet mask

        let mut is_identity = self.mask == jm;
        let mut is_counter_identity = other.mask == jm;

        let jmc = [jm.0[0].count_ones(), jm.0[1].count_ones(), jm.0[2].count_ones(), jm.0[3].count_ones()];

        let len = (jmc[0] + jmc[1] + jmc[2] + jmc[3]) as usize;
        let mut v = ValuesVec::with_capacity_in(len, self.alloc.clone());
        let new_v = v.v.spare_capacity_mut();

        let mut l = 0;
        let mut r = 0;
        let mut c = 0;

        for i in 0..4 {
            let mut lm = jm.0[i];
            while lm != 0 {
                // this body runs at most 256 times, in the case there is 100% overlap between full nodes
                let index = lm.trailing_zeros();
                // println!("{}", index);
                if ((1u64 << index) & mm.0[i]) != 0 {
                    //This runs for cofrees that exist in both nodes
                    let lv = unsafe { self.values.get_unchecked(l) };
                    let rv = unsafe { other.values.get_unchecked(r) };
                    match lv.pjoin(rv) {
                        AlgebraicResult::None => unreachable!(), //Some joined to Some should never be None
                        AlgebraicResult::Identity(mask) => {
                            debug_assert!((mask & SELF_IDENT > 0) || (mask & COUNTER_IDENT > 0));
                            if mask & SELF_IDENT == 0 {
                                is_identity = false;
                            }
                            if mask & COUNTER_IDENT == 0 {
                                is_counter_identity = false;
                            }
                            if mask & SELF_IDENT > 0 {
                                unsafe { new_v.get_unchecked_mut(c).write(lv.clone()) };
                            } else {
                                let new_cf = Cf::from_cf(rv.clone());
                                unsafe { new_v.get_unchecked_mut(c).write(new_cf) };
                            }
                        },
                        AlgebraicResult::Element(jv) => {
                            is_identity = false;
                            is_counter_identity = false;
                            debug_assert!(jv.has_rec() || jv.has_val());
                            unsafe { new_v.get_unchecked_mut(c).write(jv) };
                        }
                    }
                    // println!("pushing lv rv j {:?} {:?} {:?}", lv, rv, jv);
                    l += 1;
                    r += 1;
                } else if ((1u64 << index) & self.mask.0[i]) != 0 {
                    // This runs for CoFrees that exist in only the left node
                    is_counter_identity = false;
                    let lv = unsafe { self.values.get_unchecked(l) };
                    // println!("pushing lv {:?}", lv);
                    unsafe { new_v.get_unchecked_mut(c).write(lv.clone()) };
                    l += 1;
                } else {
                    // This runs for CoFrees that exist in only the right node
                    is_identity = false;
                    let rv = unsafe { other.values.get_unchecked(r) };
                    // println!("pushing rv {:?}", rv);
                    unsafe { new_v.get_unchecked_mut(c).write(<_>::from_cf(rv.clone())) };
                    r += 1;
                }
                lm ^= 1u64 << index;
                c += 1;
            }
        }

        unsafe{ v.v.set_len(c); }
        if c == 0 {
            AlgebraicResult::None
        } else {
            if is_identity || is_counter_identity {
                let mut mask = 0;
                if is_identity { mask |= SELF_IDENT; }
                if is_counter_identity { mask |= COUNTER_IDENT; }
                AlgebraicResult::Identity(mask)
            } else {
                AlgebraicResult::Element(Self::new_with_fields_in(jm, v, self.alloc.clone()))
            }
        }
    }

    fn join_into(&mut self, mut other: ByteNode<OtherCf, A>) -> AlgebraicStatus {
        let jm: ByteMask = self.mask | other.mask;
        let mm: ByteMask = self.mask & other.mask;

        let mut is_identity = self.mask == jm;

        let jmc = [jm.0[0].count_ones(), jm.0[1].count_ones(), jm.0[2].count_ones(), jm.0[3].count_ones()];

        let l = (jmc[0] + jmc[1] + jmc[2] + jmc[3]) as usize;
        let mut v = ValuesVec::with_capacity_in(l, self.alloc.clone());
        let new_v = v.v.spare_capacity_mut();

        let mut l = 0;
        let mut r = 0;
        let mut c = 0;

        for i in 0..4 {
            let mut lm = jm.0[i];
            while lm != 0 {
                // this body runs at most 256 times, in the case there is 100% overlap between full nodes
                let index = lm.trailing_zeros();
                // println!("{}", index);
                if ((1u64 << index) & mm.0[i]) != 0 {
                    let mut lv = unsafe { std::ptr::read(self.values.get_unchecked_mut(l)) };
                    let rv = unsafe { std::ptr::read(other.values.get_unchecked_mut(r)) };
                    match lv.join_into(rv) {
                        AlgebraicStatus::Identity => { },
                        AlgebraicStatus::Element => { is_identity = false; },
                        AlgebraicStatus::None => unreachable!(), //Some.join(Some) shouldn't create None
                    }
                    unsafe { new_v.get_unchecked_mut(c).write(lv) };
                    l += 1;
                    r += 1;
                } else if ((1u64 << index) & self.mask.0[i]) != 0 {
                    let lv = unsafe { std::ptr::read(self.values.get_unchecked_mut(l)) };
                    unsafe { new_v.get_unchecked_mut(c).write(lv) };
                    l += 1;
                } else {
                    is_identity = false;
                    let rv = unsafe { std::ptr::read(other.values.get_unchecked_mut(r)) };
                    unsafe { new_v.get_unchecked_mut(c).write(<_>::from_cf(rv)) };
                    r += 1;
                }
                lm ^= 1u64 << index;
                c += 1;
            }
        }

        unsafe { self.values.set_len(0) }
        unsafe { other.values.set_len(0) }
        unsafe { v.v.set_len(c) }
        self.mask = jm;
        self.values = v.v;

        if c == 0 {
            AlgebraicStatus::None
        } else if is_identity {
            AlgebraicStatus::Identity
        } else {
            AlgebraicStatus::Element
        }
    }

    fn pmeet(&self, other: &ByteNode<OtherCf, A>) -> AlgebraicResult<Self> {
        // TODO this technically doesn't need to calculate and iterate over jm
        // iterating over mm and calculating m such that the following suffices
        // c_{self,other} += popcnt(m & {self,other})
        let jm: ByteMask = self.mask | other.mask;
        let mut mm: ByteMask = self.mask & other.mask;

        let mut is_identity = self.mask == mm;
        let mut is_counter_identity = other.mask == mm;

        let mmc = [mm.0[0].count_ones(), mm.0[1].count_ones(), mm.0[2].count_ones(), mm.0[3].count_ones()];

        let len = (mmc[0] + mmc[1] + mmc[2] + mmc[3]) as usize;
        let mut v = ValuesVec::with_capacity_in(len, self.alloc.clone());
        let new_v = v.v.spare_capacity_mut();

        let mut l = 0;
        let mut r = 0;
        let mut c = 0;

        for i in 0..4 {
            let mut lm = jm.0[i];
            while lm != 0 {
                let index = lm.trailing_zeros();

                if ((1u64 << index) & mm.0[i]) != 0 {
                    //This runs for cofrees that exist in both nodes

                    let lv = unsafe { self.values.get_unchecked(l) };
                    let rv = unsafe { other.values.get_unchecked(r) };
                    match lv.pmeet(rv) {
                        AlgebraicResult::None => {
                            is_counter_identity = false;
                            is_identity = false;
                            mm.0[i] ^= 1u64 << index;
                        },
                        AlgebraicResult::Identity(mask) => {
                            debug_assert!((mask & SELF_IDENT > 0) || (mask & COUNTER_IDENT > 0));
                            if mask & SELF_IDENT == 0 {
                                is_identity = false;
                            }
                            if mask & COUNTER_IDENT == 0 {
                                is_counter_identity = false;
                            }
                            if mask & SELF_IDENT > 0 {
                                unsafe { new_v.get_unchecked_mut(c).write(lv.clone()) };
                            } else {
                                let new_cf = Cf::from_cf(rv.clone());
                                unsafe { new_v.get_unchecked_mut(c).write(new_cf) };
                            }
                            c += 1;
                        },
                        AlgebraicResult::Element(jv) => {
                            is_identity = false;
                            is_counter_identity = false;
                            unsafe { new_v.get_unchecked_mut(c).write(jv) };
                            c += 1;
                        },
                    }
                    l += 1;
                    r += 1;
                } else if ((1u64 << index) & self.mask.0[i]) != 0 {
                    l += 1;
                } else {
                    r += 1;
                }
                lm ^= 1u64 << index;
            }
        }

        unsafe{ v.v.set_len(c); }
        if c == 0 {
            AlgebraicResult::None
        } else {
            if is_identity || is_counter_identity {
                let mut mask = 0;
                if is_identity { mask |= SELF_IDENT; }
                if is_counter_identity { mask |= COUNTER_IDENT; }
                AlgebraicResult::Identity(mask)
            } else {
                AlgebraicResult::Element(Self::new_with_fields_in(mm, v, self.alloc.clone()))
            }
        }
    }

    //GOAT, kept for now, for reference, but this code is unreachable using the current Lattice interface
    // fn join_all(xs: &[&Self]) -> Self {
    //     let alloc = xs[0].alloc.clone();
    //     let mut jm: ByteMask = ByteMask::EMPTY;
    //     for x in xs.iter() {
    //         jm |= x.mask;
    //     }

    //     let jmc = [jm.0[0].count_ones(), jm.0[1].count_ones(), jm.0[2].count_ones(), jm.0[3].count_ones()];

    //     let len = (jmc[0] + jmc[1] + jmc[2] + jmc[3]) as usize;
    //     let mut v = ValuesVec::with_capacity_in(len, alloc.clone());
    //     let new_v = v.v.spare_capacity_mut();

    //     let mut c = 0;

    //     for i in 0..4 {
    //         let mut lm = jm.0[i];
    //         while lm != 0 {
    //             // this body runs at most 256 times, in the case there is 100% overlap between full nodes
    //             let index = lm.trailing_zeros();

    //             //GOAT, allocating a temp buffer likely undoes the gains from join_all
    //             let to_join: Vec<&Cf> = xs.iter().enumerate().filter_map(|(i, x)| x.get(i as u8)).collect();
    //             let joined = HeteroLattice::<Cf>::join_all(&to_join[..]);
    //             unsafe { new_v.get_unchecked_mut(c).write(joined) };

    //             lm ^= 1u64 << index;
    //             c += 1;
    //         }
    //     }

    //     unsafe{ v.v.set_len(c); }
    //     return Self::new_with_fields_in(jm, v, alloc);
    // }
    fn convert(other: ByteNode<OtherCf, A>) -> Self {
        let mut values = ValuesVec::with_capacity_in(other.values.len(), other.alloc.clone());
        for other_cf in other.values {
            values.v.push(Cf::convert(other_cf));
        }
        Self::new_with_fields_in(other.mask, values, other.alloc)
    }
}

//NOTE: This *looks* like an impl of DistributiveLattice, but it isn't, so we can have `self` and
// `other` be differently parameterized types
impl<V: DistributiveLattice + Clone + Send + Sync, A: Allocator, Cf: CoFree<V=V, A=A>> ByteNode<Cf, A> {
    fn psubtract<OtherCf: CoFree<V=V, A=A>>(&self, other: &ByteNode<OtherCf, A>) -> AlgebraicResult<Self> where Self: Sized {
        let mut is_identity = true;
        let mut btn = self.clone();

        for i in 0..4 {
            let mut lm = self.mask.0[i];
            while lm != 0 {
                let index = lm.trailing_zeros();

                if ((1u64 << index) & other.mask.0[i]) != 0 {
                    let lv = unsafe { self.get_unchecked(64*(i as u8) + (index as u8)) };
                    let rv = unsafe { other.get_unchecked(64*(i as u8) + (index as u8)) };
                    match HeteroDistributiveLattice::psubtract(lv, rv) {
                        AlgebraicResult::None => {
                            is_identity = false;
                            btn.remove(64*(i as u8) + (index as u8));
                        },
                        AlgebraicResult::Identity(mask) => {
                            debug_assert_eq!(mask, SELF_IDENT); //subtract is non-commutative
                        },
                        AlgebraicResult::Element(jv) => {
                            is_identity = false;
                            let dst = unsafe { btn.get_unchecked_mut(64*(i as u8) + (index as u8)) };
                            *dst = jv;
                        },
                    }
                }

                lm ^= 1u64 << index;
            }
        }

        if btn.is_empty() {
            AlgebraicResult::None
        } else {
            if is_identity {
                AlgebraicResult::Identity(SELF_IDENT)
            } else {
                AlgebraicResult::Element(btn)
            }
        }
    }
}

//NOTE: This *looks* like an impl of Quantale, but it isn't, so we can have `self` and
// `other` be differently parameterized types
impl<V: Clone + Send + Sync, A: Allocator, Cf: CoFree<V=V, A=A>> ByteNode<Cf, A> {
    fn prestrict<OtherCf: CoFree<V=V, A=A>>(&self, other: &ByteNode<OtherCf, A>) -> AlgebraicResult<Self> where Self: Sized {
        let mut is_identity = true;

        // TODO this technically doesn't need to calculate and iterate over jm
        // iterating over mm and calculating m such that the following suffices
        // c_{self,other} += popcnt(m & {self,other})
        let jm: ByteMask = self.mask | other.mask;
        let mut mm: ByteMask = self.mask & other.mask;

        let mmc = [mm.0[0].count_ones(), mm.0[1].count_ones(), mm.0[2].count_ones(), mm.0[3].count_ones()];

        let len = (mmc[0] + mmc[1] + mmc[2] + mmc[3]) as usize;
        let mut v = ValuesVec::with_capacity_in(len, self.alloc.clone());
        let new_v = v.v.spare_capacity_mut();

        let mut l = 0;
        let mut r = 0;
        let mut c = 0;

        for i in 0..4 {
            let mut lm = jm.0[i];
            while lm != 0 {
                let index = lm.trailing_zeros();

                if ((1u64 << index) & mm.0[i]) != 0 {
                    let lv = unsafe { self.values.get_unchecked(l) };
                    let rv = unsafe { other.values.get_unchecked(r) };
                    // println!("dense prestrict {}", index as usize + i*64);

                    match lv.prestrict(rv) {
                        AlgebraicResult::None => {
                            is_identity = false;
                            mm.0[i] ^= 1u64 << index;
                        }
                        AlgebraicResult::Identity(mask) => {
                            debug_assert_eq!(mask, SELF_IDENT); //restrict is non-commutative
                            unsafe { new_v.get_unchecked_mut(c).write(lv.clone()) };
                            c += 1;
                        },
                        AlgebraicResult::Element(jv) => {
                            is_identity = false;
                            unsafe { new_v.get_unchecked_mut(c).write(jv) };
                            c += 1;
                        },
                    }
                    l += 1;
                    r += 1;
                } else {
                    is_identity = false;
                    if ((1u64 << index) & self.mask.0[i]) != 0 {
                        l += 1;
                    } else {
                        r += 1;
                    }
                }
                lm ^= 1u64 << index;
            }
        }

        unsafe{ v.v.set_len(c); }
        if c == 0 {
            AlgebraicResult::None
        } else {
            if is_identity {
                AlgebraicResult::Identity(SELF_IDENT)
            } else {
                AlgebraicResult::Element(Self::new_with_fields_in(mm, v, self.alloc.clone()))
            }
        }
    }
}

#[test]
fn bit_siblings() {
    let x = 0b0000000000000000000000000000000000000100001001100000000000000010u64;
    let i = 0b0000000000000000000000000000000000000000000001000000000000000000u64;
    let p = 0b0000000000000000000000000000000000000000001000000000000000000000u64;
    let n = 0b0000000000000000000000000000000000000000000000100000000000000000u64;
    let f = 0b0000000000000000000000000000000000000100000000000000000000000000u64;
    let l = 0b0000000000000000000000000000000000000000000000000000000000000010u64;
    let bit_i = 18;
    let bit_i_onehot = 1u64 << bit_i;
    assert_eq!(i, bit_i_onehot);
    assert_ne!(bit_i_onehot & x, 0);
    assert_eq!(p, 1u64 << bit_sibling(bit_i, x, false));
    assert_eq!(n, 1u64 << bit_sibling(bit_i, x, true));
    assert_eq!(f, 1u64 << bit_sibling(f.trailing_zeros() as u8, x, false));
    assert_eq!(l, 1u64 << bit_sibling(l.trailing_zeros() as u8, x, true));
    assert_eq!(0, bit_sibling(0, 1, false));
    assert_eq!(0, bit_sibling(0, 1, true));
    assert_eq!(63, bit_sibling(63, 1u64 << 63, false));
    assert_eq!(63, bit_sibling(63, 1u64 << 63, true));
}
