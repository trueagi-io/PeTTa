
//! # Zipper Usage
//!
//! A zipper represents a cursor in a trie, and has a location called the focus.  A zipper can be moved
//! within the trie in order to access the trie for reading and/or writing.  A zipper's focus may not be
//! moved above the zipper's root.
//!

use maybe_dangling::MaybeDangling;
use fast_slice_utils::find_prefix_overlap;

use super::alloc::{Allocator, GlobalAlloc};
use super::utils::ByteMask;
use super::trie_core::node::{TrieNodeODRc, AbstractNodeRef, TaggedNodeRef};
pub use super::trie_core::r#ref::{TrieRef, TrieRefBorrowed, TrieRefOwned};
use super::PathMap;

pub use super::write_zipper::*;
pub use super::zipper_head::*;
pub use super::product_zipper::{ProductZipper, ProductZipperG, ZipperProduct, OneFactor};
pub use super::overlay_zipper::{OverlayZipper};
pub use super::prefix_zipper::{PrefixZipper};
pub use super::empty_zipper::{EmptyZipper};
pub use super::poly_zipper::{PolyZipper, PolyZipperExplicit};
pub use super::dependent_zipper::DependentProductZipperG;
use super::zipper_tracking::*;


/// The most fundamantal interface for a zipper, compatible with all zipper types
pub trait Zipper {
    /// Returns `true` if the zipper's focus is on a path within the trie, otherwise `false`
    fn path_exists(&self) -> bool;

    /// Returns `true` if there is a value at the zipper's focus, otherwise `false`
    fn is_val(&self) -> bool;

    /// Deprecated alias for [Zipper::is_val]
    #[deprecated] //GOAT-old-names
    fn is_value(&self) -> bool {
        self.is_val()
    }

    /// Returns the number of child branches from the focus node
    ///
    /// Returns 0 if the focus is on a leaf
    fn child_count(&self) -> usize;

    /// Returns 256-bit mask indicating which children exist from the branch at the zipper's focus
    ///
    /// Returns an empty mask if the focus is on a leaf or non-existent path
    fn child_mask(&self) -> ByteMask;
}

/// Methods for zippers with a known value type
pub trait ZipperValues<V> {
    /// Returns a refernce to the value at the zipper's focus, or `None` if there is no value
    ///
    /// If you have a zipper type that implements [ZipperReadOnlyValues] then [ZipperReadOnlyValues::get_val]
    /// will provide a longer-lived reference to the value.
    fn val(&self) -> Option<&V>;

    /// Deprecated alias for [ZipperValues::val]
    #[deprecated] //GOAT-old-names
    fn value(&self) -> Option<&V> {
        self.val()
    }
}

/// Method to fork a read zipper from the parent zipper
pub trait ZipperForking<V> {
    /// The read-zipper type returned from [fork_read_zipper](ZipperForking::fork_read_zipper)
    type ReadZipperT<'a>: ZipperAbsolutePath + ZipperIteration + ZipperValues<V> where Self: 'a;

    /// Returns a new read-only Zipper, with the new zipper's root being at the zipper's current focus
    ///
    /// Discussion: The main uses of this method is to construct a child zipper with a different root
    /// from the parent, to construct a read zipper when you have another zipper type, or to cheaply
    /// create a zipper you can pass to a function that takes ownership.  Often however, you want to
    /// clone the parent zipper instead.
    fn fork_read_zipper<'a>(&'a self) -> Self::ReadZipperT<'a>;
}

/// Methods for zippers that can access concrete subtries
pub trait ZipperSubtries<V: Clone + Send + Sync, A: Allocator = GlobalAlloc>: ZipperValues<V> {
    /// Returns `true` if the zipper can access a subtrie in constant time
    ///
    /// Sometimes this trait will be implemented on abstract zipper types, in which case this method will return
    /// `false`.  When it returns `true` other methods in this trait should return `Some`.
    fn native_subtries(&self) -> bool;

    /// Returns a new [PathMap] containing everything below the zipper's focus or `None` if the zipper doesn't
    /// suppot creating a new `PathMap`.
    ///
    /// GOAT: This method's behavior is affected by the `graft_root_vals` feature
    /// This method does not clone the value at the focus as the map's root.
    /// GOAT QUESTION: Should this method include the focus value as the map's root value?  That
    /// makes conceptual sense given the fact that maps have root values, however the main argument
    /// for "no" is keeping compatibility with [ZipperWriting::graft_map] and keeping an analogous API
    /// to [ZipperWriting::take_map].  Changing `ZipperWriting::graft_map` probably entails a corresponding
    /// change to [ZipperWriting::graft] also, to keep API consistency.
    /// Adam: It may not make conceptual sense, as was clear in the original cata definition; the subtrie
    /// can be interpreted as living below a value. This also argues for not having a value at the empty path,
    /// a change I'd also welcome. As for performance, graft is probably the most called zipper method.
    ///
    /// Luke: Personally I think it might make sense for all of the entry points to change behavior.
    /// Perhaps the biggest argument against the change is that it effectively doubles the cost of
    /// graft.  This is related to a similar question on [ZipperWriting::join_map_into]
    fn try_make_map(&self) -> Option<PathMap<V, A>>;

    /// Attempts to return a [TrieRef] from the current focus
    fn trie_ref(&self) -> Option<TrieRef<'_, V, A>>;

    /// Returns a clone of the allocator used by the zipper
    fn alloc(&self) -> A;
}

/// An interface to enable moving a zipper around the trie and inspecting paths
///
/// ## Terminology:
///
/// A zipper's **root** is a point in the trie above which the zipper cannot ascend.  A zipper permits
/// access to a subtrie descending from its root, but that subtrie may be a part of a supertrie that the
/// zipper is unable to access.
///
/// A zipper's **origin** is always equal-to-or-above the zipper's root.  The position of the origin depends
/// on how the zipper was created, and the origin will never be below the root.  The origin of a given zipper
/// will never change.
pub trait ZipperMoving: Zipper {
    /// Returns `true` if the zipper's focus is at its root, and it cannot ascend further, otherwise returns `false`
    fn at_root(&self) -> bool {
        self.path().len() == 0
    }

    /// Resets the zipper's focus back to its root
    fn reset(&mut self) {
        while !self.at_root() {
            self.ascend_byte();
        }
    }

    /// Returns the path from the zipper's root to the current focus
    fn path(&self) -> &[u8];

    /// Returns the total number of values contained at and below the zipper's focus, including the focus itself
    ///
    /// WARNING: This is not a cheap method. It may have an order-N cost
    //GOAT! This doesn't belong here.  Should be a function that uses a non-side-effect catamorphism
    fn val_count(&self) -> usize;

    /// Moves the zipper's focus to a specific location specified by `path`, relative to the zipper's root
    ///
    /// Returns the number of bytes shared between the old and new location
    fn move_to_path<K: AsRef<[u8]>>(&mut self, path: K) -> usize {
        let path = path.as_ref();
        let p = self.path();
        let overlap = find_prefix_overlap(path, p);
        let to_ascend = p.len() - overlap;
        if overlap == 0 {  // This heuristic can be fine-tuned for performance; the behavior of the two branches is equivalent
            self.reset();
            self.descend_to(path);
            overlap
        } else {
            self.ascend(to_ascend);
            self.descend_to(&path[overlap..]);
            overlap
        }
    }

    /// Moves the zipper deeper into the trie, to the `key` specified relative to the current zipper focus
    fn descend_to<K: AsRef<[u8]>>(&mut self, k: K);

    /// Fused [`descend_to`](ZipperMoving::descend_to) and [`path_exists`](Zipper::path_exists).  Moves the
    /// focus and returns whether or not the path exists at the new focus location
    fn descend_to_check<K: AsRef<[u8]>>(&mut self, k: K) -> bool {
        self.descend_to(k);
        self.path_exists()
    }

    /// Moves the zipper deeper into the trie, following the path specified by `k`, relative to the current
    /// zipper focus.  Descent stops at the point where the path does not exist
    ///
    /// Returns the number of bytes descended along the path.  The zipper's focus will always be on an
    /// existing path after this method returns, unless the method was called with the focus on a
    /// non-existent path.
    fn descend_to_existing<K: AsRef<[u8]>>(&mut self, k: K) -> usize {
        let k = k.as_ref();
        let mut i = 0;
        while i < k.len() {
            self.descend_to_byte(k[i]);
            if !self.path_exists() {
                self.ascend_byte();
                return i
            }
            i += 1
        }
        i
    }

    /// Moves the zipper deeper into the trie, following the path specified by `k`, relative to the current
    /// zipper focus.  Descent stops if a value is encountered or if the path ceases to exist.
    ///
    /// Returns the number of bytes descended along the path.
    ///
    /// If the focus is already on a value, this method will descend to the *next* value along
    /// the path.
    //GOAT. this default implementation could certainly be optimized
    fn descend_to_val<K: AsRef<[u8]>>(&mut self, k: K) -> usize {
        let k = k.as_ref();
        let mut i = 0;
        while i < k.len() {
            self.descend_to_byte(k[i]);
            if !self.path_exists() {
                self.ascend_byte();
                return i
            }
            i += 1;
            if self.is_val() {
                return i
            }
        }
        i
    }

    /// Deprecated alias for [ZipperMoving::descend_to_val]
    #[deprecated] //GOAT-old-names
    fn descend_to_value<K: AsRef<[u8]>>(&mut self, k: K) -> usize {
        self.descend_to_val(k)
    }

    /// Moves the zipper one byte deeper into the trie.  Identical in effect to [descend_to](Self::descend_to)
    /// with a 1-byte key argument
    fn descend_to_byte(&mut self, k: u8) {
        self.descend_to(&[k])
    }

    /// Moves the zipper one byte deeper into the trie, if the specified path byte exists in the [`child_mask`](Zipper::child_mask).
    ///
    /// Returns `true` if the zipper's focus moved and `false` if it is still at the original location.
    fn descend_to_existing_byte(&mut self, k: u8) -> bool {
        self.descend_to_byte(k);
        if self.path_exists() {
            true
        } else {
            self.ascend_byte();
            false
        }
    }

    /// Descends the zipper's focus one byte into a child branch uniquely identified by `child_idx`
    ///
    /// `child_idx` must within the range `0..child_count()` or this method will do nothing and return `false`
    ///
    /// WARNING: The branch represented by a given index is not guaranteed to be stable across modifications
    /// to the trie.  This method should only be used as part of a directed traversal operation, but
    /// index-based paths may not be stored as locations within the trie.
    fn descend_indexed_byte(&mut self, idx: usize) -> bool {
        let mask = self.child_mask();
        let child_byte = match mask.indexed_bit::<true>(idx) {
            Some(byte) => byte,
            None => {
                return false
            }
        };
        self.descend_to_byte(child_byte);
        debug_assert!(self.path_exists());
        true
    }

    /// A deprecated alias for [ZipperMoving::descend_indexed_byte]
    #[deprecated] //GOAT-old-names
    fn descend_indexed_branch(&mut self, idx: usize) -> bool {
        self.descend_indexed_byte(idx)
    }

    /// Descends the zipper's focus one step into the first child branch in a depth-first traversal
    ///
    /// NOTE: This method should have identical behavior to passing `0` to [descend_indexed_byte](ZipperMoving::descend_indexed_byte),
    /// although with less overhead
    fn descend_first_byte(&mut self) -> bool {
        self.descend_indexed_byte(0)
    }

    /// Descends the zipper's focus one step into the last child branch
    ///
    /// NOTE: This method should have identical behavior to passing `child_count() - 1` to [descend_indexed_byte](ZipperMoving::descend_indexed_byte),
    /// although with less overhead
    fn descend_last_byte(&mut self) -> bool {
        let cc = self.child_count();
        if cc == 0 { false }
        else { self.descend_indexed_byte( cc- 1) }
    }

    /// Descends the zipper's focus until a branch or a value is encountered.  Returns `true` if the focus
    /// moved otherwise returns `false`
    ///
    /// If there is a value at the focus, the zipper will descend to the next value or branch, however the
    /// zipper will not descend further if this method is called with the focus already on a branch.
    ///
    /// Does nothing and returns `false` if the zipper's focus is on a non-existent path.
    fn descend_until(&mut self) -> bool {
        let mut descended = false;
        while self.child_count() == 1 {
            descended = true;
            self.descend_first_byte();
            if self.is_val() {
                break;
            }
        }
        descended
    }

    /// Descends the zipper's focus until a branch or a value is encountered, or until `max_bytes`
    /// bytes have been descended. Returns `true` if the focus moved otherwise returns `false`
    ///
    /// If there is a value at the focus, the zipper will descend to the next value or branch, however the
    /// zipper will not descend further if this method is called with the focus already on a branch.
    ///
    /// Does nothing and returns `false` if the zipper's focus is on a non-existent path or if `max_bytes`
    /// is zero.
    fn descend_until_max_bytes(&mut self, max_bytes: usize) -> bool {
        if max_bytes == 0 {
            return false;
        }
        let target_len = self.path().len() + max_bytes;
        let descended = self.descend_until();
        let cur_len = self.path().len();
        if cur_len > target_len {
            let _ = self.ascend(cur_len - target_len);
        }
        descended
    }

    /// Ascends the zipper `steps` steps.  Returns `true` if the zipper sucessfully moved `steps`
    ///
    /// If the root is fewer than `n` steps from the zipper's position, then this method will stop at
    /// the root and return `false`
    fn ascend(&mut self, steps: usize) -> bool;

    /// Ascends the zipper up a single byte.  Equivalent to passing `1` to [ascend](Self::ascend)
    fn ascend_byte(&mut self) -> bool {
        self.ascend(1)
    }

    /// Ascends the zipper to the nearest upstream branch point or value.  Returns `true` if the zipper
    /// focus moved upwards, otherwise returns `false` if the zipper was already at the root
    ///
    /// NOTE: A default implementation could be provided, but all current zippers have more optimal native implementations.
    fn ascend_until(&mut self) -> bool;

    /// Ascends the zipper to the nearest upstream branch point, skipping over values along the way.  Returns
    /// `true` if the zipper focus moved upwards, otherwise returns `false` if the zipper was already at the
    /// root
    ///
    /// NOTE: A default implementation could be provided, but all current zippers have more optimal native implementations.
    fn ascend_until_branch(&mut self) -> bool;

    /// Moves the zipper's focus to the next sibling byte with the same parent
    ///
    /// Returns `true` if the focus was moved.  If the focus is already on the last byte among its siblings,
    /// this method returns false, leving the focus unmodified.
    ///
    /// This method is equivalent to calling [ZipperMoving::ascend] with `1`, followed by [ZipperMoving::descend_indexed_byte]
    /// where the index passed is 1 more than the index of the current focus position.
    fn to_next_sibling_byte(&mut self) -> bool {
        let cur_byte = match self.path().last() {
            Some(byte) => *byte,
            None => return false
        };
        if !self.ascend_byte() {
            return false
        }
        let mask = self.child_mask();
        match mask.next_bit(cur_byte) {
            Some(byte) => {
                self.descend_to_byte(byte);
                debug_assert!(self.path_exists());
                true
            },
            None => {
                self.descend_to_byte(cur_byte);
                false
            }
        }
    }

    /// Moves the zipper's focus to the previous sibling byte with the same parent
    ///
    /// Returns `true` if the focus was moved.  If the focus is already on the first byte among its siblings,
    /// this method returns false, leving the focus unmodified.
    ///
    /// This method is equivalent to calling [Self::ascend] with `1`, followed by [Self::descend_indexed_byte]
    /// where the index passed is 1 less than the index of the current focus position.
    fn to_prev_sibling_byte(&mut self) -> bool {
        let cur_byte = match self.path().last() {
            Some(byte) => *byte,
            None => return false
        };
        if !self.ascend_byte() {
            return false
        }
        let mask = self.child_mask();
        match mask.prev_bit(cur_byte) {
            Some(byte) => {
                self.descend_to_byte(byte);
                debug_assert!(self.path_exists());
                true
            },
            None => {
                self.descend_to_byte(cur_byte);
                debug_assert!(self.path_exists());
                false
            }
        }
    }

    /// Advances the zipper to visit every existing path within the trie in a depth-first order
    ///
    /// Returns `true` if the position of the zipper has moved, or `false` if the zipper has returned
    /// to the root
    fn to_next_step(&mut self) -> bool {

        //If we're at a leaf ascend until we're not and jump to the next sibling
        if self.child_count() == 0 {
            //We can stop ascending when we succeed in moving to a sibling
            while !self.to_next_sibling_byte() {
                if !self.ascend_byte() {
                    return false;
                }
            }
        } else {
            return self.descend_first_byte()
        }
        true
    }
}

/// An interface to access values through a [Zipper] that cannot modify the trie.  Allows
/// references with lifetimes that may outlive the zipper
///
/// This trait will never be implemented on the same type as [ZipperWriting]
pub trait ZipperReadOnlyValues<'a, V>: ZipperValues<V> {
    /// Returns a refernce to the value at the zipper's focus, or `None` if there is no value
    ///
    /// NOTE: Unlike [ZipperValues::val], this method returns a reference with the lifetime of `'a`
    /// instead of the temporary lifetime of the method.
    fn get_val(&self) -> Option<&'a V>;

    /// Deprecated alias for [ZipperReadOnlyValues::get_val]
    #[deprecated] //GOAT-old-names
    fn get_value(&self) -> Option<&'a V> {
        self.get_val()
    }
}

/// A [`witness`](ZipperReadOnlyConditionalValues::witness) type used by [`ReadZipperTracked`] and [`ReadZipperOwned`]
pub struct ReadZipperWitness<V: Clone + Send + Sync, A: Allocator>(pub(crate) Option<TrieNodeODRc<V, A>>);

super::impl_name_only_debug!(
    impl<V: Clone + Send + Sync + Unpin, A: Allocator> core::fmt::Debug for ReadZipperWitness<V, A>
);

/// Conceptually similar to [ZipperReadOnlyValues] but requires a [`witness`](ZipperReadOnlyConditionalValues::witness)
/// to ensure the data remains intact.
///
/// NOTE: In a future version of rust, when there is some support for disjoint borrows, we could unify
/// this trait with `ZipperReadOnlyValues` by splitting the "read guard" function of the zipper from
/// the "cursor" function of the zipper.
pub trait ZipperReadOnlyConditionalValues<'a, V>: ZipperValues<V> {
    /// The type that acts as a witness for the validity of the zipper
    type WitnessT;

    /// Creates a witness that can allow acquisition of longer-lived borrows of values, while the
    /// zipper itself is mutated
    fn witness<'w>(&self) -> Self::WitnessT;

    /// Returns a refernce to the value at the zipper's focus, or `None` if there is no value
    ///
    /// NOTE: Unlike [ZipperValues::val], this method returns a reference with the lifetime of `'a`
    /// instead of the temporary lifetime of the method.
    fn get_val_with_witness<'w>(&self, witness: &'w Self::WitnessT) -> Option<&'w V> where 'a: 'w;
}

/// An interface to implement iterating over all values in a subtrie via a zipper
pub trait ZipperReadOnlyIteration<'a, V>: ZipperReadOnlyValues<'a, V> + ZipperIteration {
    /// Advances to the next value with behavior identical to [ZipperIteration::to_next_val], but returns
    /// a reference to the value or `None` if the zipper has encountered the root
    fn to_next_get_val(&mut self) -> Option<&'a V> {
        if self.to_next_val() {
            let val = self.get_val();
            debug_assert!(val.is_some());
            val
        } else {
            None
        }
    }

    /// Deprecated alias for [ZipperReadOnlyIteration::to_next_get_val]
    #[deprecated] //GOAT-old-names
    fn to_next_get_value(&mut self) -> Option<&'a V> {
        self.to_next_get_val()
    }
}

/// Similar to [ZipperReadOnlyIteration].  See [ZipperReadOnlyConditionalValues] for an explanation about
/// why this trait exists
pub trait ZipperReadOnlyConditionalIteration<'a, V>: ZipperReadOnlyConditionalValues<'a, V> + ZipperIteration {
    /// See [ZipperReadOnlyIteration::to_next_get_val] and [`witness`](ZipperReadOnlyConditionalValues::witness)
    fn to_next_get_val_with_witness<'w>(&mut self, witness: &'w Self::WitnessT) -> Option<&'w V> where 'a: 'w {
        if self.to_next_val() {
            let val = self.get_val_with_witness(witness);
            debug_assert!(val.is_some());
            val
        } else {
            None
        }
    }
}

/// Similar to [ZipperSubtries], but with the stronger guarantee that subtrie access will be constant-time and won't fail
pub trait ZipperInfallibleSubtries<V: Clone + Send + Sync, A: Allocator = GlobalAlloc>: ZipperValues<V> + zipper_priv::ZipperPriv<V=V, A=A> {
    /// Returns a new [PathMap] containing everything below the zipper's focus
    fn make_map(&self) -> PathMap<V, A>;

    /// Return a [TrieRef] from the current focus
    fn get_trie_ref(&self) -> TrieRef<'_, V, A>;
}

/// An interface to access subtries through a [Zipper] that cannot modify the trie.  Allows
/// references with lifetimes that may outlive the zipper
///
/// This trait will never be implemented on the same type as [ZipperWriting]
pub trait ZipperReadOnlySubtries<'a, V: Clone + Send + Sync, A: Allocator = GlobalAlloc>: ZipperSubtries<V, A> + ZipperReadOnlyPriv<'a, V, A> {
    /// The type of the returned [TrieRef]
    type TrieRefT: Into<TrieRef<'a, V, A>> where V: 'a, A: 'a;

    /// Returns a [TrieRef] for the specified path, relative to the current focus
    fn trie_ref_at_path<K: AsRef<[u8]>>(&self, path: K) -> Self::TrieRefT;
}

/// An interface for advanced [Zipper] movements used for various types of iteration; such as iterating
/// every value, or iterating all paths descending from a common root at a certain depth
pub trait ZipperIteration: ZipperMoving {
    /// Systematically advances to the next value accessible from the zipper, traversing in a depth-first
    /// order
    ///
    /// Returns `true` if the zipper is positioned at the next value, or `false` if the zipper has
    /// encountered the root.
    fn to_next_val(&mut self) -> bool {
        loop {
            if self.descend_first_byte() {
                if self.is_val() {
                    return true
                }
                if self.descend_until() {
                    if self.is_val() {
                        return true
                    }
                }
            } else {
                'ascending: loop {
                    if self.to_next_sibling_byte() {
                        if self.is_val() {
                            return true
                        }
                        break 'ascending
                    } else {
                        self.ascend_byte();
                        if self.at_root() {
                            return false
                        }
                    }
                }
            }
        }
    }

    /// Descends the zipper to the end of the last path (last by sort order) reachable by descent
    /// from the current focus.
    ///
    /// This is equivalent to calling [ZipperMoving::descend_last_byte] in a loop.
    ///
    /// Returns `true` if the zipper has sucessfully descended, or `false` if the zipper was already
    /// at the end of a path.  If this method returns `false` then the zipper will be in its original
    /// position.
    fn descend_last_path(&mut self) -> bool {
        let mut any = false;
        while self.descend_last_byte() {
            any = true;
            self.descend_until();
        }
        any
    }

    /// Descends the zipper's focus `k` bytes, following the first child at each branch, and continuing
    /// with depth-first exploration until a path that is `k` bytes from the focus has been found
    ///
    /// Returns `true` if the zipper has sucessfully descended `k` steps, or `false` otherwise.  If this
    /// method returns `false` then the zipper will be in its original position.
    ///
    /// WARNING: This is not a constant-time operation, and may be as bad as `order n` with respect to the paths
    /// below the zipper's focus.  Although a typical cost is `order log n` or better.
    ///
    /// See: [to_next_k_path](ZipperIteration::to_next_k_path)
    fn descend_first_k_path(&mut self, k: usize) -> bool {
        k_path_default_internal(self, k, self.path().len())
    }

    /// Moves the zipper's focus to the next location with the same path length as the current focus,
    /// following a depth-first exploration from a common root `k` steps above the current focus
    ///
    /// Returns `true` if the zipper has sucessfully moved to a new location at the same level, or `false`
    /// if no further locations exist.  If this method returns `false` then the zipper will be ascended `k`
    /// steps to the common root.  (The focus position when [descend_first_k_path](ZipperIteration::descend_first_k_path) was called)
    ///
    /// WARNING: This is not a constant-time operation, and may be as bad as `order n` with respect to the paths
    /// below the zipper's focus.  Although a typical cost is `order log n` or better.
    ///
    /// See: [descend_first_k_path](ZipperIteration::descend_first_k_path)
    fn to_next_k_path(&mut self, k: usize) -> bool {
        let base_idx = if self.path().len() >= k {
            self.path().len() - k
        } else {
            return false
        };
        k_path_default_internal(self, k, base_idx)
    }
}

/// The default implementation of both [ZipperIteration::to_next_k_path] and [ZipperIteration::descend_first_k_path]
#[inline]
fn k_path_default_internal<Z: ZipperMoving + ?Sized>(z: &mut Z, k: usize, base_idx: usize) -> bool {
    loop {
        if z.path().len() < base_idx + k {
            while z.descend_first_byte() {
                if z.path().len() == base_idx + k { return true }
            }
        }
        if z.to_next_sibling_byte() {
            if z.path().len() == base_idx + k { return true }
            continue
        }
        while z.path().len() > base_idx {
            z.ascend_byte();
            if z.path().len() == base_idx { return false }
            if z.to_next_sibling_byte() { break }
        }
    }
}

/// An interface for a [Zipper] to support accessing the full path buffer used to create the zipper
pub trait ZipperAbsolutePath: ZipperMoving {
    /// Returns the entire path from the zipper's origin to its current focus
    ///
    /// The zipper's origin depends on how the zipper was created.  For zippers created directly from a
    /// [PathMap], `absolute_path` will start at the root of the map, regardless of the prefix path.
    ///
    /// `zip.origin_path() == zip.root_prefix_path() + zip.path()`
    fn origin_path(&self) -> &[u8];

    /// Returns the path from the zipper's origin to the zipper's root
    ///
    /// This function output remains constant throughout the zipper's life.
    ///
    /// After [reset](ZipperMoving::reset) is called, `zip.root_prefix_path() == zip.origin_path()`.
    fn root_prefix_path(&self) -> &[u8];
}

/// Implemented on zippers that traverse in-memory trie structures, as opposed to virtual
/// spaces or abstract projections.
///
/// In some cases `ZipperConcrete` can be implemented on projection zippers, such as [ProductZipper],
/// because it is composed of concrete tries
pub trait ZipperConcrete {
    /// Get an identifier unique to the node at the zipper's focus, if the zipper's focus is at the
    /// root of a node in memory.  When zipper's focus is inside of a node, returns `None`.
    ///
    /// NOTE: The returned value is not a logical hash of the contents, but is based on
    /// the node's memory address.  Therefore it is not stable across runs and can't be
    /// used to infer logical or structural equality.  Furthermore, it is subject to
    /// change when the content of the node is modified.
    ///
    /// However when returned identifiers are equal it means the zipper has arrived at the same node
    /// in memory, even if it got there via different parent paths through the trie.
    fn shared_node_id(&self) -> Option<u64>;

    /// Returns `true` if the zipper's focus is at a location that may be accessed via two or
    /// more distinct paths
    ///
    /// DISCUSSION: the `shared` property applied only to a singular position and is not transitive
    /// to descending locations in the trie.  In other words, if this function returns `true` for
    /// a specific location, it may not return `true` for other paths descended from that location.
    ///
    /// WARNING: your code must never rely on the return value of `is_shared` for correctness; this
    /// information should be used only for optimizations.  The `shared` property may be affected by
    /// a number of internal behaviors that must not be relied upon.  For example, a previously shared
    /// subtrie may be copied for thread isolation, or the internal trie representation might otherwise
    /// change, and alter the shared property.
    ///
    /// GOAT: Make a graphic diagram to illustrate the `shared` property.  The graphic should have
    /// multiple shared subtries accessible via distinct paths, and highlight which locations will be
    /// considered `shared` from the perspective of this method.
    fn is_shared(&self) -> bool;
}

/// Provides more direct control over a [ZipperMoving] zipper's path buffer
pub trait ZipperPathBuffer: ZipperMoving {
    /// Internal method to get the path, beyond its length.  Panics if `len` > the path's capacity, or
    /// if the zipper is relative and doesn't have an `origin_path`
    ///
    /// This method is unsafe because it relies on the caller to not read uninitialized memory, even if
    /// the memory has been allocated.
    unsafe fn origin_path_assert_len(&self, len: usize) -> &[u8];

    /// Make sure the path buffer is allocated, to facilitate zipper movement
    fn prepare_buffers(&mut self);

    /// Reserve buffer space within the zipper's path buffer and node stack
    ///
    /// This method will only grow the buffers and will never shrink them.
    fn reserve_buffers(&mut self, path_len: usize, stack_depth: usize);
}

pub(crate) mod zipper_priv {
    use super::*;

    pub trait ZipperPriv {
        type V: Clone + Send + Sync;
        type A: Allocator;

        /// Returns an abstracted reference to the node at the zipper's focus
        ///
        /// The meaning of each returned value:
        /// - `AbstractNodeRef::None`
        /// The focus is on a non-existant path
        ///
        /// - `BorrowedDyn(&'a dyn TrieNode<V>)`
        /// The focus is on an existing node, but the node's `TrieNodeODRc` is not available so
        /// a "shallow copy" i.e. refcount bump, is not possible
        ///
        /// - `BorrowedRc(&'a TrieNodeODRc<V>)`
        /// The focus is on an existing node, and we can access the `TrieNodeODRc`.  This is the
        /// ideal situation. (fastest path)
        ///
        /// - `BorrowedTiny(TinyRefNode<'a, V>)`
        /// The focus is on a position inside a node, and the TinyRefNode is effectively a pointer
        /// to that position
        ///
        /// - `OwnedRc(TrieNodeODRc<V>)`
        /// We needed to make a brand new node to represent this position.  This is the worst case
        /// scenario for performance because allocation was necessary
        fn get_focus(&self) -> AbstractNodeRef<'_, Self::V, Self::A>;

        /// Attemps to return a node at the zipper's focus.  Returns `None` if the focus is not
        /// on a node.
        ///
        /// DISCUSSION - What's the difference between `try_borrow_focus` and `get_focus`???
        /// The difference is in the intended use each is optimized for.
        ///
        /// * `get_focus` will return something that behaves like a node no matter what, if the
        /// focus is on an existing path.  So it succeeds regardless of the underlying trie
        /// structure.  It is used to get the source for algebraic and graft operations.
        ///
        /// * `try_borrow_focus` will only return a node if the zipper is positioned on a node
        /// in the underlying structure.  This enables the underlying structure to be cut, in
        /// preparation for safely splitting it into multiple independent regions, as when a
        /// [ZipperHead] needs to make a WriteZipper that can be sent to another thread. 
        fn try_borrow_focus(&self) -> Option<&TrieNodeODRc<Self::V, Self::A>>;
    }

    pub trait ZipperReadOnlyPriv<'a, V: Clone + Send + Sync, A: Allocator> {
        /// Internal method returns the minimal components that compose the zipper, which are:
        ///
        /// `(focus_node, node_key, focus_val)`
        ///
        /// This method will always return either a zero-length `node_key` or `None` for
        /// `focus_val` (it may return both of those things, but always at least one)
        fn borrow_raw_parts(&self) -> (TaggedNodeRef<'_, V, A>, &[u8], Option<&V>);

        /// Returns a the [ReadZipperCore] from inside the zipper leaving a placeholder in the zipper.
        /// Returns `None` if the zipper doesn't support movement
        ///
        /// WARNING: Separating a core from its container is very dangerous.  This method should not be
        /// used lightly.  The reason this method doesn't consume the zipper because we need to keep the
        /// original around to keep the tracker, or parhaps a backing map, active as long as we're working
        /// with this core.
        ///
        /// NOTE: (This isn't in its own trait because I didn't want to further
        /// pollute the API, given we already have [ZipperReadOnly] and [ZipperMoving])
        fn take_core(&mut self) -> Option<ReadZipperCore<'a, 'static, V, A>>;
    }
}
use zipper_priv::*;

macro_rules! zipper_impl_lens {
    (ZipperIteration $s: ident => $e:expr) => {
        fn to_next_val(&mut $s) -> bool { $e.to_next_val() }
        fn descend_first_k_path(&mut $s, k: usize) -> bool { $e.descend_first_k_path(k) }
        fn to_next_k_path(&mut $s, k: usize) -> bool { $e.to_next_k_path(k) }
    };
    (ZipperAbsolutePath $s: ident => $e:expr) => {
        fn origin_path(&$s) -> &[u8] { $e.origin_path() }
        fn root_prefix_path(&$s) -> &[u8] { $e.root_prefix_path() }
    };
    (Zipper $s: ident => $e:expr) => {
        #[inline] fn path_exists(&$s) -> bool { $e.path_exists() }
        fn is_val(&$s) -> bool { $e.is_val() }
        fn child_count(&$s) -> usize { $e.child_count() }
        fn child_mask(&$s) -> ByteMask { $e.child_mask() }
    };
    (ZipperValues $s: ident => $e:expr) => {
        fn val(&$s) -> Option<&V> { $e.val() }
    };
    (ZipperForking $s: ident => $e:expr) => {
        fn fork_read_zipper<'a>(&'a $s) -> Self::ReadZipperT<'a> { $e.fork_read_zipper() }
    };
    (ZipperSubtries $s: ident => $e:expr) => {
        fn native_subtries(&$s) -> bool { $e.native_subtries() }
        fn try_make_map(&$s) -> Option<super::PathMap<V, A>> { $e.try_make_map() }
        fn trie_ref(&$s) -> Option<TrieRef<'_, V, A>> { $e.trie_ref() }
        fn alloc(&$s) -> A { $e.alloc() }
    };
    (ZipperMoving $s: ident => $e:expr) => {
        fn at_root(&$s) -> bool { $e.at_root() }
        fn reset(&mut $s) { $e.reset() }
        #[inline] fn path(&$s) -> &[u8] { $e.path() }
        fn val_count(&$s) -> usize { $e.val_count() }
        fn descend_to<K: AsRef<[u8]>>(&mut $s, k: K) { $e.descend_to(k) }
        fn descend_to_check<K: AsRef<[u8]>>(&mut $s, k: K) -> bool { $e.descend_to_check(k) }
        fn descend_to_existing<K: AsRef<[u8]>>(&mut $s, k: K) -> usize { $e.descend_to_existing(k) }
        fn descend_to_val<K: AsRef<[u8]>>(&mut $s, k: K) -> usize { $e.descend_to_val(k) }
        fn descend_to_byte(&mut $s, k: u8) { $e.descend_to_byte(k) }
        fn descend_to_existing_byte(&mut $s, k: u8) -> bool { $e.descend_to_existing_byte(k) }
        fn descend_indexed_byte(&mut $s, child_idx: usize) -> bool { $e.descend_indexed_byte(child_idx) }
        fn descend_first_byte(&mut $s) -> bool { $e.descend_first_byte() }
        fn descend_until(&mut $s) -> bool { $e.descend_until() }
        fn descend_until_max_bytes(&mut $s, max_bytes: usize) -> bool { $e.descend_until_max_bytes(max_bytes) }
        fn to_next_sibling_byte(&mut $s) -> bool { $e.to_next_sibling_byte() }
        fn to_prev_sibling_byte(&mut $s) -> bool { $e.to_prev_sibling_byte() }
        fn ascend(&mut $s, steps: usize) -> bool { $e.ascend(steps) }
        fn ascend_byte(&mut $s) -> bool { $e.ascend_byte() }
        fn ascend_until(&mut $s) -> bool { $e.ascend_until() }
        fn ascend_until_branch(&mut $s) -> bool { $e.ascend_until_branch() }
        fn to_next_step(&mut $s) -> bool { $e.to_next_step() }
    };
    (ZipperInfallibleSubtries $s: ident => $e:expr) => {
        fn make_map(&$s) -> super::PathMap<V, A> { $e.make_map() }
        fn get_trie_ref(&$s) -> TrieRef<'_, V, A> { $e.get_trie_ref() }
    };
    (ZipperReadOnlyValues $s: ident => $e:expr) => {
        fn get_val(&$s) -> Option<&'a V> { $e.get_val() }
    };
    (ZipperReadOnlyConditionalValues $s: ident => $e:expr) => {
        fn witness<'w>(&$s) -> Self::WitnessT { $e.witness() }
        fn get_val_with_witness<'w>(&$s, witness: &'w Self::WitnessT) -> Option<&'w V> where 'a: 'w { $e.get_val_with_witness(witness) }
    };
    (ZipperReadOnlyIteration $s: ident => $e:expr) => {
        fn to_next_get_val(&mut $s) -> Option<&'a V> { $e.to_next_get_val() }
    };
    (ZipperReadOnlyConditionalIteration $s: ident => $e:expr) => {
        fn to_next_get_val_with_witness<'w>(&mut $s, witness: &'w Self::WitnessT) -> Option<&'w V> where 'a: 'w { $e.to_next_get_val_with_witness(witness) }
    };
    (ZipperReadOnlySubtries $s: ident => $e:expr) => {
        type TrieRefT = <Z as ZipperReadOnlySubtries<'a, V, A>>::TrieRefT;
        fn trie_ref_at_path<K: AsRef<[u8]>>(&$s, path: K) -> Self::TrieRefT { $e.trie_ref_at_path(path) }
    };
    (ZipperConcrete $s: ident => $e:expr) => {
        #[inline] fn shared_node_id(&$s) -> Option<u64> { $e.shared_node_id() }
        #[inline] fn is_shared(&$s) -> bool { $e.is_shared() }
    };
    (ZipperPriv $s: ident => $e:expr) => {
        type V = V;
        type A = A;
        fn get_focus(&$s) -> AbstractNodeRef<'_, Self::V, Self::A> { $e.get_focus() }
        fn try_borrow_focus(&$s) -> Option<&TrieNodeODRc<Self::V, Self::A>> { $e.try_borrow_focus() }
    };
    (ZipperPathBuffer $s: ident => $e:expr) => {
        unsafe fn origin_path_assert_len(&$s, len: usize) -> &[u8] { unsafe{ $e.origin_path_assert_len(len) } }
        fn prepare_buffers(&mut $s) { $e.prepare_buffers() }
        fn reserve_buffers(&mut $s, path_len: usize, stack_depth: usize) { $e.reserve_buffers(path_len, stack_depth) }
    };
    (ZipperReadOnlyPriv $s: ident => $e:expr) => {
        fn borrow_raw_parts<'z>(&'z $s) -> (TaggedNodeRef<'z, V, A>, &'z [u8], Option<&'z V>) { $e.borrow_raw_parts() }
        fn take_core(&mut $s) -> Option<ReadZipperCore<'a, 'static, V, A>> { $e.take_core() }
    };
}


impl <Z : Zipper> Zipper for Box<Z> { zipper_impl_lens!(Zipper self => (**self)); }
impl <Z : ZipperAbsolutePath> ZipperAbsolutePath for Box<Z> { zipper_impl_lens!(ZipperAbsolutePath self => (**self)); }
impl <Z : ZipperMoving> ZipperMoving for Box<Z> { zipper_impl_lens!(ZipperMoving self => (**self)); }
impl <Z : ZipperIteration> ZipperIteration for Box<Z> { zipper_impl_lens!(ZipperIteration self => (**self)); }
impl <V, Z : ZipperValues<V>> ZipperValues<V> for Box<Z> { zipper_impl_lens!(ZipperValues self => (**self)); }
impl <V, Z : ZipperForking<V>> ZipperForking<V> for Box<Z> { type ReadZipperT<'a> = Z::ReadZipperT<'a> where Self: 'a; zipper_impl_lens!(ZipperForking self => (**self)); }
impl <V: Clone + Send + Sync, A: Allocator, Z : ZipperSubtries<V, A>> ZipperSubtries<V, A> for Box<Z> { zipper_impl_lens!(ZipperSubtries self => (**self)); }
impl <V: Clone + Send + Sync, A: Allocator, Z : ZipperInfallibleSubtries<V, A>> ZipperInfallibleSubtries<V, A> for Box<Z> { zipper_impl_lens!(ZipperInfallibleSubtries self => (**self)); }
impl<'a, V: Clone + Send + Sync, Z> ZipperReadOnlyValues<'a, V> for Box<Z> where Z: ZipperReadOnlyValues<'a, V>, Self: ZipperValues<V> { zipper_impl_lens!(ZipperReadOnlyValues self => (**self)); }
impl<'a, V: Clone + Send + Sync, Z> ZipperReadOnlyConditionalValues<'a, V> for Box<Z> where Z: ZipperReadOnlyConditionalValues<'a, V>, Self: ZipperValues<V> { type WitnessT = Z::WitnessT; zipper_impl_lens!(ZipperReadOnlyConditionalValues self => (**self)); }
impl<'a, V, Z> ZipperReadOnlyIteration<'a, V> for Box<Z> where Z: ZipperReadOnlyIteration<'a, V>, Self: ZipperReadOnlyValues<'a, V> + ZipperIteration { zipper_impl_lens!(ZipperReadOnlyIteration self => (**self)); }
impl<'a, V, Z> ZipperReadOnlyConditionalIteration<'a, V> for Box<Z> where Z: ZipperReadOnlyConditionalIteration<'a, V>, Self: ZipperReadOnlyConditionalValues<'a, V, WitnessT = Z::WitnessT> + ZipperIteration { zipper_impl_lens!(ZipperReadOnlyConditionalIteration self => (**self)); }
impl<'a, V: Clone + Send + Sync + 'a, Z, A: Allocator + 'a> ZipperReadOnlySubtries<'a, V, A> for Box<Z> where Z: ZipperReadOnlySubtries<'a, V, A>, Self: ZipperReadOnlyPriv<'a, V, A> + ZipperSubtries<V, A> { zipper_impl_lens!(ZipperReadOnlySubtries self => (**self)); }
impl<Z> ZipperConcrete for Box<Z> where Z: ZipperConcrete { zipper_impl_lens!(ZipperConcrete self => (**self)); }
impl<V: Clone + Send + Sync, Z, A: Allocator> ZipperPriv for Box<Z> where Z: ZipperPriv<V=V, A=A> { zipper_impl_lens!(ZipperPriv self => (**self)); }
impl<Z> ZipperPathBuffer for Box<Z> where Z: ZipperPathBuffer { zipper_impl_lens!(ZipperPathBuffer self => (**self)); }
impl<'a, V: Clone + Send + Sync, Z, A: Allocator> ZipperReadOnlyPriv<'a, V, A> for Box<Z> where Z: ZipperReadOnlyPriv<'a, V, A> { zipper_impl_lens!(ZipperReadOnlyPriv self => (**self)); }

impl <Z : Zipper> Zipper for &mut Z { zipper_impl_lens!(Zipper self => (**self)); }
impl <Z : ZipperAbsolutePath> ZipperAbsolutePath for &mut Z { zipper_impl_lens!(ZipperAbsolutePath self => (**self)); }
impl <Z : ZipperMoving> ZipperMoving for &mut Z { zipper_impl_lens!(ZipperMoving self => (**self)); }
impl <Z : ZipperIteration> ZipperIteration for &mut Z { zipper_impl_lens!(ZipperIteration self => (**self)); }
impl <V, Z : ZipperValues<V>> ZipperValues<V> for &mut Z { zipper_impl_lens!(ZipperValues self => (**self)); }
impl <V, Z : ZipperForking<V>> ZipperForking<V> for &mut Z { type ReadZipperT<'a> = Z::ReadZipperT<'a> where Self: 'a; zipper_impl_lens!(ZipperForking self => (**self)); }
impl <V: Clone + Send + Sync, A: Allocator, Z : ZipperSubtries<V, A>> ZipperSubtries<V, A> for &mut Z { zipper_impl_lens!(ZipperSubtries self => (**self)); }
impl <V: Clone + Send + Sync, A: Allocator, Z : ZipperInfallibleSubtries<V, A>> ZipperInfallibleSubtries<V, A> for &mut Z { zipper_impl_lens!(ZipperInfallibleSubtries self => (**self)); }
impl<'a, V: Clone + Send + Sync, Z> ZipperReadOnlyValues<'a, V> for &mut Z where Z: ZipperReadOnlyValues<'a, V>, Self: ZipperValues<V> { zipper_impl_lens!(ZipperReadOnlyValues self => (**self)); }
impl<'a, V: Clone + Send + Sync, Z> ZipperReadOnlyConditionalValues<'a, V> for &mut Z where Z: ZipperReadOnlyConditionalValues<'a, V>, Self: ZipperValues<V> { type WitnessT = Z::WitnessT; zipper_impl_lens!(ZipperReadOnlyConditionalValues self => (**self)); }
impl<'a, V, Z> ZipperReadOnlyIteration<'a, V> for &mut Z where Z: ZipperReadOnlyIteration<'a, V>, Self: ZipperReadOnlyValues<'a, V> + ZipperIteration { zipper_impl_lens!(ZipperReadOnlyIteration self => (**self)); }
impl<'a, V, Z> ZipperReadOnlyConditionalIteration<'a, V> for &mut Z where Z: ZipperReadOnlyConditionalIteration<'a, V>, Self: ZipperReadOnlyConditionalValues<'a, V, WitnessT = Z::WitnessT> + ZipperIteration { zipper_impl_lens!(ZipperReadOnlyConditionalIteration self => (**self)); }
impl<'a, V: Clone + Send + Sync + 'a, Z, A: Allocator + 'a> ZipperReadOnlySubtries<'a, V, A> for &mut Z where Z: ZipperReadOnlySubtries<'a, V, A>, Self: ZipperReadOnlyPriv<'a, V, A> + ZipperSubtries<V, A> { zipper_impl_lens!(ZipperReadOnlySubtries self => (**self)); }
impl<Z> ZipperConcrete for &mut Z where Z: ZipperConcrete { zipper_impl_lens!(ZipperConcrete self => (**self)); }
impl<V: Clone + Send + Sync, Z, A: Allocator> ZipperPriv for &mut Z where Z: ZipperPriv<V=V, A=A> { zipper_impl_lens!(ZipperPriv self => (**self)); }
impl<Z> ZipperPathBuffer for &mut Z where Z: ZipperPathBuffer { zipper_impl_lens!(ZipperPathBuffer self => (**self)); }
impl<'a, V: Clone + Send + Sync, Z, A: Allocator> ZipperReadOnlyPriv<'a, V, A> for &mut Z where Z: ZipperReadOnlyPriv<'a, V, A> { zipper_impl_lens!(ZipperReadOnlyPriv self => (**self)); }

/// Internal macro to implement Debug on a number of internal zipper types
macro_rules! impl_zipper_debug {
    (impl $($impl_tail:tt)*) => {
        impl $($impl_tail)* {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                let origin_path = super::utils::debug::render_debug_path(
                    self.origin_path(),
                    super::utils::debug::PathRenderMode::TryAscii
                ).unwrap();
                let prefix_len = self.root_prefix_path().len();
                f.debug_struct(core::any::type_name::<Self>())
                    .field("child_mask", &self.child_mask())
                    .field("prefix_len", &prefix_len)
                    .field("origin_path", &origin_path)
                    .finish()
            }
        }
    };
}
pub(crate) use impl_zipper_debug;

// ***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---
// ReadZipperTracked
// ***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---

/// A [Zipper] returned from a [ZipperHead] that is unable to modify the trie
#[derive(Clone)]
pub struct ReadZipperTracked<'a, 'path, V: Clone + Send + Sync, A: Allocator = GlobalAlloc> {
    z: ReadZipperCore<'a, 'path, V, A>,
    #[allow(unused)]
    tracker: Option<ZipperTracker<TrackingRead>>,
}

//The Drop impl ensures the tracker gets dropped at the right time
impl<V: Clone + Send + Sync, A: Allocator> Drop for ReadZipperTracked<'_, '_, V, A> {
    fn drop(&mut self) { }
}

impl<V: Clone + Send + Sync + Unpin, A: Allocator> Zipper for ReadZipperTracked<'_, '_, V, A> { zipper_impl_lens!(Zipper self => self.z); }
impl<V: Clone + Send + Sync + Unpin, A: Allocator> ZipperValues<V> for ReadZipperTracked<'_, '_, V, A>{ zipper_impl_lens!(ZipperValues self => self.z); }
impl<V: Clone + Send + Sync + Unpin, A: Allocator> ZipperSubtries<V, A> for ReadZipperTracked<'_, '_, V, A> { zipper_impl_lens!(ZipperSubtries self => self.z); }
impl<V: Clone + Send + Sync + Unpin, A: Allocator> ZipperInfallibleSubtries<V, A> for ReadZipperTracked<'_, '_, V, A> { zipper_impl_lens!(ZipperInfallibleSubtries self => self.z); }
impl<'trie, V: Clone + Send + Sync + Unpin + 'trie, A: Allocator + 'trie> ZipperMoving for ReadZipperTracked<'trie, '_, V, A> { zipper_impl_lens!(ZipperMoving self => self.z); }
impl<'a, V: Clone + Send + Sync + Unpin + 'a, A: Allocator + 'a> ZipperReadOnlyConditionalValues<'a, V> for ReadZipperTracked<'a, '_, V, A> { type WitnessT = ReadZipperWitness<V, A>; zipper_impl_lens!(ZipperReadOnlyConditionalValues self => self.z); }
impl<V: Clone + Send + Sync + Unpin, A: Allocator> ZipperConcrete for ReadZipperTracked<'_, '_, V, A> { zipper_impl_lens!(ZipperConcrete self => self.z); }
impl<V: Clone + Send + Sync + Unpin, A: Allocator> zipper_priv::ZipperPriv for ReadZipperTracked<'_, '_, V, A> { zipper_impl_lens!(ZipperPriv self => self.z); }
impl<'trie, V: Clone + Send + Sync + Unpin + 'trie, A: Allocator + 'trie> ZipperPathBuffer for ReadZipperTracked<'trie, '_, V, A> { zipper_impl_lens!(ZipperPathBuffer self => self.z); }
impl<'trie, V: Clone + Send + Sync + Unpin + 'trie, A: Allocator + 'trie> ZipperIteration for ReadZipperTracked<'trie, '_, V, A> { zipper_impl_lens!(ZipperIteration self => self.z); }
impl<'trie, V: Clone + Send + Sync + Unpin + 'trie, A: Allocator + 'trie> ZipperReadOnlyConditionalIteration<'trie, V> for ReadZipperTracked<'trie, '_, V, A> { }
impl<'trie, V: Clone + Send + Sync + Unpin + 'trie, A: Allocator + 'trie> ZipperAbsolutePath for ReadZipperTracked<'trie, '_, V, A> { zipper_impl_lens!(ZipperAbsolutePath self => self.z); }


impl<V: Clone + Send + Sync + Unpin, A: Allocator> ZipperForking<V> for ReadZipperTracked<'_, '_, V, A>{
    type ReadZipperT<'a> = ReadZipperUntracked<'a, 'a, V, A> where Self: 'a;
    fn fork_read_zipper<'a>(&'a self) -> Self::ReadZipperT<'a> {
        let forked_zipper = self.z.fork_read_zipper();
        Self::ReadZipperT::new_forked_with_inner_zipper(forked_zipper)
    }
}

impl<'a, V: Clone + Send + Sync + Unpin + 'a, A: Allocator + 'a> ZipperReadOnlySubtries<'a, V, A> for ReadZipperTracked<'a, '_, V, A> {
    type TrieRefT = TrieRefOwned<V, A>;
    fn trie_ref_at_path<K: AsRef<[u8]>>(&self, path: K) -> TrieRefOwned<V, A> {
        let path = path.as_ref();
        TrieRefOwned::new_with_key_and_path_in(self.z.focus_parent(), self.val(), self.z.node_key(), path, self.z.alloc.clone())
    }
}


impl<'a, V: Clone + Send + Sync + Unpin + 'a, A: Allocator + 'a> ZipperReadOnlyPriv<'a, V, A> for ReadZipperTracked<'a, '_, V, A> {
    fn borrow_raw_parts<'z>(&'z self) -> (TaggedNodeRef<'z, V, A>, &'z [u8], Option<&'z V>) { self.z.borrow_raw_parts() }
    fn take_core(&mut self) -> Option<ReadZipperCore<'a, 'static, V, A>> {
        let mut temp_core = ReadZipperCore::new_with_node_and_path_internal_in(OwnedOrBorrowed::None, &[], 0, None, self.z.alloc.clone());
        core::mem::swap(&mut temp_core, &mut self.z);
        Some(temp_core.make_static_path())
    }
}

impl<'a, 'path, V: Clone + Send + Sync + Unpin, A: Allocator + 'a> ReadZipperTracked<'a, 'path, V, A> {
    /// See [ReadZipperCore::new_with_node_and_path]
    pub(crate) fn new_with_node_and_path_in(root_node: &'a TrieNodeODRc<V, A>, owned_root: bool, path: &'path [u8], root_prefix_len: usize, root_key_start: usize, root_val: Option<&'a V>, alloc: A, tracker: Option<ZipperTracker<TrackingRead>>) -> Self {
        let core = ReadZipperCore::new_with_node_and_path_in(root_node, owned_root, path, root_prefix_len, root_key_start, root_val, alloc);
        Self { z: core, tracker }
    }
    /// See [ReadZipperCore::new_with_node_and_cloned_path]
    pub(crate) fn new_with_node_and_cloned_path_in(root_node: &'a TrieNodeODRc<V, A>, owned_root: bool, path: &[u8], root_prefix_len: usize, root_key_start: usize, root_val: Option<&'a V>, alloc: A, tracker: Option<ZipperTracker<TrackingRead>>) -> Self {
        let core = ReadZipperCore::new_with_node_and_cloned_path_in(root_node, owned_root, path, root_prefix_len, root_key_start, root_val, alloc);
        Self { z: core, tracker }
    }
}

//GOAT, the standard prototype of IntoIterator isn't compatible with ReadZipperTracked anymore because 
// we would need to require a witness to create the iterator.  We could add a special into_iter method
// but I will wait for somebody to ask for it.
//
// impl<'a, 'path, V: Clone + Send + Sync + Unpin + 'a, A: Allocator + 'a> std::iter::IntoIterator for ReadZipperTracked<'a, 'path, V, A> {
//     type Item = (Vec<u8>, &'a V);
//     type IntoIter = ReadZipperIter<'a, 'path, V, A>;

//     fn into_iter(self) -> Self::IntoIter {
//         //Destructure `self` without dropping it
//         let zip = core::mem::ManuallyDrop::new(self);
//         let core_z = unsafe { std::ptr::read(&zip.z) };
//         let tracker = unsafe { std::ptr::read(&zip.tracker) };
//         ReadZipperIter {
//             started: false,
//             zipper: Some(core_z),
//             _tracker: Some(tracker)
//         }
//     }
// }

impl_zipper_debug!(
    impl<'a, 'path, V: Clone + Send + Sync + Unpin + 'a, A: Allocator + 'a> core::fmt::Debug for ReadZipperTracked<'a, 'path, V, A>
);

// ***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---
// ReadZipperUntracked
// ***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---

/// A [Zipper] that is unable to modify the trie, used when it is possible to statically guarantee
/// non-interference between zippers
#[derive(Clone)]
pub struct ReadZipperUntracked<'a, 'path, V: Clone + Send + Sync, A: Allocator = GlobalAlloc> {
    z: ReadZipperCore<'a, 'path, V, A>,
}

impl<V: Clone + Send + Sync + Unpin, A: Allocator> Zipper for ReadZipperUntracked<'_, '_, V, A> { zipper_impl_lens!(Zipper self => self.z); }
impl<V: Clone + Send + Sync + Unpin, A: Allocator> ZipperValues<V> for ReadZipperUntracked<'_, '_, V, A> { zipper_impl_lens!(ZipperValues self => self.z); }
impl<V: Clone + Send + Sync + Unpin, A: Allocator> ZipperPriv for ReadZipperUntracked<'_, '_, V, A> { zipper_impl_lens!(ZipperPriv self => self.z); }
impl<'trie, V: Clone + Send + Sync + Unpin + 'trie, A: Allocator + 'trie> ZipperPathBuffer for ReadZipperUntracked<'trie, '_, V, A> { zipper_impl_lens!(ZipperPathBuffer self => self.z); }
impl<'trie, V: Clone + Send + Sync + Unpin + 'trie, A: Allocator + 'trie> ZipperIteration for ReadZipperUntracked<'trie, '_, V, A> { zipper_impl_lens!(ZipperIteration self => self.z); }
impl<'trie, V: Clone + Send + Sync + Unpin + 'trie, A: Allocator + 'trie> ZipperReadOnlyConditionalIteration<'trie, V> for ReadZipperUntracked<'trie, '_, V, A> { }
impl<'trie, V: Clone + Send + Sync + Unpin + 'trie, A: Allocator + 'trie> ZipperAbsolutePath for ReadZipperUntracked<'trie, '_, V, A> { zipper_impl_lens!(ZipperAbsolutePath self => self.z); }
impl<V: Clone + Send + Sync + Unpin, A: Allocator> ZipperSubtries<V, A> for ReadZipperUntracked<'_, '_, V, A> { zipper_impl_lens!(ZipperSubtries self => self.z); }
impl<V: Clone + Send + Sync + Unpin, A: Allocator> ZipperInfallibleSubtries<V, A> for ReadZipperUntracked<'_, '_, V, A> { zipper_impl_lens!(ZipperInfallibleSubtries self => self.z); }
impl<'trie, V: Clone + Send + Sync + Unpin + 'trie, A: Allocator + 'trie> ZipperMoving for ReadZipperUntracked<'trie, '_, V, A> { zipper_impl_lens!(ZipperMoving self => self.z); }
impl<V: Clone + Send + Sync + Unpin, A: Allocator> ZipperConcrete for ReadZipperUntracked<'_, '_, V, A> { zipper_impl_lens!(ZipperConcrete self => self.z); }

impl<V: Clone + Send + Sync + Unpin, A: Allocator> ZipperForking<V> for ReadZipperUntracked<'_, '_, V, A> {
    type ReadZipperT<'a> = ReadZipperUntracked<'a, 'a, V, A> where Self: 'a;
    fn fork_read_zipper<'a>(&'a self) -> Self::ReadZipperT<'a> {
        let forked_zipper = self.z.fork_read_zipper();
        Self::ReadZipperT::new_forked_with_inner_zipper(forked_zipper)
    }
}

impl<'trie, V: Clone + Send + Sync + Unpin + 'trie, A: Allocator + 'trie> ZipperReadOnlyValues<'trie, V> for ReadZipperUntracked<'trie, '_, V, A> {
    fn get_val(&self) -> Option<&'trie V> { unsafe{ self.z.get_val() } }
}

impl<'trie, V: Clone + Send + Sync + Unpin + 'trie, A: Allocator + 'trie> ZipperReadOnlyConditionalValues<'trie, V> for ReadZipperUntracked<'trie, '_, V, A> {
    type WitnessT = ();
    fn witness<'w>(&self) -> Self::WitnessT { () }
    fn get_val_with_witness<'w>(&self, _witness: &'w Self::WitnessT) -> Option<&'w V> where 'trie: 'w { self.get_val() }
}

impl<'a, V: Clone + Send + Sync + Unpin + 'a, A: Allocator + 'a> ZipperReadOnlySubtries<'a, V, A> for ReadZipperUntracked<'a, '_, V, A> {
    type TrieRefT = TrieRefBorrowed<'a, V, A>;
    fn trie_ref_at_path<K: AsRef<[u8]>>(&self, path: K) -> TrieRefBorrowed<'a, V, A> {
        let path = path.as_ref();
        TrieRefBorrowed::new_with_key_and_path_in(self.z.focus_parent_borrowed(), self.get_val(), self.z.node_key(), path, self.z.alloc.clone())
    }
}

impl<'a, V: Clone + Send + Sync + Unpin + 'a, A: Allocator + 'a> ZipperReadOnlyPriv<'a, V, A> for ReadZipperUntracked<'a, '_, V, A>{
    fn borrow_raw_parts<'z>(&'z self) -> (TaggedNodeRef<'z, V, A>, &'z [u8], Option<&'z V>) { self.z.borrow_raw_parts() }
    fn take_core(&mut self) -> Option<ReadZipperCore<'a, 'static, V, A>> {
        let mut temp_core = ReadZipperCore::new_with_node_and_path_internal_in(OwnedOrBorrowed::None, &[], 0, None, self.z.alloc.clone());
        core::mem::swap(&mut temp_core, &mut self.z);
        Some(temp_core.make_static_path())
    }
}


impl<'trie, V: Clone + Send + Sync + Unpin + 'trie, A: Allocator + 'trie> ZipperReadOnlyIteration<'trie, V> for ReadZipperUntracked<'trie, '_, V, A> {
    fn to_next_get_val(&mut self) -> Option<&'trie V> { unsafe{ self.z.to_next_get_val() } }
}

impl<'a, 'path, V: Clone + Send + Sync + Unpin + 'a, A: Allocator + 'a> ReadZipperUntracked<'a, 'path, V, A> {
    /// See [ReadZipperCore::new_with_node_and_path]
    pub(crate) fn new_with_node_and_path_in(root_node: &'a TrieNodeODRc<V, A>, path: &'path [u8], root_prefix_len: usize, root_key_start: usize, root_val: Option<&'a V>, alloc: A) -> Self {
        let core = ReadZipperCore::new_with_node_and_path_in(root_node, false, path, root_prefix_len, root_key_start, root_val, alloc);
        Self { z: core }
    }
    /// See [ReadZipperCore::new_with_node_and_path_internal]
    pub(crate) fn new_with_node_and_path_internal_in(root_node: &'a TrieNodeODRc<V, A>, path: &'path [u8], root_key_start: usize, root_val: Option<&'a V>, alloc: A) -> Self {
        let core = ReadZipperCore::new_with_node_and_path_internal_in(OwnedOrBorrowed::Borrowed(root_node), path, root_key_start, root_val, alloc);
        Self { z: core }
    }
    /// See [ReadZipperCore::new_with_node_and_cloned_path]
    pub(crate) fn new_with_node_and_cloned_path_in(root_node: &'a TrieNodeODRc<V, A>, path: &[u8], root_prefix_len: usize, root_key_start: usize, root_val: Option<&'a V>, alloc: A) -> Self {
        let core = ReadZipperCore::new_with_node_and_cloned_path_in(root_node, false, path, root_prefix_len, root_key_start, root_val, alloc);
        Self { z: core }
    }
    /// Makes a new `ReadZipperUntracked` for use in the implementation of [Zipper::fork_read_zipper].
    /// Forked zippers never need to be tracked because they are always fully covered by their parent's permissions
    pub(crate) fn new_forked_with_inner_zipper(core: ReadZipperCore<'a, 'path, V, A>) -> Self {
        ReadZipperUntracked{ z: core }
    }
}

impl<'a, 'path, V: Clone + Send + Sync + Unpin + 'a, A: Allocator + 'a> ReadZipperUntracked<'a, 'path, V, A> {
    /// Consumes the zipper and returns the `Vec` storing its path
    ///
    /// The returned `Vec` will contain bytes equivalent to those provided by [origin_path](ZipperAbsolutePath::origin_path),
    /// unless the internal path has not yet been initialized, in which case it will return `vec![]`.
    pub fn into_path(self) -> Vec<u8> {
        self.z.into_path()
    }

    /// Consumes the zipper and returns an iterator over all terminating paths reachable from the current focus,
    /// including paths that do not hold values
    pub fn into_path_iter(self) -> ReadZipperPathIter<'a, 'path, V, A> {
        ReadZipperPathIter { zipper: Some(self.z) }
    }
}

impl<'a, 'path, V: Clone + Send + Sync + Unpin + 'a, A: Allocator + 'a> std::iter::IntoIterator for ReadZipperUntracked<'a, 'path, V, A> {
    type Item = (Vec<u8>, &'a V);
    type IntoIter = ReadZipperIter<'a, 'path, V, A>;

    fn into_iter(self) -> Self::IntoIter {
        ReadZipperIter {
            started: false,
            zipper: Some(self.z),
        }
    }
}
super::impl_name_only_debug!(
    impl<V: Clone + Send + Sync + Unpin, A: Allocator> core::fmt::Debug for ReadZipperIter<'_, '_, V, A>
);
super::impl_name_only_debug!(
    impl<V: Clone + Send + Sync + Unpin, A: Allocator> core::fmt::Debug for ReadZipperPathIter<'_, '_, V, A>
);

impl_zipper_debug!(
    impl<'a, 'path, V: Clone + Send + Sync + Unpin + 'a, A: Allocator + 'a> core::fmt::Debug for ReadZipperUntracked<'a, 'path, V, A>
);

// ***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---
// ReadZipperOwned
// ***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---

/// A [Zipper] that holds ownership of the root node, so there is no need for a lifetime parameter
//
//GOAT ReadZipperOwned doesn't need its internal map anymore.
// Then, ReadZipperTracked is then just a ReadZipperOwned plus a tracker
//**UPDATE** TOo early to make this change; because the root value still lives outside the ReadZipper.  We could move to an owned root value, like we did for TrieRefOwned, but I would rather just push ahead with moving root values inside nodes.  For now I am just trying to get to the right external API.
pub struct ReadZipperOwned<V: Clone + Send + Sync + 'static, A: Allocator + 'static = GlobalAlloc> {
    map: MaybeDangling<Box<PathMap<V, A>>>,
    // NOTE About this Box around the WriteZipperCore... The reason this is needed is for the
    // [WriteZipperOwned::into_map] method.  This box effectively provides a fence, ensuring that the
    // `&mut` references to the `map` and the `prefix_path` are totally gone before we access `map`.
    // But I would like to find a zero-cost way to accomplish the same thing without the indirection.
    //
    // UPDATE: I could give the ReadZipperCore the same ptr treatment that I did to WriteZipper with the
    // WZNodePtr, although it's likely easier for the ReadZipperCore because we don't have to worry about
    // mutability and the constraints of the MutCursorRootedVec
    z: Box<ReadZipperCore<'static, 'static, V, A>>,
}

impl<V: 'static + Clone + Send + Sync + Unpin, A: Allocator> Clone for ReadZipperOwned<V, A> {
    fn clone(&self) -> Self {
        let new_map = (**self.map).clone();
        Self::new_with_map(new_map, self.root_prefix_path())
    }
}

impl<V: 'static + Clone + Send + Sync + Unpin, A: Allocator> ReadZipperOwned<V, A> {
    /// See [ReadZipperCore::new_with_node_and_cloned_path]
    pub(crate) fn new_with_map<K: AsRef<[u8]>>(map: PathMap<V, A>, path: K) -> Self {
        map.ensure_root();
        let alloc = map.alloc.clone();
        let path = path.as_ref();
        let map = MaybeDangling::new(Box::new(map));
        let root_ref = unsafe{ &*(*map).root.get() }.as_ref().unwrap();
        let root_val = Option::as_ref( unsafe{ &*(*map).root_val.get() } );
        let core = ReadZipperCore::new_with_node_and_cloned_path_in(root_ref, true, path, path.len(), 0, root_val, alloc);
        Self { map, z: Box::new(core) }
    }
    /// Consumes the zipper and returns a map contained within the zipper
    pub fn into_map(self) -> PathMap<V, A> {
        drop(self.z);
        let map = MaybeDangling::into_inner(self.map);
        *map
    }
}


impl<V: Clone + Send + Sync + Unpin, A: Allocator> Zipper for ReadZipperOwned<V, A> { zipper_impl_lens!(Zipper self => self.z); }
impl<V: Clone + Send + Sync + Unpin, A: Allocator> ZipperSubtries<V, A> for ReadZipperOwned<V, A> { zipper_impl_lens!(ZipperSubtries self => self.z); }
impl<V: Clone + Send + Sync + Unpin, A: Allocator> ZipperInfallibleSubtries<V, A> for ReadZipperOwned<V, A> { zipper_impl_lens!(ZipperInfallibleSubtries self => self.z); }
impl<'trie, V: Clone + Send + Sync + Unpin + 'trie, A: Allocator + 'trie> ZipperMoving for ReadZipperOwned<V, A> { zipper_impl_lens!(ZipperMoving self => self.z); }
impl<V: Clone + Send + Sync + Unpin, A: Allocator> ZipperConcrete for ReadZipperOwned<V, A> { zipper_impl_lens!(ZipperConcrete self => self.z); }
impl<V: Clone + Send + Sync + Unpin, A: Allocator> ZipperPriv for ReadZipperOwned<V, A> { zipper_impl_lens!(ZipperPriv self => self.z); }
impl<'trie, V: Clone + Send + Sync + Unpin + 'trie, A: Allocator + 'trie> ZipperPathBuffer for ReadZipperOwned<V, A> { zipper_impl_lens!(ZipperPathBuffer self => self.z); }
impl<'trie, V: Clone + Send + Sync + Unpin + 'trie, A: Allocator + 'trie> ZipperIteration for ReadZipperOwned<V, A> { zipper_impl_lens!(ZipperIteration self => self.z); }
impl<'trie, V: Clone + Send + Sync + Unpin + 'trie, A: Allocator + 'trie> ZipperReadOnlyConditionalIteration<'trie, V> for ReadZipperOwned<V, A> { }
impl<'trie, V: Clone + Send + Sync + Unpin + 'trie, A: Allocator + 'trie> ZipperAbsolutePath for ReadZipperOwned<V, A> { zipper_impl_lens!(ZipperAbsolutePath self => self.z); }


impl<V: Clone + Send + Sync + Unpin, A: Allocator> ZipperValues<V> for ReadZipperOwned<V, A> {
    fn val(&self) -> Option<&V> { unsafe{ self.z.get_val() } }
}

impl<V: Clone + Send + Sync + Unpin, A: Allocator> ZipperForking<V> for ReadZipperOwned<V, A> {
    type ReadZipperT<'a> = ReadZipperUntracked<'a, 'a, V, A> where Self: 'a;
    fn fork_read_zipper<'a>(&'a self) -> Self::ReadZipperT<'a> {
        let forked_zipper = self.z.fork_read_zipper();
        Self::ReadZipperT::new_forked_with_inner_zipper(forked_zipper)
    }
}

impl<'trie, V: Clone + Send + Sync + Unpin + 'trie, A: Allocator + 'trie> ZipperReadOnlyConditionalValues<'trie, V> for ReadZipperOwned<V, A> {
    type WitnessT = ReadZipperWitness<V, A>;
    fn witness<'w>(&self) -> ReadZipperWitness<V, A> { self.z.witness() }
    fn get_val_with_witness<'w>(&self, witness: &'w ReadZipperWitness<V, A>) -> Option<&'w V> where 'trie: 'w { self.z.get_val_with_witness(witness) }
}

impl<'a, V: Clone + Send + Sync + Unpin, A: Allocator> ZipperReadOnlySubtries<'a, V, A> for ReadZipperOwned<V, A> where Self: 'a {
    type TrieRefT = TrieRefOwned<V, A>;
    fn trie_ref_at_path<K: AsRef<[u8]>>(&self, path: K) -> TrieRefOwned<V, A> {
        let path = path.as_ref();
        TrieRefOwned::new_with_key_and_path_in(self.z.focus_parent(), self.val(), self.z.node_key(), path, self.z.alloc.clone())
    }
}


impl<'a, V: Clone + Send + Sync + Unpin, A: Allocator> ZipperReadOnlyPriv<'a, V, A> for ReadZipperOwned<V, A> where Self: 'a {
    fn borrow_raw_parts<'z>(&'z self) -> (TaggedNodeRef<'z, V, A>, &'z [u8], Option<&'z V>) { self.z.borrow_raw_parts() }
    fn take_core(&mut self) -> Option<ReadZipperCore<'a, 'static, V, A>> {
        let mut temp_core = ReadZipperCore::new_with_node_and_path_internal_in(OwnedOrBorrowed::None, &[], 0, None, self.z.alloc.clone());
        core::mem::swap(&mut temp_core, &mut self.z);
        Some(temp_core.make_static_path())
    }
}


impl_zipper_debug!(
    impl<V: Clone + Send + Sync + Unpin, A: Allocator> core::fmt::Debug for ReadZipperOwned<V, A>
);

// ***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---
// ReadZipperCore (the actual implementation)
// ***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---***---

/// Size of node stack to preallocate in the zipper
pub(crate) const EXPECTED_DEPTH: usize = 16;

/// Size in bytes to preallocate path storage in the zipper
pub(crate) const EXPECTED_PATH_LEN: usize = 64;

pub(crate) mod read_zipper_core {
    use super::super::trie_core::node::*;
    use super::super::trie_core::r#ref::*;
    use super::super::PathMap;
    use super::*;

    /// A [Zipper] that is unable to modify the trie
    ///
    /// (Internal type, but in private module so it can be part of sealed interface)
    pub struct ReadZipperCore<'a, 'path, V: Clone + Send + Sync, A: Allocator> {
        /// A reference to the entire origin path, of which `root_key` is the final subset
        origin_path: SliceOrLen<'path>,
        /// The byte offset in `origin_path` for the start of the root node's key.
        /// `root_key = origin_path[root_key_start..]`
        root_key_start: usize,
        /// The byte offset in `origin_path` for the start of the key in `root_node` that produces the
        /// zipper's root.  This is needed to access to the root value if we have a ReadZipper that was
        /// descended from a [ZipperHead], and thus the `root_node` field is `Owned`
        /// In the cases where this field is not used, it will be set to [usize::MAX]
        root_parent_key_start: usize,
        /// A special-case to access a value at the root node, because that value would be otherwise inaccessible
        /// NOTE: `root_val` will be `None`, except in situations where `ReadZipper` at the root of a map or `ZipperHead`
        root_val: Option<&'a V>,
        /// The [TrieNodeODRc] that contains the root node from which the zipper is descended. If the zipper
        /// is descended from a `ZipperHead`, this field will be `Owned`, otherwise it will be `Borrowed`
        root_node: OwnedOrBorrowed<'a, TrieNodeODRc<V, A>>,
        /// A reference to the focus node
        focus_node: MiriWrapper<TaggedNodeRef<'a, V, A>>,
        /// An iter token corresponding to the location of the `node_key` within the `focus_node`, or NODE_ITER_INVALID
        /// if iteration is not in-process
        focus_iter_token: u128,
        /// Stores the entire path from the root node, including the bytes from `root_key`
        prefix_buf: Vec<u8>,
        /// Stores a stack of parent node references.  Does not include the focus_node
        /// The tuple contains: `(node_ref, iter_token, key_offset_in_prefix_buf)`
        ancestors: Vec<(TaggedNodeRef<'a, V, A>, u128, usize)>,
        pub(crate) alloc: A,
    }

    //GOAT-TODO, we should unify this `OwnedOrBorrowed` type with [`AbstractNodeRef`], and it should be able to
    // be packed into a single 64-bit word, and do the right thing when it is dropped.
    #[derive(Clone, Debug)]
    pub enum OwnedOrBorrowed<'a, T> {
        Owned(T),
        Borrowed(&'a T),
        None,
    }

    impl<'a, T> From<Option<&'a T>> for OwnedOrBorrowed<'a, T> {
        fn from(opt: Option<&'a T>) -> Self {
            match opt {
                Some(t) => Self::Borrowed(t),
                None => Self::None
            }
        }
    }

    impl<'a, T> OwnedOrBorrowed<'a, T> {
        /// Returns a reference to the content, regardless of whether it is owned or borrowed
        #[inline]
        pub fn as_ref(&self) -> &T {
            match self {
                Self::Owned(t) => &t,
                Self::Borrowed(t) => t,
                Self::None => panic!(),
            }
        }
        //GOAT, may be unneeded
        // /// Returns a reference to the 
        // #[inline]
        // pub fn as_option(&self) -> Option<&T> {
        //     match self {
        //         Self::Owned(t) => Some(&t),
        //         Self::Borrowed(t) => Some(t),
        //         Self::None => None,
        //     }
        // }
        /// Returns a reference to the content in the reference lifetime, if it's borrowed.  Panics if the content is owned
        pub fn as_borrowed_ref(&self) -> &'a T {
            match self {
                Self::Borrowed(t) => t,
                Self::Owned(_) => panic!(),
                Self::None => panic!(),
            }
        }
        /// Returns a reference to the owned content, or `None` if the content is borrowed
        pub fn get_owned_ref(&self) -> Option<&T> {
            match self {
                Self::Owned(t) => Some(&t),
                Self::Borrowed(_) => None,
                Self::None => None,
            }
        }
        /// Returns `true` if the content is owned, or `false` otherwise
        pub fn is_owned(&self) -> bool {
            match self {
                Self::Owned(_) => true,
                Self::Borrowed(_) => false,
                Self::None => false,
            }
        }
        //GOAT, maybe unneeded
        // /// Returns `true` if the content is borrowed, or `false` otherwise
        // pub fn is_borrowed(&self) -> bool {
        //     match self {
        //         Self::Borrowed(_) => true,
        //         Self::Owned(_) => false,
        //         Self::None => false,
        //     }
        // }
    }

    #[cfg(miri)]
    type MiriWrapper<T> = Box<T>;

    #[cfg(not(miri))]
    use miri_wrapper::MiriWrapper;
    mod miri_wrapper {
        use std::ops::{Deref, DerefMut};

        /// A type that appears to be a Box but does nothing, so I can replace it with a box under miri
        ///
        /// The issue is how miri scopes its pointer tags, which make self-referential types fail
        /// when they are dropped, because the reference and the referant get dropped within the same
        /// scope.
        #[derive(Clone, Debug)]
        pub struct MiriWrapper<T>(T);

        impl<T> Deref for MiriWrapper<T> {
            type Target = T;
            #[inline]
            fn deref(&self) -> &T {
                &self.0
            }
        }
        impl<T> DerefMut for MiriWrapper<T> {
            #[inline]
            fn deref_mut(&mut self) -> &mut T {
                &mut self.0
            }
        }
        impl<T> MiriWrapper<T> {
            pub fn new(t: T) -> Self {
                Self(t)
            }
        }
    }

    impl<V: Clone + Send + Sync, A: Allocator> Clone for ReadZipperCore<'_, '_, V, A> where V: Clone {
        fn clone(&self) -> Self {
            Self {
                origin_path: self.origin_path.clone(),
                root_key_start: self.root_key_start,
                root_parent_key_start: self.root_parent_key_start,
                root_val: self.root_val,
                root_node: self.root_node.clone(),
                focus_node: self.focus_node.clone(),
                focus_iter_token: NODE_ITER_INVALID,
                prefix_buf: self.prefix_buf.clone(),
                ancestors: self.ancestors.clone(),
                alloc: self.alloc.clone(),
            }
        }
    }

    impl<V: Clone + Send + Sync + Unpin, A: Allocator> Zipper for ReadZipperCore<'_, '_, V, A> {
        #[inline]
        fn path_exists(&self) -> bool {
            let key = self.node_key();
            if key.len() > 0 {
                self.focus_node.node_contains_partial_key(key)
            } else {
                true
            }
        }
        fn is_val(&self) -> bool {
            self.is_val_internal()
        }
        fn child_count(&self) -> usize {
            debug_assert!(self.is_regularized());
            self.focus_node.count_branches(self.node_key())
        }
        fn child_mask(&self) -> ByteMask {
            debug_assert!(self.is_regularized());
            self.focus_node.node_branches_mask(self.node_key())
        }
    }

    impl<V: Clone + Send + Sync + Unpin, A: Allocator> ZipperValues<V> for ReadZipperCore<'_, '_, V, A> {
        fn val(&self) -> Option<&V> { unsafe{ self.get_val() } }
    }

    impl<V: Clone + Send + Sync + Unpin, A: Allocator> ZipperForking<V> for ReadZipperCore<'_, '_, V, A> {
        type ReadZipperT<'a> = ReadZipperCore<'a, 'a, V, A> where Self: 'a;
        fn fork_read_zipper<'a>(&'a self) -> Self::ReadZipperT<'a> {
            let new_root_val = self.val();
            let new_root_path = self.origin_path();
            let new_root_key_start = new_root_path.len() - self.node_key().len();
            Self::ReadZipperT::new_with_node_and_path_internal_in(OwnedOrBorrowed::Borrowed(self.focus_parent()), new_root_path, new_root_key_start, new_root_val, self.alloc.clone())
        }
    }

    impl<V: Clone + Send + Sync + Unpin, A: Allocator> ZipperSubtries<V, A> for ReadZipperCore<'_, '_, V, A> {
        fn native_subtries(&self) -> bool { true }
        fn try_make_map(&self) -> Option<PathMap<V, A>> {
            Some(self.make_map())
        }
        fn trie_ref(&self) -> Option<TrieRef<'_, V, A>> {
            Some(self.get_trie_ref())
        }
        fn alloc(&self) -> A { self.alloc.clone() }
    }

    impl<V: Clone + Send + Sync + Unpin, A: Allocator> ZipperInfallibleSubtries<V, A> for ReadZipperCore<'_, '_, V, A> {
        fn make_map(&self) -> PathMap<V, A> {
            #[cfg(not(feature = "graft_root_vals"))]
            let root_val = None;
            #[cfg(feature = "graft_root_vals")]
            let root_val = self.val().cloned();

            let root_node = self.get_focus().into_option();
            PathMap::new_with_root_in(root_node, root_val, self.alloc.clone())
        }
        fn get_trie_ref(&self) -> TrieRef<'_, V, A> {
            TrieRefBorrowed::new_with_key_and_path_in(self.focus_parent_borrowed(), self.val(), self.node_key(), b"", self.alloc.clone()).into()
        }
    }


    impl<'trie, V: Clone + Send + Sync + Unpin + 'trie, A: Allocator + 'trie> ZipperMoving for ReadZipperCore<'trie, '_, V, A> {
        fn at_root(&self) -> bool {
            self.prefix_buf.len() <= self.origin_path.len()
        }

        fn reset(&mut self) {
            self.ancestors.truncate(1);
            match self.ancestors.pop() {
                Some((node, _tok, _prefix_len)) => {
                    *self.focus_node = node;
                    self.focus_iter_token = NODE_ITER_INVALID;
                },
                None => {}
            }
            self.prefix_buf.truncate(self.origin_path.len());
        }

        #[inline]
        fn path(&self) -> &[u8] {
            if self.prefix_buf.len() > 0 {
                &self.prefix_buf[self.origin_path.len()..]
            } else {
                &[]
            }
        }

        fn val_count(&self) -> usize {
            let root_val = self.is_val() as usize;
            if self.node_key().len() == 0 {
                val_count_below_root(*self.focus_node) + root_val
            } else {
                let focus = self.get_focus();
                if focus.is_none() {
                    root_val
                } else {
                    val_count_below_root(focus.as_tagged()) + root_val
                }
            }
        }
        fn descend_to<K: AsRef<[u8]>>(&mut self, k: K) {
            let k = k.as_ref();
            if k.len() == 0 {
                return //Zero-length path is a no-op
            }

            self.prepare_buffers();
            debug_assert!(self.is_regularized());

            self.descend_to_internal(k);
        }

        fn descend_to_check<K: AsRef<[u8]>>(&mut self, k: K) -> bool {
            let k = k.as_ref();
            if k.len() == 0 {
                return self.path_exists() //Zero-length path is a no-op
            }

            self.prepare_buffers();
            debug_assert!(self.is_regularized());

            let (borrowed_self, key) = self.descend_to_internal(k);
            if key.len() == 0 {
                debug_assert!(self.path_exists());
                true
            } else {
                borrowed_self.focus_node.node_contains_partial_key(key)
            }
        }

        #[inline]
        fn descend_to_byte(&mut self, k: u8) {
            self.prepare_buffers();
            debug_assert!(self.is_regularized());

            self.prefix_buf.push(k);
            self.focus_iter_token = NODE_ITER_INVALID;
            if let Some((_consumed_byte_cnt, next_node)) = self.focus_node.node_get_child(self.node_key()) {
                let next_node = next_node.as_tagged();
                self.ancestors.push((*self.focus_node, self.focus_iter_token, self.prefix_buf.len()));
                *self.focus_node = next_node;
            }
        }

        #[inline]
        fn descend_to_existing_byte(&mut self, k: u8) -> bool {
            self.prepare_buffers();
            debug_assert!(self.is_regularized());

            self.prefix_buf.push(k);
            let node_key = self.node_key();
            if let Some((_consumed_byte_cnt, next_node)) = self.focus_node.node_get_child(node_key) {
                self.focus_iter_token = NODE_ITER_INVALID;
                let next_node = next_node.as_tagged();
                self.ancestors.push((*self.focus_node, self.focus_iter_token, self.prefix_buf.len()));
                *self.focus_node = next_node;
                return true;
            }
            if self.focus_node.node_contains_partial_key(node_key) {
                true
            } else {
                self.prefix_buf.pop();
                false
            }
        }

        fn descend_indexed_byte(&mut self, child_idx: usize) -> bool {
            self.prepare_buffers();
            debug_assert!(self.is_regularized());

            match self.focus_node.nth_child_from_key(self.node_key(), child_idx) {
                (Some(prefix), Some(child_node)) => {
                    self.prefix_buf.push(prefix);
                    self.ancestors.push((*self.focus_node.clone(), self.focus_iter_token, self.prefix_buf.len()));
                    *self.focus_node = child_node;
                    self.focus_iter_token = NODE_ITER_INVALID;
                    true
                },
                (Some(prefix), None) => {
                    self.prefix_buf.push(prefix);
                    self.focus_iter_token = NODE_ITER_INVALID;
                    true
                },
                (None, _) => false
            }
        }

        fn descend_first_byte(&mut self) -> bool {
            self.prepare_buffers();
            debug_assert!(self.is_regularized());
            let cur_tok = self.focus_node.iter_token_for_path(self.node_key());
            self.focus_iter_token = cur_tok;

            let (new_tok, key_bytes, child_node, _value) = self.focus_node.next_items(self.focus_iter_token);

            if new_tok != NODE_ITER_FINISHED {
                let byte_idx = self.node_key().len();
                if byte_idx >= key_bytes.len() {
                    debug_assert!(self.is_regularized());
                    return false; //We can't go any deeper down this path
                }
                self.focus_iter_token = new_tok;
                self.prefix_buf.push(key_bytes[byte_idx]);

                if key_bytes.len() == byte_idx+1 {
                    match child_node {
                        None => {},
                        Some(rec) => {
                            self.ancestors.push((*self.focus_node.clone(), new_tok, self.prefix_buf.len()));
                            *self.focus_node = rec.as_tagged();
                            self.focus_iter_token = self.focus_node.new_iter_token();
                        },
                    }
                }
                debug_assert!(self.is_regularized());
                true
            } else {
                self.focus_iter_token = new_tok;
                debug_assert!(self.is_regularized());
                false
            }
        }

        fn descend_until(&mut self) -> bool {
            debug_assert!(self.is_regularized());
            let mut moved = false;
            while self.child_count() == 1 {
                moved = true;
                self.descend_first();
                if self.is_val_internal() {
                    break;
                }
            }
            moved
        }

        fn descend_until_max_bytes(&mut self, max_bytes: usize) -> bool {
            if max_bytes == 0 {
                return false;
            }
            debug_assert!(self.is_regularized());
            let mut remaining = max_bytes;
            let mut moved = false;
            while self.child_count() == 1 && remaining > 0 {
                self.prepare_buffers();
                let (prefix_opt, child_node_opt) = self.focus_node.first_child_from_key(self.node_key());
                let Some(prefix) = prefix_opt else { unreachable!() };

                if prefix.len() == 0 {
                    // Move to child node without consuming bytes.
                    if let Some(child_node) = child_node_opt {
                        moved = true;
                        self.ancestors.push((*self.focus_node.clone(), self.focus_iter_token, self.prefix_buf.len()));
                        *self.focus_node = child_node;
                        self.focus_iter_token = NODE_ITER_INVALID;
                        continue;
                    } else {
                        break;
                    }
                }

                let take = remaining.min(prefix.len());
                moved = true;
                self.prefix_buf.extend(&prefix[..take]);
                remaining -= take;

                if take < prefix.len() {
                    break;
                }

                if let Some(child_node) = child_node_opt {
                    self.ancestors.push((*self.focus_node.clone(), self.focus_iter_token, self.prefix_buf.len()));
                    *self.focus_node = child_node;
                    self.focus_iter_token = NODE_ITER_INVALID;
                }

                if self.is_val_internal() {
                    break;
                }
            }
            moved
        }

        fn descend_to_existing<K: AsRef<[u8]>>(&mut self, k: K) -> usize {
            let mut k = k.as_ref();
            if k.len() == 0 {
                return 0 //Zero-length path is a no-op
            }
            self.prepare_buffers();
            debug_assert!(self.is_regularized());

            let original_path_len = self.prefix_buf.len();
            let mut key_start = self.node_key_start();

            //Early out if we're on a non-existent path
            if key_start < self.prefix_buf.len() && !self.focus_node.node_contains_partial_key(&self.prefix_buf[key_start..]) {
                return 0
            }

            //Descend through all the existing nodes
            //
            //NOTE: One of the advantages of `descend_to_existing` vs ordinary `descend_to` is that it
            // avoids copying the whole path argument into the path buffer unless that's actually needed.
            // So this loop copies the path arg in chunks.  If we didn't care about this, we could just
            // grow the path buffer in one call with `self.descend_to_internal(k)`, like `descend_to` does
            const CHUNK_SIZE: usize = 64;
            debug_assert!(CHUNK_SIZE >= MAX_NODE_KEY_BYTES);
            while k.len() > 0 {
                let (chunk, remaining) = if k.len() > CHUNK_SIZE {
                    (&k[..CHUNK_SIZE], &k[CHUNK_SIZE..])
                } else {
                    (k, &[][..])
                };
                let _ = self.descend_to_internal(chunk);
                let new_key_start = self.node_key_start();
                if new_key_start == key_start {
                    break;
                }
                key_start = new_key_start;
                k = remaining;
            }

            //Now trim the buffer to the length of the last existing path within the node
            let node_key = &self.prefix_buf[key_start..];
            let overlap = if node_key.len() > 0 {
                self.focus_node.node_key_overlap(node_key)
            } else {
                0
            };
            self.prefix_buf.truncate(key_start+overlap);

            debug_assert!(self.is_regularized());
            self.prefix_buf.len() - original_path_len
        }

        //GOAT, WIP.  I think `node_first_val_depth_along_key` needs to change in order to
        // ignore values with a key length smaller than a specified length
        // fn descend_to_val<K: AsRef<[u8]>>(&mut self, k: K) -> usize {
        //     let mut k = k.as_ref();
        //     if k.len() == 0 {
        //         return 0 //Zero-length path is a no-op
        //     }
        //     self.prepare_buffers();
        //     debug_assert!(self.is_regularized());

        //     self.focus_node.node_first_val_depth_along_key();
        // }

        fn to_next_sibling_byte(&mut self) -> bool {
            self.prepare_buffers();
            if self.prefix_buf.len() == 0 {
                return false
            }
            debug_assert!(self.is_regularized());
            self.deregularize();
            if self.focus_iter_token == NODE_ITER_INVALID {
                let cur_tok = self.focus_node.iter_token_for_path(self.node_key());
                self.focus_iter_token = cur_tok;
            }

            if self.focus_iter_token == NODE_ITER_FINISHED {
                self.regularize();
                return false
            }

            let (mut new_tok, mut key_bytes, mut child_node, mut _value) = self.focus_node.next_items(self.focus_iter_token);
            while new_tok != NODE_ITER_FINISHED {
                //Check to see if the iter result has modified more than one byte
                let node_key = self.node_key();
                if node_key.len() == 0 {
                    self.focus_iter_token = NODE_ITER_INVALID;
                    return false;
                }
                let fixed_len = node_key.len() - 1;
                if fixed_len >= key_bytes.len() || key_bytes[..fixed_len] != node_key[..fixed_len] {
                    self.regularize();
                    return false;
                }

                if key_bytes[fixed_len] > node_key[fixed_len] {
                    *self.prefix_buf.last_mut().unwrap() = key_bytes[node_key.len()-1];
                    self.focus_iter_token = new_tok;

                    //If this operation landed us at the end of the path within the node, then we
                    // should re-regularize the zipper before returning
                    if key_bytes.len() == 1 {
                        match child_node {
                            None => {},
                            Some(rec) => {
                                self.ancestors.push((*self.focus_node.clone(), new_tok, self.prefix_buf.len()));
                                *self.focus_node = rec.as_tagged();
                                self.focus_iter_token = NODE_ITER_INVALID
                            },
                        }
                    }

                    debug_assert!(self.is_regularized());
                    return true
                }

                (new_tok, key_bytes, child_node, _value) = self.focus_node.next_items(new_tok);
            }

            self.focus_iter_token = NODE_ITER_FINISHED;
            self.regularize();
            false
        }

        fn to_prev_sibling_byte(&mut self) -> bool {
            self.to_sibling(false)
        }

        fn ascend(&mut self, mut steps: usize) -> bool {
            debug_assert!(self.is_regularized());
            while steps > 0 {
                if self.excess_key_len() == 0 {
                    match self.ancestors.pop() {
                        Some((node, iter_tok, _prefix_offset)) => {
                            *self.focus_node = node;
                            self.focus_iter_token = iter_tok;
                        },
                        None => {
                            debug_assert!(self.is_regularized());
                            return false
                        }
                    };
                }
                let cur_jump = steps.min(self.excess_key_len());
                self.prefix_buf.truncate(self.prefix_buf.len() - cur_jump);
                steps -= cur_jump;
            }
            debug_assert!(self.is_regularized());
            true
        }

        fn ascend_byte(&mut self) -> bool {
            debug_assert!(self.is_regularized());
            if self.excess_key_len() == 0 {
                match self.ancestors.pop() {
                    Some((node, iter_tok, _prefix_offset)) => {
                        *self.focus_node = node;
                        self.focus_iter_token = iter_tok;
                    },
                    None => {
                        debug_assert!(self.is_regularized());
                        return false
                    }
                };
            }
            self.prefix_buf.pop();
            debug_assert!(self.is_regularized());
            true
        }

        fn ascend_until(&mut self) -> bool {
            debug_assert!(self.is_regularized());
            if self.at_root() {
                return false;
            }
            loop {
                if self.node_key().len() == 0 {
                    self.ascend_across_nodes();
                }
                self.ascend_within_node();
                if self.child_count() > 1 || self.is_val() || self.at_root() {
                    return true;
                }
            }
        }

        fn ascend_until_branch(&mut self) -> bool {
            debug_assert!(self.is_regularized());
            if self.at_root() {
                return false;
            }
            loop {
                if self.node_key().len() == 0 {
                    self.ascend_across_nodes();
                }
                self.ascend_within_node();
                if self.child_count() > 1 || self.at_root() {
                    return true;
                }
            }
        }
    }

    impl<V: Clone + Send + Sync + Unpin, A: Allocator> zipper_priv::ZipperPriv for ReadZipperCore<'_, '_, V, A> {
        type V = V;
        type A = A;
        fn get_focus(&self) -> AbstractNodeRef<'_, Self::V, Self::A> {
            let node_key = self.node_key();

            //See if we need to deregularize the zipper here to get at the ODRc that holds the focus
            let (focus_node, node_key) = if node_key.len() == 0 {
                match self.ancestors.last() {
                    Some((focus_node, _iter_tok, _prefix_offset)) => (focus_node, self.parent_key()),
                    None => {
                        return AbstractNodeRef::BorrowedRc(self.root_node.as_ref())
                    }
                }
            } else {
                (&*self.focus_node, node_key)
            };
            focus_node.get_node_at_key(node_key)
        }
        fn try_borrow_focus(&self) -> Option<&TrieNodeODRc<Self::V, Self::A>> {
            let node_key = self.node_key();
            let (focus_node, node_key) = if node_key.len() == 0 {
                let parent_key = self.parent_key();
                if parent_key.len() == 0 {
                    return Some(self.root_node.as_ref())
                }

                let parent_node = match self.ancestors.last() {
                    Some((focus_node, _iter_tok, _prefix_offset)) => {
                        *focus_node
                    },
                    None => {
                        self.root_node.as_ref().as_tagged()
                    }
                };
                (parent_node, parent_key)
            } else {
                (*self.focus_node, node_key)
            };

            match focus_node.node_get_child(node_key) {
                Some((consumed_bytes, child_node)) => {
                    debug_assert_eq!(consumed_bytes, node_key.len());
                    Some(child_node)
                },
                None => None
            }
        }
    }

    impl<'trie, V: Clone + Send + Sync + Unpin + 'trie, A: Allocator + 'trie> ZipperPathBuffer for ReadZipperCore<'trie, '_, V, A> {
        unsafe fn origin_path_assert_len(&self, len: usize) -> &[u8] {
            if self.prefix_buf.capacity() > 0 {
                assert!(len <= self.prefix_buf.capacity());
                unsafe{ core::slice::from_raw_parts(self.prefix_buf.as_ptr(), len) }
            } else {
                assert!(len <= self.origin_path.len());
                unsafe{ &self.origin_path.as_slice_unchecked() }
            }
        }
        /// Internal method to ensure buffers to facilitate movement of zipper are allocated and initialized
        #[inline(always)]
        fn prepare_buffers(&mut self) {
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
            if self.ancestors.capacity() < stack_depth {
                self.ancestors.reserve(stack_depth.saturating_sub(self.ancestors.len()));
            }
        }
    }

    impl<'trie, V: Clone + Send + Sync + Unpin + 'trie, A: Allocator + 'trie> ZipperReadOnlyConditionalValues<'trie, V> for ReadZipperCore<'trie, '_, V, A> {
        type WitnessT = ReadZipperWitness<V, A>;
        fn witness<'w>(&self) -> Self::WitnessT {
            if self.root_node.is_owned() {
                ReadZipperWitness(Some(self.root_node.as_ref().clone()))
            } else {
                ReadZipperWitness(None)
            }
        }
        fn get_val_with_witness<'w>(&self, witness: &'w Self::WitnessT) -> Option<&'w V> where 'trie: 'w {
            assert_eq!(witness.0.as_ref(), self.root_node.get_owned_ref());
            let key = self.node_key();
            if key.len() > 0 {
                self.focus_node.node_get_val(key)
            } else {
                if let Some((parent, _iter_tok, _prefix_offset)) = self.ancestors.last() {
                    parent.node_get_val(self.parent_key())
                } else {
                    if self.root_val.is_some() {
                        self.root_val
                    } else {
                        //We know the node in the witness and the node in self.root_node are the same,
                        // but we borrow it from the witness here because that has the lifetime we need
                        witness.0.as_ref().unwrap().as_tagged().node_get_val(self.root_node_key())
                    }
                }
            }
        }
    }

    impl<'a, V: Clone + Send + Sync + Unpin, A: Allocator + 'a> ZipperReadOnlyPriv<'a, V, A> for ReadZipperCore<'a, '_, V, A> {
        fn borrow_raw_parts(&self) -> (TaggedNodeRef<'_, V, A>, &[u8], Option<&V>) {
            let node_key = self.node_key();
            if node_key.len() > 0 {
                (*self.focus_node, node_key, None)
            } else {
                (*self.focus_node, &[], self.val())
            }
        }
        fn take_core(&mut self) -> Option<ReadZipperCore<'a, 'static, V, A>> {
            unreachable!()
        }
    }

    impl<V: Clone + Send + Sync + Unpin, A: Allocator> ZipperConcrete for ReadZipperCore<'_, '_, V, A> {
        #[inline]
        fn shared_node_id(&self) -> Option<u64> {
            read_zipper_shared_node_id(self)
        }
        #[inline]
        fn is_shared(&self) -> bool {
            let key = self.node_key();
            if key.len() > 0 {
                false
            } else {
                if let Some((parent, _iter_tok, _prefix_offset)) = self.ancestors.last() {
                    let (_key_len, focus_node) = parent.node_get_child(self.parent_key()).unwrap();
                    focus_node.refcount() > 1
                } else {
                    false //root
                }
            }
        }
    }

    #[inline]
    pub(crate) fn read_zipper_shared_node_id<'a, V: Clone + Send + Sync + 'a, A: Allocator + 'a, Z: Zipper + ZipperReadOnlyPriv<'a, V, A> + ZipperConcrete>(zipper: &Z) -> Option<u64> {
        let (node, key, value) = zipper.borrow_raw_parts();
        if !zipper.is_shared() || !key.is_empty() || value.is_some() {
            // TODO(igorm): Currently values associated with a nodes that can be shared
            // are stored outside of the node. This means one focus address can
            // correspond to two different points which have different values.
            // Therefore, we can't cache nodes that have values themselves.
            // Relevant discussion:
            // https://github.com/Adam-Vandervorst/PathMap/pull/8#discussion_r2005555762
            // https://github.com/Adam-Vandervorst/PathMap/blob/cleanup_to_release/pathmap-book/src/A.0001_map_root_values.md
            // https://discord.com/channels/@me/1215835387432271922/1352463443541754068
            return None
        }
        Some(node.shared_node_id())
    }

    //GOAT.  Need to add `to_first_val` method that moves the zipper to the root, and if the root contains a
    // value, returns it, and otherwise calls to_next_val().
    //
    //Then I need to port all the iter() conveniences over to use that new method

    impl<'trie, V: Clone + Send + Sync + Unpin + 'trie, A: Allocator + 'trie> ZipperIteration for ReadZipperCore<'trie, '_, V, A> {
        fn to_next_val(&mut self) -> bool {
            unsafe{ self.to_next_get_val() }.is_some()
        }
        fn descend_first_k_path(&mut self, k: usize) -> bool {
            self.prepare_buffers();
            debug_assert!(self.is_regularized());

            let cur_tok = self.focus_node.iter_token_for_path(self.node_key());
            self.focus_iter_token = cur_tok;

            self.k_path_internal(k, self.prefix_buf.len())
        }
        fn to_next_k_path(&mut self, k: usize) -> bool {
            let base_idx = if self.path_len() >= k {
                self.prefix_buf.len() - k
            } else {
                self.origin_path.len()
            };
            //De-regularize the zipper
            debug_assert!(self.is_regularized());
            self.deregularize();
            self.k_path_internal(k, base_idx)
        }
    }

    impl<'trie, V: Clone + Send + Sync + Unpin + 'trie, A: Allocator + 'trie> ZipperAbsolutePath for ReadZipperCore<'trie, '_, V, A> {
        fn origin_path(&self) -> &[u8] {
            if self.prefix_buf.capacity() > 0 {
                &self.prefix_buf
            } else {
                unsafe{ &self.origin_path.as_slice_unchecked() }
            }
        }
        fn root_prefix_path(&self) -> &[u8] {
            if self.prefix_buf.capacity() > 0 {
                &self.prefix_buf[..self.origin_path.len()]
            } else {
                unsafe{ &self.origin_path.as_slice_unchecked() }
            }
        }
    }

    impl<'a, 'path, V: Clone + Send + Sync + Unpin + 'a, A: Allocator + 'a> ReadZipperCore<'a, 'path, V, A> {

        /// Creates a new zipper, with a path relative to a node
        ///
        /// `root_key_start` is the offset in `path` that aligns with the `root_node` that is passed in
        /// It is used to pre-initialize an origin_path / root_prefix_path.
        ///
        /// `root_prefix_len` is the offset in `path` from which the zipper's root begins.
        ///
        /// `path.len() >= root_prefix_len >= root_key_start` or this method will panic.
        /// 
        /// ```ignoretext
        /// (dots '.' are node separators in this example path)
        ///
        ///                  ancestors[0]  ancestors[1]    focus_node
        ///                       v             v              v
        /// prefix_buf = "this-is.a-path-to-the.current-zipper.focus"
        ///                       ^         ^
        ///                       |   root_prefix_len
        ///                 root_key_start
        /// ```ignore
        pub(crate) fn new_with_node_and_path_in(root_node: &'a TrieNodeODRc<V, A>, owned_root: bool, path: &'path [u8], root_prefix_len: usize, root_key_start: usize, root_val: Option<&'a V>, alloc: A) -> Self {
            let (node, key, val) = node_along_path(root_node, &path[root_key_start..], root_val, false);
            let node = if owned_root {
                OwnedOrBorrowed::Owned(node.clone())
            } else {
                OwnedOrBorrowed::Borrowed(node)
            };
            let new_root_key_start = root_prefix_len - key.len();
            Self::new_with_node_and_path_internal_in(node, path, new_root_key_start, val, alloc)
        }
        /// Creates a new zipper, with a path relative to a node, assuming the path is fully-contained within
        /// the node
        ///
        /// NOTE: This method currently doesn't descend subnodes.  Use [Self::new_with_node_and_path_in] if you can't
        /// guarantee the path is within the supplied node.
        pub(crate) fn new_with_node_and_path_internal_in(root_node: OwnedOrBorrowed<'a, TrieNodeODRc<V, A>>, path: &'path [u8], mut root_key_start: usize, root_val: Option<&'a V>, alloc: A) -> Self {
            let mut focus: TaggedNodeRef<'a, V, A> = match &root_node {
                OwnedOrBorrowed::Owned(root_node) => {
                    // SAFETY: The root_node makes the ReadZipper essentially a self-referential type.  As long
                    // as the ReadZipperCore is alive, this will remain valid, and the "witness" mechanism in
                    // `ZipperReadOnlyConditionalValues` makes sure the references returned from the `get_val`
                    // method remain valid
                    unsafe{ core::mem::transmute(root_node.as_tagged()) }
                },
                OwnedOrBorrowed::Borrowed(root_node) => {
                    root_node.as_tagged()
                },
                OwnedOrBorrowed::None => {
                    TaggedNodeRef::empty_node()
                }
            };

            //Finish regularizing the zipper, if we stopped short when traversing to the focus node
            let mut root_parent_key_start = usize::MAX;
            if path.len() > root_key_start {
                if let Some((consumed_byte_cnt, next_node)) = focus.node_get_child(&path[root_key_start..]) {
                    focus = next_node.as_tagged();
                    root_parent_key_start = root_key_start;
                    root_key_start += consumed_byte_cnt;
                }
            }
            Self {
                origin_path: SliceOrLen::from(path),
                root_key_start,
                root_parent_key_start,
                root_val: root_val.map(|v| v.into()),
                focus_node: MiriWrapper::new(focus),
                root_node,
                focus_iter_token: NODE_ITER_INVALID,
                prefix_buf: vec![],
                ancestors: vec![],
                alloc,
            }
        }
        /// Same as [Self::new_with_node_and_path_in], but inits the zipper stack ahead of time, allowing a zipper
        /// that isn't bound by `'path`
        pub(crate) fn new_with_node_and_cloned_path_in(root_node: &'a TrieNodeODRc<V, A>, owned_root: bool, path: &[u8], root_prefix_len: usize, root_key_start: usize, root_val: Option<&'a V>, alloc: A) -> Self {
            let (node, key, val) = node_along_path(root_node, &path[root_key_start..], root_val, false);
            let node = if owned_root {
                OwnedOrBorrowed::Owned(node.clone())
            } else {
                OwnedOrBorrowed::Borrowed(node)
            };
            let new_root_key_start = root_prefix_len - key.len();
            let mut new_zipper = ReadZipperCore::<'a, '_, V, A>::new_with_node_and_path_internal_in(node, path, new_root_key_start, val, alloc);

            new_zipper.prefix_buf = Vec::with_capacity(EXPECTED_PATH_LEN);
            new_zipper.prefix_buf.extend(path);
            new_zipper.origin_path = SliceOrLen::new_owned(path.len());
            new_zipper.ancestors = Vec::with_capacity(EXPECTED_DEPTH);

            new_zipper.make_static_path()
        }

        /// Makes a version of `self` that has an allocated path buffer and a `'static`` path lifetime
        #[inline]
        pub(crate) fn make_static_path(mut self) -> ReadZipperCore<'a, 'static, V, A> {
            self.prepare_buffers();
            ReadZipperCore {
                origin_path: SliceOrLen::new_owned(self.origin_path.len()),
                root_key_start: self.root_key_start,
                root_parent_key_start: self.root_parent_key_start,
                root_val: self.root_val,
                root_node: self.root_node,
                focus_node: self.focus_node,
                focus_iter_token: NODE_ITER_INVALID,
                prefix_buf: self.prefix_buf,
                ancestors: self.ancestors,
                alloc: self.alloc
            }
        }

        /// Returns the length of the `self.path()`, saving a couple instructions, but is internal because it may panic
        #[inline(always)]
        fn path_len(&self) -> usize {
            self.prefix_buf.len() - self.origin_path.len()
        }

        /// Ensures the zipper is in its regularized form
        ///
        /// Q: What the heck is "regularized form"?!?!?!
        /// A: The same zipper position may be representated with multiple configurations of the zipper's
        ///  field variables.  Consider the path: `abcd`, where the zipper points to `c`.  This could be
        ///  represented with the `focus_node` of `c` and a `node_key()` of `[]`; called the zipper's
        ///  regularized form.  Alternatively it could be represented with the `focus_node` of `b` and a
        ///  `node_key()` of `c`, which is called a deregularized form.
        #[inline]
        pub(crate) fn regularize(&mut self) {
            debug_assert!(self.prefix_buf.len() >= self.node_key_start()); //If this triggers, we have uninitialized buffers
            if let Some((_consumed_byte_cnt, next_node)) = self.focus_node.node_get_child(self.node_key()) {
                self.ancestors.push((*self.focus_node.clone(), self.focus_iter_token, self.prefix_buf.len()));
                *self.focus_node = next_node.as_tagged();
                self.focus_iter_token = NODE_ITER_INVALID;
            }
        }

        /// Ensures the zipper is in a deregularized form
        ///
        /// While there are dozens of deregularized forms of the zipper and only one regularized, this
        /// method puts the zipper in the state where the focus_node will be as close to the focus as
        /// possible while also ensuring `node_key().len() > 0` or the zipper is at the root.
        #[inline]
        pub(crate) fn deregularize(&mut self) {
            if self.prefix_buf.len() == self.node_key_start() {
                self.ascend_across_nodes();
            }
        }

        /// Returns `true` if the zipper is in a regularized form, otherwise returns the `false`
        ///
        /// See docs for [Self::regularize].
        #[inline]
        fn is_regularized(&self) -> bool {
            let key_start = self.node_key_start();
            if self.prefix_buf.len() > key_start {
                self.focus_node.node_get_child(self.node_key()).is_none()
            } else {
                true
            }
        }

        /// Internal impl is marked `unsafe` because ReadZipperCore::root_node might be `Owned`, meaning
        /// the returned value might outlive the zipper.  We need to only expose this through methods
        /// that have tighter lifetime bounds, or on types that guarantee the root won't be owned
        pub(crate) unsafe fn get_val(&self) -> Option<&'a V> {
            let key = self.node_key();
            if key.len() > 0 {
                self.focus_node.node_get_val(key)
            } else {
                if let Some((parent, _iter_tok, _prefix_offset)) = self.ancestors.last() {
                    parent.node_get_val(self.parent_key())
                } else {
                    //NOTE: It's true that we shouldn't have ZipperReadOnlyValues implemented on a type that comes from a ZipperHead, but
                    // we currently share the same implementation between `val()` and `get_val()` because the only difference is the return
                    // lifetime, and the current ZipperHead implementation is actually ok with referencing the value in the root of the ZipperHead.
                    // debug_assert!(self.root_node.is_borrowed());
                    self.root_val
                }
            }
        }

        /// See [ReadZipperCore::get_val] for explanation as to why this is unsafe
        pub(crate) unsafe fn to_next_get_val(&mut self) -> Option<&'a V> {
            self.prepare_buffers();
            loop {
                if self.focus_iter_token == NODE_ITER_INVALID {
                    let cur_tok = self.focus_node.iter_token_for_path(self.node_key());
                    self.focus_iter_token = cur_tok;
                }

                let (new_tok, key_bytes, child_node, value) = if self.focus_iter_token != NODE_ITER_FINISHED {
                    self.focus_node.next_items(self.focus_iter_token)
                } else {
                    (NODE_ITER_FINISHED, &[][..] as &[u8], None, None)
                };

                if new_tok != NODE_ITER_FINISHED {
                    self.focus_iter_token = new_tok;

                    let key_start = self.node_key_start();

                    //Make sure we don't move to a branch that forks above our zipper root
                    let origin_path_len = self.origin_path.len();
                    if key_start < origin_path_len {
                        debug_assert_eq!(self.ancestors.len(), 0);

                        let unmodifiable_len = origin_path_len - key_start;
                        let unmodifiable_subkey = &self.prefix_buf[key_start..origin_path_len];
                        if unmodifiable_len > key_bytes.len() || &key_bytes[..unmodifiable_len] != unmodifiable_subkey {
                            self.prefix_buf.truncate(origin_path_len);
                            return None
                        }
                    }

                    self.prefix_buf.truncate(key_start);
                    self.prefix_buf.extend(key_bytes);

                    match child_node {
                        None => {},
                        Some(rec) => {
                            self.ancestors.push((*self.focus_node.clone(), new_tok, self.prefix_buf.len()));
                            *self.focus_node = rec.as_tagged();
                            self.focus_iter_token = self.focus_node.new_iter_token();
                        },
                    }

                    match value {
                        Some(v) => return Some(v),
                        None => {}
                    }
                } else {
                    //Ascend
                    if let Some((focus_node, iter_tok, prefix_offset)) = self.ancestors.pop() {
                        *self.focus_node = focus_node;
                        self.focus_iter_token = iter_tok;
                        self.prefix_buf.truncate(prefix_offset);
                    } else {
                        let new_len = self.origin_path.len();
                        self.focus_iter_token = NODE_ITER_INVALID;
                        self.prefix_buf.truncate(new_len);
                        return None
                    }
                }
            }
        }

        /// Returns the parent and path from which the top of the focus_stack can be re-acquired
        #[inline]
        pub(crate) fn focus_parent(&self) -> &TrieNodeODRc<V, A> {
            let parent_key = self.parent_key();
            if parent_key.len() == 0 {
                return self.root_node.as_ref()
            }
            self.focus_parent_borrowed()
        }

        /// Same behavior as [Self::focus_parent], but with a less restrictive return lifetime
        #[inline]
        pub(crate) fn focus_parent_borrowed(&self) -> &'a TrieNodeODRc<V, A> {
            let parent_key = self.parent_key();
            if parent_key.len() == 0 {
                return self.root_node.as_borrowed_ref()
            }

            let parent_node = match self.ancestors.last() {
                Some((focus_node, _iter_tok, _prefix_offset)) => {
                    *focus_node
                },
                None => unreachable!()
            };
            let (key_len, node) = parent_node.node_get_child(parent_key).unwrap();
            debug_assert_eq!(key_len, parent_key.len());
            node
        }

        /// Internal method to implement `descend_to` and similar methods, handling the movement
        /// of the focus node, but not necessarily the whole method contract
        ///
        /// Returns the remaining `node_key`, after the node descent has gone as far as possible,
        /// along with a re-borrow of `self` to work around the borrow checker
        #[inline]
        fn descend_to_internal(&mut self, k: &[u8]) -> (&Self, &[u8]) {
            self.focus_iter_token = NODE_ITER_INVALID;
            self.prefix_buf.extend(k);
            let mut key_start = self.node_key_start();
            let mut key = &self.prefix_buf[key_start..];

            //GOAT... WIP.  planning to add a "CheckF: Fn(&dyn TrieNode<V>, &[u8])->Option<usize>"
            // argument that can cause an early return, and be used to look for values as we descend
            //
            // //Run the check_f on the current focus node, before advancing to the next node
            // match check_f(self.focus_node.borrow(), &self.prefix_buf[key_start..]) {
            //     Some(byte_cnt) => {
            //         return (self, &self.prefix_buf[key_start..byte_cnt])
            //     },
            //     None => {}
            // }

            //Step until we get to the end of the key or find a leaf node
            while let Some((consumed_byte_cnt, next_node)) = self.focus_node.node_get_child(key) {
                let next_node = next_node.as_tagged();
                key_start += consumed_byte_cnt;
                self.ancestors.push((*self.focus_node.clone(), NODE_ITER_INVALID, key_start));
                *self.focus_node = next_node;
                if consumed_byte_cnt < key.len() {
                    key = &key[consumed_byte_cnt..]
                } else {
                    return (self, &[]);
                };
            }
            (self, key)
        }

        /// Internal implementation of `to_next_sibling_byte` / `to_prev_sibling_byte`, which
        /// performs about as well as the `to_next_sibling_byte` that is there, but doesn't
        /// update the zipper's iter tokens
        #[inline]
        fn to_sibling(&mut self, next: bool) -> bool {
            self.prepare_buffers();
            debug_assert!(self.is_regularized());
            if self.node_key().len() != 0 {
                match self.focus_node.get_sibling_of_child(self.node_key(), next) {
                    (Some(prefix), Some(child_node)) => {
                        *self.prefix_buf.last_mut().unwrap() = prefix;
                        self.ancestors.push((*self.focus_node.clone(), self.focus_iter_token, self.prefix_buf.len()));
                        *self.focus_node = child_node;
                        self.focus_iter_token = NODE_ITER_INVALID;
                        true
                    },
                    (Some(prefix), None) => {
                        *self.prefix_buf.last_mut().unwrap() = prefix;
                        true
                    },
                    (None, _) => false
                }
            } else {
                let mut should_pop = false;
                let result = match self.ancestors.last() {
                    None => { false }
                    Some((parent, _iter_tok, _prefix_offset)) => {
                        match parent.get_sibling_of_child(self.parent_key(), next) {
                            (Some(prefix), Some(child_node)) => {
                                *self.prefix_buf.last_mut().unwrap() = prefix;
                                *self.focus_node = child_node;
                                self.focus_iter_token = NODE_ITER_INVALID;
                                true
                            },
                            (Some(prefix), None) => {
                                *self.prefix_buf.last_mut().unwrap() = prefix;
                                should_pop = true;
                                true
                            },
                            (None, _) => {
                                false
                            }
                        }
                    }
                };
                if should_pop {
                    let (focus_node, iter_tok, _prefix_offset) = self.ancestors.pop().unwrap();
                    *self.focus_node = focus_node;
                    self.focus_iter_token = iter_tok;
                }
                result
            }
        }

        /// Internal method that implements both `k_path...` methods above
        #[inline]
        fn k_path_internal(&mut self, k: usize, base_idx: usize) -> bool {
            loop {
                //If either of these trip, the caller is probably misusing the API and likely didn't call
                // `descend_first_k_path` before calling `to_next_k_path`
                debug_assert!(self.prefix_buf.len() <= base_idx+k);
                debug_assert!(self.prefix_buf.len() >= base_idx);

                //Check to see if we need to reset the iter_token in the middle of the iteration.
                // This shouldn't happen unless some other zipper methods invalidated the k_path iteration state,
                // but that can happen and we should try our best to resume the iteration where we left it.
                if self.focus_iter_token == NODE_ITER_INVALID {
                    self.focus_iter_token = self.focus_node.iter_token_for_path(self.node_key());
                    let (new_tok, key_bytes, _child_node, _value) = self.focus_node.next_items(self.focus_iter_token);
                    let node_key = self.node_key();
                    if key_bytes.len() >= node_key.len() {
                        if &key_bytes[..node_key.len()] == node_key {
                            self.focus_iter_token = new_tok;
                        }
                    }
                }

                if self.focus_iter_token == NODE_ITER_FINISHED {
                    //This branch means we need to ascend or we're finished with the iteration and will
                    // return a result at `path_len == base_idx`

                    //Have we reached the root of this k_path iteration?
                    if self.node_key_start() <= base_idx  {
                        self.focus_iter_token = NODE_ITER_FINISHED;
                        self.prefix_buf.truncate(base_idx);
                        return false
                    }

                    if let Some((focus_node, iter_tok, prefix_offset)) = self.ancestors.pop() {
                        *self.focus_node = focus_node;
                        self.focus_iter_token = iter_tok;
                        self.prefix_buf.truncate(prefix_offset);
                    } else {
                        let new_len = self.origin_path.len();
                        self.focus_iter_token = NODE_ITER_INVALID;
                        self.prefix_buf.truncate(new_len);
                        return false
                    }
                }

                //Move the zipper to the next sibling position, if we can
                let (new_tok, key_bytes, child_node, _value) = self.focus_node.next_items(self.focus_iter_token);

                if new_tok != NODE_ITER_FINISHED {

                    //Check to see if the iteration has modified more characters than allowed by `k`
                    let key_start = self.node_key_start();
                    if key_start < base_idx {
                        let base_key_len = base_idx - key_start; //The number of bytes we should not modify
                        if base_key_len > key_bytes.len() || &key_bytes[..base_key_len] != &self.prefix_buf[key_start..base_idx] {
                            self.prefix_buf.truncate(base_idx);
                            return false;
                        }
                    }

                    self.focus_iter_token = new_tok;

                    //If we got here, it means we're either going to continue to descend, or return a
                    // result at `path_len == base_idx+k`
                    let key_start = self.node_key_start();
                    self.prefix_buf.truncate(key_start);
                    self.prefix_buf.extend(key_bytes);

                    if self.prefix_buf.len() <= k+base_idx {
                        match child_node {
                            None => {},
                            Some(rec) => {
                                self.ancestors.push((*self.focus_node.clone(), new_tok, self.prefix_buf.len()));
                                *self.focus_node = rec.as_tagged();
                                self.focus_iter_token = self.focus_node.new_iter_token();
                            },
                        }
                    } else {
                        self.prefix_buf.truncate(k+base_idx);
                    }

                    //See if we have a result to return
                    if self.prefix_buf.len() == k+base_idx {
                        return true;
                    }
                } else {
                    self.focus_iter_token = NODE_ITER_FINISHED;
                }
            }
        }

        // //GOAT, ALTERNATIVE IMPLEMENTATION.  Performance is roughly equal between the two, but the other
        // //   implementation was chosen because it initializes the iter_token in preparation for subsequent iteration
        // pub fn descend_first_byte(&mut self) -> bool {
        //     self.prepare_buffers();
        //     debug_assert!(self.is_regularized());
        //     match self.focus_node.first_child_from_key(self.node_key()) {
        //         (Some(prefix), Some(child_node)) => {
        //             match prefix.len() {
        //                 0 => {
        //                     panic!(); //GOAT, I don't think we will hit this
        //                     //If we're at the root of the new node, descend to the first child
        //                     self.descend_first_byte()
        //                 },
        //                 1 => {
        //                     //Step to a new node
        //                     self.prefix_buf.push(prefix[0]);
        //                     self.ancestors.push((self.focus_node.clone(), self.focus_iter_token, self.prefix_buf.len()));
        //                     self.focus_iter_token = self.focus_node.new_iter_token();
        //                     self.focus_node = child_node.as_tagged();
        //                     true
        //                 },
        //                 _ => {
        //                     //Stay within the same node, and just grow the path
        //                     self.prefix_buf.push(prefix[0]);
        //                     true
        //                 }
        //             }
        //         },
        //         (Some(prefix), None) => {
        //             //Stay within the same node
        //             self.prefix_buf.push(prefix[0]);
        //             true
        //         },
        //         (None, _) => false
        //     }
        // }

        /// Internal method that implements [Self::is_val], but so it can be inlined elsewhere
        #[inline]
        fn is_val_internal(&self) -> bool {
            let key = self.node_key();
            if key.len() > 0 {
                self.focus_node.node_contains_val(key)
            } else {
                if let Some((parent, _iter_tok, _prefix_offset)) = self.ancestors.last() {
                    parent.node_contains_val(self.parent_key())
                } else {
                    self.root_val.is_some()
                }
            }
        }

        /// Internal method implementing part of [Self::descend_until], but doesn't pay attention to to [Self::child_count]
        #[inline]
        fn descend_first(&mut self) {
            self.prepare_buffers();
            match self.focus_node.first_child_from_key(self.node_key()) {
                (Some(prefix), Some(child_node)) => {
                    //Step to a new node
                    self.prefix_buf.extend(prefix);
                    self.ancestors.push((*self.focus_node.clone(), self.focus_iter_token, self.prefix_buf.len()));
                    *self.focus_node = child_node;
                    self.focus_iter_token = NODE_ITER_INVALID;

                    //If we're at the root of the new node, descend to the first child
                    if prefix.len() == 0 {
                        self.descend_first()
                    }
                },
                (Some(prefix), None) => {
                    //Stay within the same node
                    self.prefix_buf.extend(prefix);
                },
                (None, _) => unreachable!()
            }
        }

        /// Internal method returning the index to the key char beyond the path to the `self.focus_node`
        #[inline]
        fn node_key_start(&self) -> usize {
            self.ancestors.last().map(|(_node, _iter_tok, i)| *i)
                .unwrap_or_else(|| self.root_key_start)
        }
        //GOAT, probably don't need this
        // /// Returns the offset of the end of the focus node's key
        // #[inline]
        // fn node_key_end(&self) -> usize {
        //     if self.prefix_buf.len() > 0 {
        //         self.prefix_buf.len()
        //     } else {
        //         self.origin_path.len()
        //     }
        // }
        /// Internal method returning the key within the focus node
        #[inline]
        pub(crate) fn node_key(&self) -> &[u8] {
            let key_start = self.node_key_start();
            if self.prefix_buf.len() > 0 {
                &self.prefix_buf[key_start..]
            } else {
                if self.origin_path.len() > 0 {
                    unsafe{ &self.origin_path.as_slice_unchecked()[key_start..] }
                } else {
                    &[]
                }
            }
        }
        /// Internal method returning the key that leads to the zipper root within the zipper's root_node
        #[inline]
        fn root_node_key(&self) -> &[u8] {
            //This method should only be called when we have an owned `root_node`, which should go with
            // a valid value for `root_parent_key_start`
            debug_assert!(matches!(self.root_node, OwnedOrBorrowed::Owned(_)));
            debug_assert!(self.root_parent_key_start < usize::MAX);
            if self.prefix_buf.capacity() == 0 && self.origin_path.len() > 0 {
                unsafe{ &self.origin_path.as_slice_unchecked()[self.root_parent_key_start..] }
            } else {
                &self.prefix_buf[self.root_parent_key_start..self.root_key_start]
            }
        }
        /// Internal method returning the key that leads to `self.focus_node` within the parent
        #[inline]
        fn parent_key(&self) -> &[u8] {
            if self.prefix_buf.len() > 0 {
                let key_start = if self.ancestors.len() > 1 {
                    unsafe{ self.ancestors.get_unchecked(self.ancestors.len()-2) }.2
                } else {
                    self.root_key_start
                };
                &self.prefix_buf[key_start..self.node_key_start()]
            } else {
                if self.root_parent_key_start == usize::MAX || self.origin_path.len() == 0 {
                    &[]
                } else {
                    let origin_path = unsafe{ self.origin_path.as_slice_unchecked() };
                    &origin_path[self.root_parent_key_start..self.root_key_start]
                }
            }
        }
        /// Internal method similar to `self.node_key().len()`, but returns the number of chars that can be
        /// legally ascended within the node, taking into account the root_key
        #[inline]
        fn excess_key_len(&self) -> usize {
            self.prefix_buf.len() - self.ancestors.last().map(|(_node, _iter_tok, i)| *i).unwrap_or(self.origin_path.len())
        }
        /// Internal method which doesn't actually move the zipper, but ensures `self.node_key().len() > 0`
        /// WARNING, must never be called if `self.node_key().len() != 0`
        #[inline]
        fn ascend_across_nodes(&mut self) {
            debug_assert!(self.node_key().len() == 0);
            if let Some((focus_node, iter_tok, _prefix_offset)) = self.ancestors.pop() {
                *self.focus_node = focus_node;
                self.focus_iter_token = iter_tok;
            } else {
                self.focus_iter_token = NODE_ITER_INVALID;
            }
        }
        /// Internal method used to impement `ascend_until` when ascending within a node
        #[inline]
        fn ascend_within_node(&mut self) {
            let branch_key = self.focus_node.prior_branch_key(self.node_key());
            let new_len = self.origin_path.len().max(self.node_key_start() + branch_key.len());
            self.prefix_buf.truncate(new_len);
        }
        /// Push a new node-path pair onto the zipper.  This is used in the internal implementation of
        /// the [super::zipper::ProductZipper]
        pub(crate) fn push_node(&mut self, node: TaggedNodeRef<'a, V, A>) {
            self.ancestors.push((*self.focus_node.clone(), self.focus_iter_token, self.prefix_buf.len()));
            *self.focus_node = node;
            self.focus_iter_token = NODE_ITER_INVALID;
        }

        pub(crate) fn into_path(self) -> Vec<u8> {
            self.prefix_buf
        }
    }

    /// Validate we don't accidentially reallocate the path buffer when we don't need to
    #[test]
    fn read_zipper_reserve_buffer_test() {
        let map = PathMap::<()>::new();

        //Try with no prefix path
        let mut rz = map.read_zipper();
        assert_eq!(rz.z.prefix_buf.capacity(), 0);
        rz.reserve_buffers(4096, 512);
        assert_eq!(rz.z.prefix_buf.capacity(), 4096);
        let old_ptr = rz.z.prefix_buf.as_ptr();
        rz.descend_to(b"hello");
        assert_eq!(rz.z.prefix_buf.capacity(), 4096);
        assert_eq!(rz.z.prefix_buf.as_ptr(), old_ptr);
        assert_eq!(&rz.z.prefix_buf, b"hello");
        assert_eq!(&rz.path(), b"hello");

        //Try with a prefix path
        let mut rz = map.read_zipper_at_borrowed_path(b"hi");
        assert_eq!(rz.z.prefix_buf.capacity(), 0);
        assert_eq!(rz.path(), b"");
        assert_eq!(rz.origin_path(), b"hi");
        rz.reserve_buffers(4096, 512);
        assert_eq!(rz.z.prefix_buf.capacity(), 4096);
        let old_ptr = rz.z.prefix_buf.as_ptr();
        assert_eq!(rz.path(), b"");
        assert_eq!(rz.origin_path(), b"hi");
        assert_eq!(&rz.z.prefix_buf, b"hi");
        rz.descend_to(b"-howdy");
        assert_eq!(rz.z.prefix_buf.capacity(), 4096);
        assert_eq!(rz.z.prefix_buf.as_ptr(), old_ptr);
        assert_eq!(&rz.z.prefix_buf, b"hi-howdy");
        assert_eq!(&rz.path(), b"-howdy");
        assert_eq!(rz.origin_path(), b"hi-howdy");
    }
}
use read_zipper_core::*;

/// Internal function to walk along a path to the final node reference
pub(crate) fn node_along_path<'a, 'path, V: Clone + Sync + Send, A: Allocator + 'a>(root_node: &'a TrieNodeODRc<V, A>, path: &'path [u8], root_val: Option<&'a V>, stop_short: bool) -> (&'a TrieNodeODRc<V, A>, &'path [u8], Option<&'a V>) {
    let mut key = path;
    let mut node = root_node;
    let mut val = root_val;
    let mut tagged_node = node.as_tagged();

    //Step until we get to the end of the key or find a leaf node
    if key.len() > 0 {
        while let Some((consumed_byte_cnt, next_node)) = tagged_node.node_get_child(key) {
            if consumed_byte_cnt < key.len() {
                node = next_node;
                key = &key[consumed_byte_cnt..];
            } else {
                if !stop_short {
                    val = tagged_node.node_get_val(key);
                    node = next_node;
                    key = &[];
                } else {
                    val = None;
                }
                break;
            };
            tagged_node = node.as_tagged();
        }
    }

    (node, key, val)
}

/// An iterator for depth-first traversal of a [Zipper], returned from [ReadZipperUntracked::into_iter]
///
/// NOTE: This is a convenience to allow access to syntactic sugar like `for` loops, [collect](std::iter::Iterator::collect),
///  etc.  It will always be faster to use the zipper itself for iteration and traversal.
pub struct ReadZipperIter<'a, 'path, V: Clone + Send + Sync, A: Allocator = GlobalAlloc>{
    started: bool,
    zipper: Option<ReadZipperCore<'a, 'path, V, A>>,
}

impl<'a, V: Clone + Send + Sync + Unpin + 'a, A: Allocator + 'a> Iterator for ReadZipperIter<'a, '_, V, A> {
    type Item = (Vec<u8>, &'a V);

    fn next(&mut self) -> Option<(Vec<u8>, &'a V)> {
        if !self.started {
            self.started = true;
            if let Some(zipper) = &mut self.zipper {
                //SAFETY: we only allow ReadZipperUntracked to become a `ReadZipperIter`
                if let Some(val) = unsafe{ zipper.get_val() } {
                    return Some((zipper.path().to_vec(), val))
                }
            }
        }
        if let Some(zipper) = &mut self.zipper {
            //SAFETY: we only allow ReadZipperUntracked to become a `ReadZipperIter`
            match unsafe{ zipper.to_next_get_val() } {
                Some(val) => return Some((zipper.path().to_vec(), val)),
                None => self.zipper = None
            }
        }
        None
    }
}

/// An iterator for depth-first traversal of every existing path end (leaf) reachable from a [Zipper],
/// regardless of whether a value exists at that path. Returned from [ReadZipperUntracked::into_path_iter]
///
/// NOTE: This is a convenience to allow access to syntactic sugar like `for` loops, [collect](std::iter::Iterator::collect),
///  etc.  It will always be faster to use the zipper itself for iteration and traversal.
pub struct ReadZipperPathIter<'a, 'path, V: Clone + Send + Sync, A: Allocator = GlobalAlloc>{
    zipper: Option<ReadZipperCore<'a, 'path, V, A>>,
}

impl<V: Clone + Send + Sync + Unpin, A: Allocator> ReadZipperPathIter<'_, '_, V, A> {
    /// Returns a reference to the value at the last-returned path 
    pub fn val(&self) -> Option<&V> {
        self.zipper.as_ref().and_then(|z| z.val())
    }
}

impl<'a, V: Clone + Send + Sync + Unpin + 'a, A: Allocator + 'a> Iterator for ReadZipperPathIter<'a, '_, V, A> {
    type Item = Vec<u8>;

    fn next(&mut self) -> Option<Vec<u8>> {
        if let Some(zipper) = &mut self.zipper {
            while zipper.to_next_step() {
                if zipper.path_exists() && zipper.child_count() == 0 {
                    return Some(zipper.path().to_vec());
                }
            }
        }
        self.zipper = None;
        None
    }
}

/// The origin path, will be a slice if it's borrowed from outside the Zipper, or length of the origin path in
/// the `prefix_buf` if it has already been copied
#[derive(Clone, Copy)]
pub(crate) enum SliceOrLen<'a> {
    Slice(&'a [u8]),
    Len(usize),
}

impl<'a> From<&'a [u8]> for SliceOrLen<'a> {
    fn from(slice: &'a [u8]) -> Self {
        Self::Slice(slice)
    }
}

impl SliceOrLen<'static> {
    #[inline]
    pub fn new_owned(len: usize) -> Self {
        if len == 0 {
            Self::Slice(&[])
        } else {
            Self::Len(len)
        }
    }
}

#[allow(unused)]
impl<'a> SliceOrLen<'a> {
    #[inline]
    pub fn len(&self) -> usize {
        match self {
            Self::Slice(slice) => slice.len(),
            Self::Len(len) => {
                debug_assert!(*len > 0); //Zero-length SliceOrLen should always be represented as a `Slice`
                *len
            },
        }
    }
    pub fn make_len(&mut self) {
        if self.len() > 0 {
            match self {
                Self::Slice(slice) => {*self = Self::Len(slice.len())},
                Self::Len(_) => {},
            }
        }
    }
    #[inline]
    pub fn is_slice(&self) -> bool {
        match self {
            Self::Slice(_) => true,
            Self::Len(_) => false,
        }
    }
    #[inline]
    pub fn as_slice(&self) -> &'a[u8] {
        match self {
            Self::Slice(slice) => slice,
            Self::Len(_) => unreachable!()
        }
    }
    #[inline]
    pub fn try_as_slice(&self) -> Option<&'a[u8]> {
        match self {
            Self::Slice(slice) => Some(slice),
            Self::Len(_) => None
        }
    }
    #[inline]
    pub unsafe fn as_slice_unchecked(&self) -> &'a[u8] {
        match self {
            Self::Slice(slice) => slice,
            Self::Len(_) => unsafe{ core::hint::unreachable_unchecked() }
        }
    }
    #[inline]
    pub fn set_slice(&mut self, slice: &'a[u8]) {
        *self = Self::Slice(slice);
    }
    #[inline]
    pub fn set_len(&mut self, len: usize) {
        if len > 0 {
            *self = Self::Len(len)
        } else {
            *self = Self::Slice(&[])
        }
    }
}

/// Implements tests that apply to all [ZipperMoving] types
#[cfg(all(test, feature = "pathmap-internal-tests"))]
pub(crate) mod zipper_moving_tests {
    use super::trie_map::*;
    use super::*;

    /// `$ident` is a unique identifier for the zipper, so the generated tests don't collide
    /// `$read_keys` is a function that will create a store containing all paths, from which a zipper can be created
    /// `$make_z` is a function that will create a zipper from a slice of paths
    macro_rules! zipper_moving_tests {
        ($z_name:ident, $read_keys:expr, $make_z:expr)=>{
            paste::paste! {
                #[test]
                fn [<$z_name _zipper_moving_basic_test>]() {
                    let mut temp_store = $read_keys(super::zipper::zipper_moving_tests::ZIPPER_MOVING_BASIC_TEST_KEYS);
                    super::zipper::zipper_moving_tests::run_test(&mut temp_store, $make_z, &[], super::zipper::zipper_moving_tests::zipper_moving_basic_test)
                }

                #[test]
                fn [<$z_name _zipper_with_root_path>]() {
                    let mut temp_store = $read_keys(super::zipper::zipper_moving_tests::ZIPPER_WITH_ROOT_PATH_KEYS);
                    super::zipper::zipper_moving_tests::run_test(&mut temp_store, $make_z, super::zipper::zipper_moving_tests::ZIPPER_WITH_ROOT_PATH_PATH, super::zipper::zipper_moving_tests::zipper_with_root_path)
                }

                #[test]
                fn [<$z_name _zipper_indexed_bytes_test1>]() {
                    let mut temp_store = $read_keys(super::zipper::zipper_moving_tests::ZIPPER_INDEXED_BYTE_TEST1_KEYS);
                    super::zipper::zipper_moving_tests::run_test(&mut temp_store, $make_z, &[], super::zipper::zipper_moving_tests::zipper_indexed_bytes_test1)
                }

                #[test]
                fn [<$z_name _zipper_indexed_bytes_test2>]() {
                    let mut temp_store = $read_keys(super::zipper::zipper_moving_tests::ZIPPER_INDEXED_BYTE_TEST2_KEYS);
                    super::zipper::zipper_moving_tests::run_test(&mut temp_store, $make_z, &[], super::zipper::zipper_moving_tests::zipper_indexed_bytes_test2)
                }

                #[test]
                fn [<$z_name _zipper_descend_until_test1>]() {
                    let mut temp_store = $read_keys(super::zipper::zipper_moving_tests::ZIPPER_DESCEND_UNTIL_TEST1_KEYS);
                    super::zipper::zipper_moving_tests::run_test(&mut temp_store, $make_z, &[], super::zipper::zipper_moving_tests::zipper_descend_until_test1)
                }

                #[test]
                fn [<$z_name _zipper_descend_until_max_bytes_test1>]() {
                    let mut temp_store = $read_keys(super::zipper::zipper_moving_tests::ZIPPER_DESCEND_UNTIL_MAX_BYTES_TEST1_KEYS);
                    super::zipper::zipper_moving_tests::run_test(&mut temp_store, $make_z, &[], super::zipper::zipper_moving_tests::zipper_descend_until_max_bytes_test1)
                }

                #[test]
                fn [<$z_name _zipper_ascend_until_test1>]() {
                    let mut temp_store = $read_keys(super::zipper::zipper_moving_tests::ZIPPER_ASCEND_UNTIL_TEST1_KEYS);
                    super::zipper::zipper_moving_tests::run_test(&mut temp_store, $make_z, &[], super::zipper::zipper_moving_tests::zipper_ascend_until_test1)
                }

                #[test]
                fn [<$z_name _zipper_ascend_until_test2>]() {
                    let mut temp_store = $read_keys(super::zipper::zipper_moving_tests::ZIPPER_ASCEND_UNTIL_TEST2_KEYS);
                    super::zipper::zipper_moving_tests::run_test(&mut temp_store, $make_z, &[], super::zipper::zipper_moving_tests::zipper_ascend_until_test2)
                }

                #[test]
                fn [<$z_name _zipper_ascend_until_test3>]() {
                    let mut temp_store = $read_keys(super::zipper::zipper_moving_tests::ZIPPER_ASCEND_UNTIL_TEST3_KEYS);
                    super::zipper::zipper_moving_tests::run_test(&mut temp_store, $make_z, &[], super::zipper::zipper_moving_tests::zipper_ascend_until_test3)
                }

                #[test]
                fn [<$z_name _zipper_ascend_until_test4>]() {
                    let mut temp_store = $read_keys(super::zipper::zipper_moving_tests::ZIPPER_ASCEND_UNTIL_TEST4_KEYS);
                    super::zipper::zipper_moving_tests::run_test(&mut temp_store, $make_z, &[], super::zipper::zipper_moving_tests::zipper_ascend_until_test4)
                }

                #[test]
                fn [<$z_name _zipper_ascend_until_test5>]() {
                    let mut temp_store = $read_keys(super::zipper::zipper_moving_tests::ZIPPER_ASCEND_UNTIL_TEST5_KEYS);
                    super::zipper::zipper_moving_tests::run_test(&mut temp_store, $make_z, &[], super::zipper::zipper_moving_tests::zipper_ascend_until_test5)
                }

                #[test]
                fn [<$z_name _indexed_zipper_movement1>]() {
                    let mut temp_store = $read_keys(super::zipper::zipper_moving_tests::ZIPPER_INDEXED_MOVEMENT_TEST1_KEYS);
                    super::zipper::zipper_moving_tests::run_test(&mut temp_store, $make_z, &[], super::zipper::zipper_moving_tests::indexed_zipper_movement1)
                }

                #[test]
                fn [<$z_name _zipper_value_locations>]() {
                    let mut temp_store = $read_keys(super::zipper::zipper_moving_tests::ZIPPER_VALUE_LOCATIONS_TEST1_KEYS);
                    super::zipper::zipper_moving_tests::run_test(&mut temp_store, $make_z, &[], super::zipper::zipper_moving_tests::zipper_value_locations)
                }

                #[test]
                fn [<$z_name _zipper_child_mask_test1>]() {
                    let mut temp_store = $read_keys(super::zipper::zipper_moving_tests::ZIPPER_CHILD_MASK_TEST1_KEYS);
                    super::zipper::zipper_moving_tests::run_test(&mut temp_store, $make_z, &[], super::zipper::zipper_moving_tests::zipper_child_mask_test1)
                }

                #[test]
                fn [<$z_name _zipper_child_mask_test2>]() {
                    let mut temp_store = $read_keys(super::zipper::zipper_moving_tests::ZIPPER_CHILD_MASK_TEST2_KEYS);
                    super::zipper::zipper_moving_tests::run_test(&mut temp_store, $make_z, &[], super::zipper::zipper_moving_tests::zipper_child_mask_test2)
                }

                #[test]
                fn [<$z_name _descend_to_existing_test1>]() {
                    let mut temp_store = $read_keys(super::zipper::zipper_moving_tests::ZIPPER_DESCEND_TO_EXISTING_TEST1_KEYS);
                    super::zipper::zipper_moving_tests::run_test(&mut temp_store, $make_z, &[], super::zipper::zipper_moving_tests::descend_to_existing_test1)
                }

                #[test]
                fn [<$z_name _descend_to_existing_test2>]() {
                    let mut temp_store = $read_keys(super::zipper::zipper_moving_tests::ZIPPER_DESCEND_TO_EXISTING_TEST2_KEYS);
                    super::zipper::zipper_moving_tests::run_test(&mut temp_store, $make_z, &[], super::zipper::zipper_moving_tests::descend_to_existing_test2)
                }

                #[test]
                fn [<$z_name _descend_to_existing_test3>]() {
                    let mut temp_store = $read_keys(super::zipper::zipper_moving_tests::ZIPPER_DESCEND_TO_EXISTING_TEST3_KEYS);
                    super::zipper::zipper_moving_tests::run_test(&mut temp_store, $make_z, &[], super::zipper::zipper_moving_tests::descend_to_existing_test3)
                }

                #[test]
                fn [<$z_name _to_next_step_test1>]() {
                    let mut temp_store = $read_keys(super::zipper::zipper_moving_tests::ZIPPER_TO_NEXT_STEP_TEST1_KEYS);
                    super::zipper::zipper_moving_tests::run_test(&mut temp_store, $make_z, &[], super::zipper::zipper_moving_tests::to_next_step_test1)
                }

                #[test]
                fn [<$z_name _zipper_byte_iter_test1>]() {
                    let mut temp_store = $read_keys(super::zipper::zipper_moving_tests::ZIPPER_BYTES_ITER_TEST1_KEYS);
                    super::zipper::zipper_moving_tests::run_test(&mut temp_store, $make_z, &[], super::zipper::zipper_moving_tests::zipper_byte_iter_test1)
                }

                #[test]
                fn [<$z_name _zipper_byte_iter_test2>]() {
                    let mut temp_store = $read_keys(super::zipper::zipper_moving_tests::ZIPPER_BYTES_ITER_TEST2_KEYS);
                    super::zipper::zipper_moving_tests::run_test(&mut temp_store, $make_z, super::zipper::zipper_moving_tests::ZIPPER_BYTES_ITER_TEST2_PATH, super::zipper::zipper_moving_tests::zipper_byte_iter_test2)
                }

                #[test]
                fn [<$z_name _zipper_byte_iter_test3>]() {
                    let mut temp_store = $read_keys(super::zipper::zipper_moving_tests::ZIPPER_BYTES_ITER_TEST3_KEYS);
                    super::zipper::zipper_moving_tests::run_test(&mut temp_store, $make_z, super::zipper::zipper_moving_tests::ZIPPER_BYTES_ITER_TEST3_PATH, super::zipper::zipper_moving_tests::zipper_byte_iter_test3)
                }

                #[test]
                fn [<$z_name _zipper_byte_iter_test4>]() {
                    let mut temp_store = $read_keys(super::zipper::zipper_moving_tests::ZIPPER_BYTES_ITER_TEST4_KEYS);
                    super::zipper::zipper_moving_tests::run_test(&mut temp_store, $make_z, &[], super::zipper::zipper_moving_tests::zipper_byte_iter_test4)
                }

                #[test]
                fn [<$z_name _zipper_byte_iter_test5>]() {
                    let mut temp_store = $read_keys(super::zipper::zipper_moving_tests::ZIPPER_BYTES_ITER_TEST5_KEYS);
                    super::zipper::zipper_moving_tests::run_test(&mut temp_store, $make_z, &[], super::zipper::zipper_moving_tests::zipper_byte_iter_test5)
                }
            }
        }
    }
    pub(crate) use zipper_moving_tests;

    /// Internal method to provide a lifetime bound on the macro arguments to the test macro
    pub fn run_test<'a, T: 'a + ZipperMoving, Store>(
        store: &'a mut Store,
        make_t: impl Fn(&'a mut Store, &'a[u8]) -> T,
        z_path: &'a[u8],
        test_f: impl Fn(T)
    ) {
        let t = make_t(store, z_path);
        test_f(t);
    }

    /// from https://en.wikipedia.org/wiki/Radix_tree#/media/File:Patricia_trie.svg
    pub const ZIPPER_MOVING_BASIC_TEST_KEYS: &[&[u8]] = &[b"romane", b"romanus", b"romulus", b"rubens", b"ruber", b"rubicon", b"rubicundus", b"rom'i"];

    pub fn zipper_moving_basic_test<Z: ZipperMoving>(mut zipper: Z) {
        fn assert_in_list(val: &[u8], list: &[&[u8]]) {
            for test_val in list {
                if *test_val == val {
                    return;
                }
            }
            panic!("val not found in list: {}", std::str::from_utf8(val).unwrap_or(""))
        }

        zipper.descend_to(&[b'r']); zipper.descend_to(&[b'o']); zipper.descend_to(&[b'm']); // focus = rom
        zipper.descend_to(&[b'\'']);
        assert!(zipper.path_exists()); // focus = rom'  (' is the lowest byte)
        assert!(zipper.to_next_sibling_byte()); // focus = roma  (a is the second byte), but we can't actually guarantee whether we land on 'a' or 'u'
        assert_in_list(zipper.path(), &[b"roma", b"romu"]);
        assert_eq!(zipper.child_mask().iter().collect::<Vec<_>>(), vec![b'n']); // both follow-ups romane and romanus have n following a
        assert!(zipper.to_next_sibling_byte()); // focus = romu  (u is the third byte)
        assert_in_list(zipper.path(), &[b"roma", b"romu"]);
        assert_eq!(zipper.child_mask().iter().collect::<Vec<_>>(), vec![b'l']); // and romu is followed by lus
        assert!(!zipper.to_next_sibling_byte()); // fails because there were only 3 children ['\'', 'a', 'u']
        assert!(zipper.to_prev_sibling_byte()); // focus = roma or romu (we stepped back)
        assert_in_list(zipper.path(), &[b"roma", b"romu"]);
        assert!(zipper.to_prev_sibling_byte()); // focus = rom' (we stepped back to where we began)
        assert_eq!(zipper.path(), b"rom'");
        assert_eq!(zipper.child_mask().iter().collect::<Vec<_>>(), vec![b'i']);
        assert!(zipper.ascend(1)); // focus = rom
        assert_eq!(zipper.child_mask().iter().collect::<Vec<_>>(), vec![b'\'', b'a', b'u']); // all three options we visited
        assert!(zipper.descend_indexed_byte(0)); // focus = rom'
        assert_eq!(zipper.child_mask().iter().collect::<Vec<_>>(), vec![b'i']);
        assert!(zipper.ascend(1)); // focus = rom
        assert!(zipper.descend_indexed_byte(1)); // focus = roma
        assert_eq!(zipper.child_mask().iter().collect::<Vec<_>>(), vec![b'n']);
        assert!(zipper.ascend(1));
        assert!(zipper.descend_indexed_byte(2)); // focus = romu
        assert_eq!(zipper.child_mask().iter().collect::<Vec<_>>(), vec![b'l']);
        assert!(zipper.ascend(1));
        assert!(zipper.descend_indexed_byte(1)); // focus = roma
        assert_eq!(zipper.child_mask().iter().collect::<Vec<_>>(), vec![b'n']);
        assert!(zipper.ascend(1));
        // ' < a < u
        // 39 105 117
    }

    pub const ZIPPER_WITH_ROOT_PATH_KEYS: &[&[u8]] = &[b"romane", b"romanus", b"romulus", b"rubens", b"ruber", b"rubicon", b"rubicundus", b"rom'i"];
    pub const ZIPPER_WITH_ROOT_PATH_PATH: &[u8] = b"ro";

    /// Tests creating a zipper at a specific key within a map
    pub fn zipper_with_root_path<Z: ZipperMoving>(mut zipper: Z) {

        //Test `descend_to` and `ascend_until`
        assert_eq!(zipper.path(), b"");
        assert_eq!(zipper.child_count(), 1);
        zipper.descend_to(b"m");
        assert_eq!(zipper.path(), b"m");
        assert_eq!(zipper.child_count(), 3);
        zipper.descend_to(b"an");
        assert_eq!(zipper.path(), b"man");
        assert_eq!(zipper.child_count(), 2);
        zipper.descend_to(b"e");
        assert_eq!(zipper.path(), b"mane");
        assert_eq!(zipper.child_count(), 0);
        assert_eq!(zipper.ascend_until(), true);
        zipper.descend_to(b"us");
        assert_eq!(zipper.path(), b"manus");
        assert_eq!(zipper.child_count(), 0);
        assert_eq!(zipper.ascend_until(), true);
        assert_eq!(zipper.path(), b"man");
        assert_eq!(zipper.child_count(), 2);
        assert_eq!(zipper.ascend_until(), true);
        assert_eq!(zipper.path(), b"m");
        assert_eq!(zipper.child_count(), 3);
        assert_eq!(zipper.ascend_until(), true);
        assert_eq!(zipper.path(), b"");
        assert_eq!(zipper.child_count(), 1);
        assert_eq!(zipper.at_root(), true);
        assert_eq!(zipper.ascend_until(), false);

        //Test `ascend`
        zipper.descend_to(b"manus");
        assert_eq!(zipper.path(), b"manus");
        assert_eq!(zipper.ascend(1), true);
        assert_eq!(zipper.path(), b"manu");
        assert_eq!(zipper.ascend(5), false);
        assert_eq!(zipper.path(), b"");
        assert_eq!(zipper.at_root(), true);
        zipper.descend_to(b"mane");
        assert_eq!(zipper.path(), b"mane");
        assert_eq!(zipper.ascend(3), true);
        assert_eq!(zipper.path(), b"m");
        assert_eq!(zipper.child_count(), 3);
    }

    // A wide shallow trie
    pub const ZIPPER_INDEXED_BYTE_TEST1_KEYS: &[&[u8]] = &[b"0", b"1", b"2", b"3", b"4", b"5", b"6"];

    pub fn zipper_indexed_bytes_test1<Z: ZipperMoving>(mut zip: Z) {
        zip.descend_to("2");
        assert_eq!(zip.is_val(), true);
        assert_eq!(zip.child_count(), 0);
        assert!(!zip.descend_indexed_byte(1));
        assert_eq!(zip.path(), b"2");

        zip.reset();
        assert!(zip.descend_indexed_byte(2));
        assert_eq!(zip.is_val(), true);
        assert_eq!(zip.child_count(), 0);
        assert_eq!(zip.path(), b"2");
        assert!(!zip.descend_indexed_byte(1));
        assert_eq!(zip.path(), b"2");

        zip.reset();
        assert!(!zip.descend_indexed_byte(7));
        assert_eq!(zip.is_val(), false);
        assert_eq!(zip.child_count(), 7);
        assert_eq!(zip.path(), b"");

        // Try with a narrow deeper trie
        let keys = ["000", "1Z", "00AAA", "00AA000", "00AA00AAA"];
        let map: PathMap<()> = keys.into_iter().map(|v| (v, ())).collect();
        let mut zip = map.read_zipper();

        zip.descend_to("000");
        assert_eq!(zip.val(), Some(&()));
        assert_eq!(zip.path(), b"000");
        assert_eq!(zip.child_count(), 0);
        assert!(!zip.descend_indexed_byte(1));
        assert_eq!(zip.path(), b"000");

        zip.reset();
        assert!(!zip.descend_indexed_byte(2));
        assert_eq!(zip.child_count(), 2);
        assert!(zip.descend_indexed_byte(1));
        assert_eq!(zip.path(), b"1");
        assert_eq!(zip.val(), None);
        assert_eq!(zip.child_count(), 1);
        assert!(!zip.descend_indexed_byte(1));
        assert_eq!(zip.val(), None);
        assert_eq!(zip.path(), b"1");

        zip.reset();
        assert!(zip.descend_indexed_byte(0));
        assert_eq!(zip.path(), b"0");
        assert_eq!(zip.val(), None);
        assert_eq!(zip.child_count(), 1);
        assert!(!zip.descend_indexed_byte(1));
        assert_eq!(zip.val(), None);
        assert_eq!(zip.path(), b"0");
    }

    // A narrow deeper trie
    pub const ZIPPER_INDEXED_BYTE_TEST2_KEYS: &[&[u8]] = &[b"000", b"1Z", b"00AAA", b"00AA000", b"00AA00AAA"];

    pub fn zipper_indexed_bytes_test2<Z: ZipperMoving>(mut zip: Z) {
        zip.descend_to("000");
        assert_eq!(zip.is_val(), true);
        assert_eq!(zip.path(), b"000");
        assert_eq!(zip.child_count(), 0);
        assert!(!zip.descend_indexed_byte(1));
        assert_eq!(zip.path(), b"000");

        zip.reset();
        assert!(!zip.descend_indexed_byte(2));
        assert_eq!(zip.child_count(), 2);
        assert!(zip.descend_indexed_byte(1));
        assert_eq!(zip.path(), b"1");
        assert_eq!(zip.is_val(), false);
        assert_eq!(zip.child_count(), 1);
        assert!(!zip.descend_indexed_byte(1));
        assert_eq!(zip.is_val(), false);
        assert_eq!(zip.path(), b"1");

        zip.reset();
        assert!(zip.descend_indexed_byte(0));
        assert_eq!(zip.path(), b"0");
        assert_eq!(zip.is_val(), false);
        assert_eq!(zip.child_count(), 1);
        assert!(!zip.descend_indexed_byte(1));
        assert_eq!(zip.is_val(), false);
        assert_eq!(zip.path(), b"0");
    }

    // Tests how descend_until treats values along paths
    pub const ZIPPER_DESCEND_UNTIL_TEST1_KEYS: &[&[u8]] = &[b"a", b"ab", b"abCDEf", b"abCDEfGHi"];

    pub fn zipper_descend_until_test1<Z: ZipperMoving>(mut zip: Z) {
        for key in ZIPPER_DESCEND_UNTIL_TEST1_KEYS {
            assert!(zip.descend_until());
            assert_eq!(zip.path(), *key);
        }
    }

    // Tests how descend_until_max_bytes enforces a max descent length
    pub const ZIPPER_DESCEND_UNTIL_MAX_BYTES_TEST1_KEYS: &[&[u8]] = &[b"a0abcdef", b"a0abcxy", b"a1mnopqr"];

    pub fn zipper_descend_until_max_bytes_test1<Z: ZipperMoving>(mut zip: Z) {
        zip.descend_to(b"a0");
        assert_eq!(zip.path(), b"a0");
        assert!(zip.descend_until_max_bytes(2));
        assert_eq!(zip.path(), b"a0ab");

        zip.reset();
        zip.descend_to(b"a1");
        assert_eq!(zip.path(), b"a1");
        assert!(zip.descend_until_max_bytes(3));
        assert_eq!(zip.path(), b"a1mno");

        zip.reset();
        zip.descend_to(b"a0");
        assert_eq!(zip.path(), b"a0");
        assert!(zip.descend_until_max_bytes(10));
        assert_eq!(zip.path(), b"a0abc");

        zip.reset();
        zip.descend_to(b"a0");
        assert_eq!(zip.path(), b"a0");
        assert!(!zip.descend_until_max_bytes(0));
        assert_eq!(zip.path(), b"a0");
    }

    // Test a 3-way branch, so we definitely don't have a pair node
    pub const ZIPPER_ASCEND_UNTIL_TEST1_KEYS: &[&[u8]] = &[b"AAa", b"AAb", b"AAc"];

    pub fn zipper_ascend_until_test1<Z: ZipperMoving>(mut zip: Z) {
        zip.descend_to(b"AAaDDd");
        assert!(!zip.path_exists());
        assert_eq!(zip.path(), b"AAaDDd");
        assert!(zip.ascend_until());
        assert_eq!(zip.path(), b"AAa");
        assert!(zip.ascend_until());
        assert_eq!(zip.path(), b"AA");
        assert!(zip.ascend_until());
        assert_eq!(zip.path(), b"");
        assert!(!zip.ascend_until());
    }

    // Test what's likely to be represented as a pair node
    pub const ZIPPER_ASCEND_UNTIL_TEST2_KEYS: &[&[u8]] = &[b"AAa", b"AAb"];

    pub fn zipper_ascend_until_test2<Z: ZipperMoving>(mut zip: Z) {
        zip.descend_to(b"AAaDDd");
        assert!(!zip.path_exists());
        assert_eq!(zip.path(), b"AAaDDd");
        assert!(zip.ascend_until());
        assert_eq!(zip.path(), b"AAa");
        assert!(zip.ascend_until());
        assert_eq!(zip.path(), b"AA");
        assert!(zip.ascend_until());
        assert_eq!(zip.path(), b"");
        assert!(!zip.ascend_until());
    }

    /// Test a straight-line trie
    pub const ZIPPER_ASCEND_UNTIL_TEST3_KEYS: &[&[u8]] = &[b"1", b"12", b"123", b"1234", b"12345"];

    pub fn zipper_ascend_until_test3<Z: ZipperMoving>(mut zip: Z) {

        //First test that ascend_until stops when transitioning from non-existent path
        zip.descend_to(b"123456");
        assert_eq!(zip.path_exists(), false);
        assert!(zip.ascend_until());
        assert_eq!(zip.path(), b"12345");

        //Test that ascend_until stops at each value
        assert!(zip.ascend_until());
        assert_eq!(zip.path(), b"1234");
        assert!(zip.ascend_until());
        assert_eq!(zip.path(), b"123");
        assert!(zip.ascend_until());
        assert_eq!(zip.path(), b"12");
        assert!(zip.ascend_until());
        assert_eq!(zip.path(), b"1");
        assert!(zip.ascend_until());
        assert_eq!(zip.path(), b"");
        assert!(!zip.ascend_until());
        assert!(zip.at_root());

        //Test that ascend_until_branch skips over all the values
        zip.descend_to(b"12345");
        assert!(zip.path_exists());
        assert_eq!(zip.path(), b"12345");
        assert!(zip.ascend_until_branch());
        assert_eq!(zip.path(), b"");
        assert!(zip.at_root());

        //Try with some actual branches in the trie.
        //Some paths encountered will be values only, some will be branches only, and some will be both
        let keys = ["1", "123", "12345", "1abc", "1234abc"];
        let map: PathMap<()> = keys.into_iter().map(|v| (v, ())).collect();
        let mut zip = map.read_zipper();

        zip.descend_to(b"12345");
        assert!(zip.path_exists());
        assert_eq!(zip.path(), b"12345");
        assert!(zip.ascend_until());
        assert_eq!(zip.path(), b"1234"); // "1234" is a branch only
        assert_eq!(zip.is_val(), false);
        assert_eq!(zip.child_count(), 2);
        assert!(zip.ascend_until());
        assert_eq!(zip.path(), b"123"); // "123" is a value only
        assert_eq!(zip.child_count(), 1);
        assert_eq!(zip.is_val(), true);
        assert!(zip.ascend_until()); // Jump over "12" because it's neither a branch nor a value
        assert_eq!(zip.path(), b"1"); // "1" is both a branch and a value
        assert_eq!(zip.is_val(), true);
        assert_eq!(zip.child_count(), 2);
        assert!(zip.ascend_until());
        assert_eq!(zip.path(), b"");
        assert_eq!(zip.child_count(), 1);
        assert!(!zip.ascend_until());
        assert!(zip.at_root());

        //Test that ascend_until_branch skips over all the values
        zip.descend_to(b"12345");
        assert!(zip.path_exists());
        assert!(zip.ascend_until_branch());
        assert_eq!(zip.path(), b"1234");
        assert!(zip.ascend_until_branch());
        assert_eq!(zip.path(), b"1");
        assert!(zip.ascend_until_branch());
        assert_eq!(zip.path(), b"");
        assert!(!zip.ascend_until_branch());
        assert!(zip.at_root());
    }

    /// Test a trie with some actual branches
    /// Some paths encountered will be values only, some will be branches only, and some will be both
    pub const ZIPPER_ASCEND_UNTIL_TEST4_KEYS: &[&[u8]] = &[b"1", b"123", b"12345", b"1abc", b"1234abc"];

    pub fn zipper_ascend_until_test4<Z: ZipperMoving>(mut zip: Z) {

        zip.descend_to(b"12345");
        assert!(zip.path_exists());
        assert_eq!(zip.path(), b"12345");
        assert!(zip.ascend_until());
        assert_eq!(zip.path(), b"1234"); // "1234" is a branch only
        assert_eq!(zip.is_val(), false);
        assert_eq!(zip.child_count(), 2);
        assert!(zip.ascend_until());
        assert_eq!(zip.path(), b"123"); // "123" is a value only
        assert_eq!(zip.child_count(), 1);
        assert_eq!(zip.is_val(), true);
        assert!(zip.ascend_until()); // Jump over "12" because it's neither a branch nor a value
        assert_eq!(zip.path(), b"1"); // "1" is both a branch and a value
        assert_eq!(zip.is_val(), true);
        assert_eq!(zip.child_count(), 2);
        assert!(zip.ascend_until());
        assert_eq!(zip.path(), b"");
        assert_eq!(zip.child_count(), 1);
        assert!(!zip.ascend_until());
        assert!(zip.at_root());

        //Test that ascend_until_branch skips over all the values
        zip.descend_to(b"12345");
        assert!(zip.path_exists());
        assert!(zip.ascend_until_branch());
        assert_eq!(zip.path(), b"1234");
        assert!(zip.ascend_until_branch());
        assert_eq!(zip.path(), b"1");
        assert!(zip.ascend_until_branch());
        assert_eq!(zip.path(), b"");
        assert!(!zip.ascend_until_branch());
        assert!(zip.at_root());
    }

    /// Test ascending over a long key that spans multiple nodes
    pub const ZIPPER_ASCEND_UNTIL_TEST5_KEYS: &[&[u8]] = &[b"A", b"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"];

    pub fn zipper_ascend_until_test5<Z: ZipperMoving>(mut zip: Z) {

        //Test that ascend_until stops when transitioning from non-existent path
        zip.descend_to(b"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB");
        assert_eq!(zip.path_exists(), false);
        assert!(zip.ascend_until());
        assert_eq!(zip.path(), b"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA");

        //Test that jump all the way back to where we want to be
        assert!(zip.ascend_until());
        assert_eq!(zip.path(), b"A");
        assert!(zip.ascend_until());
        assert_eq!(zip.path(), b"");
        assert_eq!(zip.ascend_until(), false);
    }

    pub const ZIPPER_INDEXED_MOVEMENT_TEST1_KEYS: &[&[u8]] = &[b"arrow", b"bow", b"cannon", b"romane", b"romanus", b"romulus", b"rubens", b"ruber", b"rubicon", b"rubicundus", b"rom'i"];

    pub fn indexed_zipper_movement1<Z: ZipperMoving>(mut zipper: Z) {
        //descends a single specific byte using `descend_indexed_byte`. Just for testing. A real user would use `descend_towards`
        fn descend_byte<Z: Zipper + ZipperMoving>(zipper: &mut Z, byte: u8) {
            for i in 0..zipper.child_count() {
                assert_eq!(zipper.descend_indexed_byte(i), true);
                if *zipper.path().last().unwrap() == byte {
                    break
                } else {
                    assert_eq!(zipper.ascend(1), true);
                }
            }
        }

        assert_eq!(zipper.path(), b"");
        assert_eq!(zipper.child_count(), 4);
        descend_byte(&mut zipper, b'r');
        assert_eq!(zipper.path(), b"r");
        assert_eq!(zipper.child_count(), 2);
        assert_eq!(zipper.descend_until(), false);
        descend_byte(&mut zipper, b'o');
        assert_eq!(zipper.path(), b"ro");
        assert_eq!(zipper.child_count(), 1);
        assert_eq!(zipper.descend_until(), true);
        assert_eq!(zipper.path(), b"rom");
        assert_eq!(zipper.child_count(), 3);

        zipper.reset();
        assert_eq!(zipper.descend_until(), false);
        descend_byte(&mut zipper, b'a');
        assert_eq!(zipper.path(), b"a");
        assert_eq!(zipper.child_count(), 1);
        assert_eq!(zipper.descend_until(), true);
        assert_eq!(zipper.path(), b"arrow");
        assert_eq!(zipper.child_count(), 0);

        assert_eq!(zipper.ascend(3), true);
        assert_eq!(zipper.path(), b"ar");
        assert_eq!(zipper.child_count(), 1);
    }

    pub const ZIPPER_VALUE_LOCATIONS_TEST1_KEYS: &[&[u8]] = &[b"arrow", b"bow", b"cannon", b"roman", b"romane", b"romanus", b"romulus", b"rubens", b"ruber", b"rubicon", b"rubicundus", b"rom'i"];

    pub fn zipper_value_locations<Z: ZipperMoving>(mut zipper: Z) {

        zipper.descend_to(b"ro");
        assert!(zipper.path_exists());
        assert_eq!(zipper.is_val(), false);
        zipper.descend_to(b"mulus");
        assert_eq!(zipper.is_val(), true);

        zipper.reset();
        zipper.descend_to(b"roman");
        assert!(zipper.path_exists());
        assert_eq!(zipper.is_val(), true);
        zipper.descend_to(b"e");
        assert_eq!(zipper.is_val(), true);
        assert_eq!(zipper.ascend(1), true);
        zipper.descend_to(b"u");
        assert_eq!(zipper.is_val(), false);
        zipper.descend_until();
        assert_eq!(zipper.is_val(), true);
    }

    pub const ZIPPER_CHILD_MASK_TEST1_KEYS: &[&[u8]] = &[&[8, 194, 1, 45, 194, 1], &[34, 193]];

    pub fn zipper_child_mask_test1<Z: ZipperMoving>(mut zipper: Z) {

        zipper.descend_to(&[8, 194, 1]);
        assert_eq!(zipper.path_exists(), true);
        assert_eq!(zipper.child_count(), 1);
        assert_eq!(zipper.child_mask(), [0x200000000000, 0, 0, 0]);

        zipper.reset();
        zipper.descend_to(&[8, 194, 1, 45]);
        assert_eq!(zipper.path_exists(), true);
        assert_eq!(zipper.child_count(), 1);
        assert_eq!(zipper.child_mask(), [0, 0, 0, 0x4]);
    }

    pub const ZIPPER_CHILD_MASK_TEST2_KEYS: &[&[u8]] = &[b"arrow", b"bow", b"cannon", b"roman", b"romane", b"romanus", b"romulus", b"rubens", b"ruber", b"rubicon", b"rubicundus", b"rom'i"];

    pub fn zipper_child_mask_test2<Z: ZipperMoving>(mut zipper: Z) {

        //'a' + 'b' + 'c' + 'r'
        assert_eq!(zipper.child_mask(), [0, 1<<(b'a'-64) | 1<<(b'b'-64) | 1<<(b'c'-64) | 1<<(b'r'-64), 0, 0]);

        let mut i = 0;
        while zipper.to_next_step() {
            match i {
                //'r' descending from 'a' in "arrow"
                0 => assert_eq!(zipper.child_mask(), [0, 1<<(b'r'-64), 0, 0]),
                //'r' descending from "ar" in "arrow"
                1 => assert_eq!(zipper.child_mask(), [0, 1<<(b'r'-64), 0, 0]),
                //'o' descending from "arr" in "arrow"
                2 => assert_eq!(zipper.child_mask(), [0, 1<<(b'o'-64), 0, 0]),
                //'w' descending from "arro" in "arrow"
                3 => assert_eq!(zipper.child_mask(), [0, 1<<(b'w'-64), 0, 0]),
                //leaf node, "arrow"
                4 => assert_eq!(zipper.child_mask(), [0, 0, 0, 0]),
                //'o' + 'u' descending from 'r' in "roman", "rubens", etc.
                14 => assert_eq!(zipper.child_mask(), [0, 1<<(b'o'-64) | 1<<(b'u'-64), 0, 0]),
                _ => {}
            }
            i += 1;
        }
    }

    pub const ZIPPER_DESCEND_TO_EXISTING_TEST1_KEYS: &[&[u8]] = &[b"arrow", b"bow", b"cannon", b"roman", b"romane", b"romanus", b"romulus", b"rubens", b"ruber", b"rubicon", b"rubicundus", b"rom'i"];

    pub fn descend_to_existing_test1<Z: ZipperMoving>(mut zipper: Z) {

        assert_eq!(3, zipper.descend_to_existing("bowling"));
        assert_eq!("bow".as_bytes(), zipper.path());
        zipper.reset();

        assert_eq!(3, zipper.descend_to_existing("can"));
        assert_eq!("can".as_bytes(), zipper.path());
        zipper.reset();

        assert_eq!(0, zipper.descend_to_existing(""));
        assert_eq!("".as_bytes(), zipper.path());
        zipper.reset();
    }

    pub const ZIPPER_DESCEND_TO_EXISTING_TEST2_KEYS: &[&[u8]] = &[b"arrow"];

    /// Tests a really long path that doesn't exist, to exercise the chunk-descending code
    pub fn descend_to_existing_test2<Z: ZipperMoving>(mut zipper: Z) {

        assert_eq!(5, zipper.descend_to_existing("arrow0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"));
        assert_eq!(zipper.path(), &b"arrow"[..]);
        zipper.reset();

        assert_eq!(3, zipper.descend_to_existing("arr0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"));
        assert_eq!(zipper.path(), &b"arr"[..]);
    }

    pub const ZIPPER_DESCEND_TO_EXISTING_TEST3_KEYS: &[&[u8]] = &[b"arrow"];

    /// Tests calling the method when the focus is already on a non-existent path
    pub fn descend_to_existing_test3<Z: ZipperMoving>(mut zipper: Z) {

        zipper.descend_to("arrow00000");
        assert_eq!(false, zipper.path_exists());
        assert_eq!(zipper.path(), &b"arrow00000"[..]);

        assert_eq!(0, zipper.descend_to_existing("0000"));
        assert_eq!(zipper.path(), &b"arrow00000"[..]);
    }

    pub const ZIPPER_TO_NEXT_STEP_TEST1_KEYS: &[&[u8]] = &[b"arrow", b"bow", b"cannon", b"roman", b"romane", b"romanus", b"romulus", b"rubens", b"ruber", b"rubicon", b"rubicundus", b"rom'i"];

    pub fn to_next_step_test1<Z: ZipperMoving>(mut zipper: Z) {
        let mut i = 0;
        while zipper.to_next_step() {
            match i {
                0 => assert_eq!(zipper.path(), b"a"),
                4 => assert_eq!(zipper.path(), b"arrow"),
                5 => assert_eq!(zipper.path(), b"b"),
                7 => assert_eq!(zipper.path(), b"bow"),
                8 => assert_eq!(zipper.path(), b"c"),
                13 => assert_eq!(zipper.path(), b"cannon"),
                14 => assert_eq!(zipper.path(), b"r"),
                18 => assert_eq!(zipper.path(), b"rom'i"),
                20 => assert_eq!(zipper.path(), b"roman"),
                21 => assert_eq!(zipper.path(), b"romane"),
                23 => assert_eq!(zipper.path(), b"romanus"),
                24 => assert_eq!(zipper.path(), b"romu"),
                25 => assert_eq!(zipper.path(), b"romul"),
                26 => assert_eq!(zipper.path(), b"romulu"),
                27 => assert_eq!(zipper.path(), b"romulus"),
                28 => assert_eq!(zipper.path(), b"ru"),
                32 => assert_eq!(zipper.path(), b"rubens"),
                33 => assert_eq!(zipper.path(), b"ruber"),
                37 => assert_eq!(zipper.path(), b"rubicon"),
                42 => assert_eq!(zipper.path(), b"rubicundus"),
                _ => {}
            }
            i += 1;
        }
    }

    pub const ZIPPER_BYTES_ITER_TEST1_KEYS: &[&[u8]] = &[b"ABCDEFGHIJKLMNOPQRSTUVWXYZ", b"ab",];

    pub fn zipper_byte_iter_test1<Z: ZipperMoving>(mut zipper: Z) {

        zipper.descend_to_byte(b'A');
        assert_eq!(zipper.path_exists(), true);
        assert_eq!(zipper.descend_first_byte(), true);
        assert_eq!(zipper.path(), b"AB");
        assert_eq!(zipper.to_next_sibling_byte(), false);
        assert_eq!(zipper.path(), b"AB");
    }

    pub const ZIPPER_BYTES_ITER_TEST2_KEYS: &[&[u8]] = &[&[2, 194, 1, 1, 193, 5], &[3, 194, 1, 0, 193, 6, 193, 5], &[3, 193, 4, 193]];
    pub const ZIPPER_BYTES_ITER_TEST2_PATH: &[u8] = &[2, 194];

    pub fn zipper_byte_iter_test2<Z: ZipperMoving>(mut zipper: Z) {
        assert_eq!(zipper.descend_first_byte(), true);
        assert_eq!(zipper.path(), &[1]);
        assert_eq!(zipper.to_next_sibling_byte(), false);
        assert_eq!(zipper.path(), &[1]);
    }

    pub const ZIPPER_BYTES_ITER_TEST3_KEYS: &[&[u8]] = &[&[3, 193, 4, 193, 5, 2, 193, 6, 193, 7], &[3, 193, 4, 193, 5, 2, 193, 6, 255]];
    pub const ZIPPER_BYTES_ITER_TEST3_PATH: &[u8] = &[3, 193, 4, 193, 5, 2, 193];

    pub fn zipper_byte_iter_test3<Z: ZipperMoving>(mut zipper: Z) {
        assert_eq!(zipper.path(), &[]);
        assert_eq!(zipper.descend_first_byte(), true);
        assert_eq!(zipper.path(), &[6]);
        assert_eq!(zipper.descend_first_byte(), true);
        assert_eq!(zipper.path(), &[6, 193]);
        assert_eq!(zipper.descend_first_byte(), true);
        assert_eq!(zipper.path(), &[6, 193, 7]);
    }

    pub const ZIPPER_BYTES_ITER_TEST4_KEYS: &[&[u8]] = &[b"ABC", b"ABCDEF", b"ABCdef"];

    pub fn zipper_byte_iter_test4<Z: ZipperMoving>(mut zipper: Z) {

        //Check that we end up at the first leaf by depth-first search
        while zipper.descend_first_byte() {}
        assert_eq!(zipper.path(), b"ABCDEF");

        //Try taking a different branch
        zipper.reset();
        zipper.descend_to(b"ABC");
        assert!(zipper.path_exists());
        assert_eq!(zipper.path(), b"ABC");
        assert!(zipper.descend_indexed_byte(1));
        assert_eq!(zipper.path(), b"ABCd");
        assert!(zipper.descend_first_byte());
        assert_eq!(zipper.path(), b"ABCde");
        assert!(zipper.descend_first_byte());
        assert_eq!(zipper.path(), b"ABCdef");
        assert!(!zipper.descend_first_byte());
    }

    pub const ZIPPER_BYTES_ITER_TEST5_KEYS: &[&[u8]] = &[
        &[2, 197, 97, 120, 105, 111, 109, 3, 193, 61, 4, 193, 97, 192, 192, 3, 193, 75, 192, 3, 193, 84, 192, 3, 193, 75, 128, 131, 193, 49],
        &[2, 197, 97, 120, 105, 111, 109, 3, 193, 61, 4, 193, 97, 192, 192, 3, 193, 84, 3, 193, 75, 192, 192, 3, 193, 75, 128, 131, 193, 49],
        &[2, 197, 97, 120, 255, 111, 109, 3, 193, 61, 4, 193, 97, 192, 192, 3, 193, 84, 3, 193, 75, 192, 192, 3, 193, 75, 128, 131, 193, 49],
    ];

    pub fn zipper_byte_iter_test5<Z: ZipperMoving>(mut zipper: Z) {

        let keys = ZIPPER_BYTES_ITER_TEST5_KEYS;
        for i in 0..keys[0].len() {
            zipper.reset();
            zipper.descend_to(&keys[0][..i]);
            if i != 18 && i != 5 {
                assert_eq!(zipper.to_next_sibling_byte(), false);
            }
        }

        zipper.reset();
        zipper.descend_to([2, 197, 97, 120, 105, 111, 109, 3, 193, 61, 4, 193, 97, 192, 192, 3, 193, 75]);
        assert_eq!(zipper.to_next_sibling_byte(), true);
        zipper.reset();
        zipper.descend_to([2, 197, 97, 120, 105]);
        assert_eq!(zipper.to_next_sibling_byte(), true);
    }

}

/// Implements tests that apply to all [ZipperIteration] types
#[cfg(all(test, feature = "pathmap-internal-tests"))]
pub(crate) mod zipper_iteration_tests {
    use super::*;

    /// `$ident` is a unique identifier for the zipper, so the generated tests don't collide
    /// `$read_keys` is a function that will create a store containing all paths, from which a zipper can be created
    /// `$make_z` is a function that will create a zipper from a slice of paths
    macro_rules! zipper_iteration_tests {
        ($z_name:ident, $read_keys:expr, $make_z:expr)=>{
            paste::paste! {
                #[test]
                fn [<$z_name _zipper_iter_test1>]() {
                    let mut temp_store = $read_keys(super::zipper::zipper_iteration_tests::ZIPPER_ITER_TEST1_KEYS);
                    super::zipper::zipper_iteration_tests::run_test(&mut temp_store, $make_z, &[], super::zipper::zipper_iteration_tests::zipper_iter_test1)
                }

                #[test]
                fn [<$z_name _zipper_iter_test2>]() {
                    let paths = super::zipper::zipper_iteration_tests::zipper_iter_test2_paths();
                    let path_refs: Vec<&[u8]> = paths.iter().map(|path| &path[..]).collect();
                    let mut temp_store = $read_keys(&path_refs[..]);
                    super::zipper::zipper_iteration_tests::run_test(&mut temp_store, $make_z, b"in", super::zipper::zipper_iteration_tests::zipper_iter_test2)
                }

                #[test]
                fn [<$z_name _k_path_test1>]() {
                    let mut temp_store = $read_keys(super::zipper::zipper_iteration_tests::K_PATH_TEST1_KEYS);
                    super::zipper::zipper_iteration_tests::run_test(&mut temp_store, $make_z, b":", super::zipper::zipper_iteration_tests::k_path_test1)
                }

                #[test]
                fn [<$z_name _k_path_test2>]() {
                    let paths = super::zipper::zipper_iteration_tests::k_path_test2_paths();
                    let path_refs: Vec<&[u8]> = paths.iter().map(|path| &path[..]).collect();
                    let mut temp_store = $read_keys(&path_refs[..]);
                    super::zipper::zipper_iteration_tests::run_test(&mut temp_store, $make_z, &[], super::zipper::zipper_iteration_tests::k_path_test2)
                }

                #[test]
                fn [<$z_name _k_path_test3>]() {
                    let mut temp_store = $read_keys(super::zipper::zipper_iteration_tests::K_PATH_TEST3_KEYS);
                    super::zipper::zipper_iteration_tests::run_test(&mut temp_store, $make_z, b":", super::zipper::zipper_iteration_tests::k_path_test3)
                }

                #[test]
                fn [<$z_name _k_path_test4>]() {
                    let mut temp_store = $read_keys(super::zipper::zipper_iteration_tests::K_PATH_TEST4_KEYS);
                    super::zipper::zipper_iteration_tests::run_test(&mut temp_store, $make_z, b"", super::zipper::zipper_iteration_tests::k_path_test4)
                }

                #[test]
                fn [<$z_name _k_path_test5>]() {
                    let mut temp_store = $read_keys(super::zipper::zipper_iteration_tests::K_PATH_TEST5_KEYS);
                    super::zipper::zipper_iteration_tests::run_test(&mut temp_store, $make_z, b"", super::zipper::zipper_iteration_tests::k_path_test5)
                }

                #[test]
                fn [<$z_name _k_path_test6>]() {
                    let mut temp_store = $read_keys(super::zipper::zipper_iteration_tests::K_PATH_TEST6_KEYS);
                    super::zipper::zipper_iteration_tests::run_test(&mut temp_store, $make_z, b"", super::zipper::zipper_iteration_tests::k_path_test6)
                }

                #[test]
                fn [<$z_name _k_path_test7>]() {
                    let mut temp_store = $read_keys(super::zipper::zipper_iteration_tests::K_PATH_TEST7_KEYS);
                    super::zipper::zipper_iteration_tests::run_test(&mut temp_store, $make_z, b"", super::zipper::zipper_iteration_tests::k_path_test7)
                }

                #[test]
                fn [<$z_name _k_path_test8>]() {
                    let mut temp_store = $read_keys(super::zipper::zipper_iteration_tests::K_PATH_TEST8_KEYS);
                    super::zipper::zipper_iteration_tests::run_test(&mut temp_store, $make_z, b"", super::zipper::zipper_iteration_tests::k_path_test8)
                }

                #[test]
                fn [<$z_name _k_path_test9>]() {
                    let mut temp_store = $read_keys(super::zipper::zipper_iteration_tests::K_PATH_TEST9_KEYS);
                    super::zipper::zipper_iteration_tests::run_test(&mut temp_store, $make_z, &[2, 194], super::zipper::zipper_iteration_tests::k_path_test9)
                }
            }
        }
    }
    pub(crate) use zipper_iteration_tests;

    /// Internal method to provide a lifetime bound on the macro arguments to the test macro
    pub fn run_test<'a, T: 'a + ZipperIteration, Store>(
        store: &'a mut Store,
        make_t: impl Fn(&'a mut Store, &[u8]) -> T,
        z_path: &[u8],
        test_f: impl Fn(T)
    ) {
        let t = make_t(store, z_path);
        test_f(t);
    }

    pub const ZIPPER_ITER_TEST1_KEYS: &[&[u8]] = &[b"arrow", b"bow", b"cannon", b"rom'i", b"roman", b"romane", b"romanus", b"romulus", b"rubens", b"ruber", b"rubicon", b"rubicundus"];

    /// Simply calls `to_next_val` over the whole trie, ensuring all paths are visited exactly once
    pub fn zipper_iter_test1<'a, Z: ZipperIteration>(mut zipper: Z) {
        let keys = ZIPPER_ITER_TEST1_KEYS;

        //Test iteration of the whole tree
        let mut idx = 0;
        assert_eq!(zipper.is_val(), false);
        while zipper.to_next_val() {
            println!("{idx}  {}", std::str::from_utf8(zipper.path()).unwrap());
            assert_eq!(keys[idx], zipper.path());
            idx += 1;
        }
        assert_eq!(idx, keys.len());
    }

    const ZIPPER_ITER_TEST2_COUNT: usize = 32;
    pub fn zipper_iter_test2_paths() -> Vec<Vec<u8>> {
        (0usize..ZIPPER_ITER_TEST2_COUNT).into_iter().map(|i| {
            [b"in", &i.to_be_bytes()[..]].concat()
        }).collect()
    }

    pub fn zipper_iter_test2<'a, Z: ZipperIteration>(mut zipper: Z) {

        //Test iterating using a zipper that has a root that is not the map root
        let mut count: usize = 0;
        while zipper.to_next_val() {
            assert_eq!(zipper.is_val(), true);
            assert_eq!(zipper.path(), count.to_be_bytes());
            count += 1;
        }
        assert_eq!(count, ZIPPER_ITER_TEST2_COUNT);
    }

    /// This is a toy encoding where `:n:` precedes a symbol `n` characters long
    pub const K_PATH_TEST1_KEYS: &[&[u8]] = &[
        b":5:above:3:the:4:fray:",
        b":5:err:",
        b":5:erronious:6:potato:",
        b":5:error:2:is:2:my:4:name:",
        b":5:hello:5:world:",
        b":5:mucky:4:muck:",
        b":5:roger:6:rabbit:",
        b":5:zebra:",
        b":9:muckymuck:5:raker:",
    ];

    pub fn k_path_test1<'a, Z: ZipperIteration>(mut zipper: Z) {

        //This is a cheesy way to encode lengths, but it's is more readable than unprintable chars
        assert!(zipper.descend_indexed_byte(0));
        let sym_len = usize::from_str_radix(std::str::from_utf8(&[zipper.path()[0]]).unwrap(), 10).unwrap();
        assert_eq!(sym_len, 5);

        //Step over the ':' character
        assert!(zipper.descend_indexed_byte(0));
        assert_eq!(zipper.child_count(), 6);

        //Start iterating over all the symbols of length=sym_len
        assert_eq!(zipper.descend_first_k_path(sym_len+1), true);

        //This should have taken us to "above:"
        assert_eq!(zipper.path(), b"5:above:");

        //Go to the next symbol.
        // Two interesting things will happen.  First, we blow past "err" because its path length is
        // shorter than `k`.  Second, we will stop in the middle of "erronious".
        // These situations would be caused by an encode bug.  Which hopefully we won't have in real
        // paths. But the parser should recognize the last digit of the path isn't ':'
        assert_eq!(zipper.to_next_k_path(sym_len+1), true);
        assert_eq!(zipper.path(), b"5:erroni");
        assert_ne!(zipper.path().last(), Some(&b':'));

        //Now we'll move on to some correctly formed symbols
        assert_eq!(zipper.to_next_k_path(sym_len+1), true);
        assert_eq!(zipper.path(), b"5:error:");
        assert_eq!(zipper.to_next_k_path(sym_len+1), true);
        assert_eq!(zipper.path(), b"5:hello:");
        assert_eq!(zipper.to_next_k_path(sym_len+1), true);
        assert_eq!(zipper.path(), b"5:mucky:");
        assert_eq!(zipper.to_next_k_path(sym_len+1), true);
        assert_eq!(zipper.path(), b"5:roger:");
        assert_eq!(zipper.to_next_k_path(sym_len+1), true);
        assert_eq!(zipper.path(), b"5:zebra:");

        //The last step should return false, and put us back to where we began
        assert_eq!(zipper.to_next_k_path(sym_len+1), false);
        assert_eq!(zipper.path(), b"5:");
        assert_eq!(zipper.child_count(), 6);
    }

    const K_PATH_TEST2_COUNT: usize = 50;
    pub fn k_path_test2_paths() -> Vec<Vec<u8>> {
        (0..K_PATH_TEST2_COUNT).into_iter().map(|i| {
            let len = (i % 15) + 5; //length between 5 and 20 chars
            (0..len).into_iter().map(|j| ((j+i) % 255) as u8).collect()
        }).collect()
    }

    pub fn k_path_test2<'a, Z: ZipperIteration>(mut zipper: Z) {

        zipper.descend_first_k_path(5);
        let mut count = 1;
        while zipper.to_next_k_path(5) {
            count += 1;
        }
        assert_eq!(count, K_PATH_TEST2_COUNT);
    }

    pub const K_PATH_TEST3_KEYS: &[&[u8]] = &[b":1a1A", b":1a1B", b":1a1C", b":1b1A", b":1b1B", b":1b1C", b":1c1A"];

    pub fn k_path_test3<'a, Z: ZipperIteration>(mut zipper: Z) {

        //Scan over the first symbols in the path (lower case letters)
        zipper.descend_to(b"1");
        assert_eq!(zipper.path_exists(), true);
        assert_eq!(zipper.descend_first_k_path(1), true);
        assert_eq!(zipper.path(), b"1a");
        assert_eq!(zipper.to_next_k_path(1), true);
        assert_eq!(zipper.path(), b"1b");
        assert_eq!(zipper.to_next_k_path(1), true);
        assert_eq!(zipper.path(), b"1c");
        assert_eq!(zipper.to_next_k_path(1), false);
        assert_eq!(zipper.path(), b"1");

        //Scan over the nested second symbols in the path (upper case letters)
        zipper.reset();
        zipper.descend_to(b"1a1");
        assert!(zipper.path_exists());
        assert_eq!(zipper.descend_first_k_path(1), true);
        assert_eq!(zipper.path(), b"1a1A");
        assert_eq!(zipper.to_next_k_path(1), true);
        assert_eq!(zipper.path(), b"1a1B");
        assert_eq!(zipper.to_next_k_path(1), true);
        assert_eq!(zipper.path(), b"1a1C");
        assert_eq!(zipper.to_next_k_path(1), false);
        assert_eq!(zipper.path(), b"1a1");

        //Recursively scan nested symbols within a scan of the first outer symbols
        zipper.reset();
        zipper.descend_to(b"1");
        assert!(zipper.path_exists());
        assert_eq!(zipper.descend_first_k_path(1), true);
        assert_eq!(zipper.path(), b"1a");
        assert_eq!(zipper.descend_first_k_path(2), true);
        assert_eq!(zipper.path(), b"1a1A");
        assert_eq!(zipper.to_next_k_path(2), true);
        assert_eq!(zipper.path(), b"1a1B");
        assert_eq!(zipper.to_next_k_path(2), true);
        assert_eq!(zipper.path(), b"1a1C");
        assert_eq!(zipper.to_next_k_path(2), false);
        assert_eq!(zipper.path(), b"1a");
        assert_eq!(zipper.to_next_k_path(1), true);
        assert_eq!(zipper.path(), b"1b");
        assert_eq!(zipper.to_next_k_path(1), true);
        assert_eq!(zipper.path(), b"1c");
        assert_eq!(zipper.to_next_k_path(1), false);
        assert_eq!(zipper.path(), b"1");

        //Similar to above, but inter-operating with `descend_indexed_byte`
        zipper.reset();
        zipper.descend_to(b"1");
        assert!(zipper.path_exists());
        assert_eq!(zipper.descend_first_k_path(1), true);
        assert_eq!(zipper.path(), b"1a");
        assert_eq!(zipper.descend_indexed_byte(0), true);
        assert_eq!(zipper.path(), b"1a1");
        assert_eq!(zipper.descend_first_k_path(1), true);
        assert_eq!(zipper.path(), b"1a1A");
        assert_eq!(zipper.to_next_k_path(1), true);
        assert_eq!(zipper.path(), b"1a1B");
        assert_eq!(zipper.to_next_k_path(1), true);
        assert_eq!(zipper.path(), b"1a1C");
        assert_eq!(zipper.to_next_k_path(1), false);
        assert_eq!(zipper.path(), b"1a1");
        assert_eq!(zipper.ascend(1), true);
        assert_eq!(zipper.path(), b"1a");
        assert_eq!(zipper.to_next_k_path(1), true);
        assert_eq!(zipper.path(), b"1b");
        assert_eq!(zipper.to_next_k_path(1), true);
        assert_eq!(zipper.path(), b"1c");
        assert_eq!(zipper.to_next_k_path(1), false);
        assert_eq!(zipper.path(), b"1");
    }

    pub const K_PATH_TEST4_KEYS: &[&[u8]] = &[
        &[100, 74, 37, 218, 90, 211, 23, 84, 226, 59, 193, 236],
        &[199, 102, 166, 28, 234, 168, 198, 13],
        &[101, 241, 88, 163, 2, 9, 37, 110, 53, 201, 251, 164, 23, 162, 216],
        &[237, 8, 108, 15, 63, 3, 249, 78, 200, 154, 103, 191],
        &[106, 30, 34, 182, 157, 102, 126, 90, 200, 5, 93, 0, 163, 245, 112],
        &[188, 177, 13, 5, 50, 66, 169, 113, 157, 202, 72, 11, 79, 73],
        &[250, 96, 103, 31, 32, 104],
        &[100, 152, 199, 46, 48, 252, 139, 150, 158, 8, 57, 50, 123],
        &[65, 16, 128, 207, 27, 252, 145, 123, 105, 238, 230],
        &[244, 34, 40, 224, 11, 125, 102],
        &[116, 63, 105, 214, 137, 86, 202],
        &[63, 70, 201, 21, 131, 60],
        &[139, 209, 149, 73, 172, 12, 139, 80, 184, 105],
        &[253, 235, 49, 156, 40, 50, 60, 73, 145, 249],
        &[228, 81, 220, 29, 208, 234, 27],
        &[116, 109, 134, 122, 15, 78, 126, 240, 158, 42, 221, 229, 93, 200, 194],
        &[180, 216, 189, 14, 82, 14, 170, 195, 196, 42, 177, 144, 153, 156, 140, 109, 93, 78, 157],
        &[190, 6, 59, 69, 208, 253, 2, 33, 86],
        &[245, 168, 144, 122, 243, 111],
        &[123, 150, 249, 114, 32, 140, 186, 204, 199, 8, 205, 150, 34, 104, 186, 236],
        &[8, 29, 191, 189, 72, 101, 39, 24, 105, 44, 13, 87, 75, 187],
        &[14, 201, 29, 151, 113, 10, 175],
        &[83, 130, 247, 5, 250, 101, 141, 5, 42, 132, 205, 3, 118, 152, 33, 219, 1, 91, 204],
        &[207, 215, 38, 17, 244, 96],
        &[34, 132, 138, 222, 250, 162, 231, 68, 142, 162, 152, 172, 244, 102, 179, 111, 161, 95],
        &[124, 120, 11, 4, 219, 210, 172, 50, 182, 160, 86, 88, 136, 122, 97, 98],
        &[86, 74, 181, 17, 3, 173, 12],
        &[18, 234, 66, 134, 20],
        &[20, 24, 83, 219, 209, 20, 236, 128, 155, 15, 110, 54, 237, 105, 186, 62],
        &[67, 11, 50, 124, 120, 33, 218],
        &[89, 248, 169, 97, 245, 98, 230, 53, 114, 198, 227, 148, 22, 127, 198, 153, 238, 59, 223],
        &[100, 128, 38, 54, 171, 186, 9, 133, 191, 82, 113, 86, 10, 72, 236, 124, 201, 65],
        &[152, 115, 99, 124, 81, 254, 0, 179, 24, 87, 24, 77, 60],
        &[107, 117, 222, 38, 162, 193, 48, 44, 140, 162, 104, 139, 90],
        &[63, 29, 217, 85, 63, 130, 110, 121, 227, 43, 215, 223, 249, 1, 72, 134, 92, 188],
        &[117, 3, 144, 15, 103, 113, 130, 253, 0, 102, 47, 24, 234, 0, 159],
        &[38, 60, 197, 120, 53, 94, 202, 137, 116, 27, 12, 181],
        &[248, 41, 252, 254, 98, 173, 42, 92, 30, 65, 72],
        &[240, 147, 89, 110, 224, 8],
        &[199, 86, 108, 195, 62, 169, 61],
        &[93, 225, 21, 185, 91, 23, 19, 7, 108, 176, 191, 91],
        &[70, 10, 122, 77, 171],
        &[32, 161, 24, 162, 112, 152, 21, 226, 149, 253, 212, 246, 175, 182],
        &[99, 7, 213, 87, 192, 2, 110, 242, 222, 89, 20, 83, 138, 112],
        &[92, 64, 61, 35, 111, 41, 151, 121, 24, 157],
        &[115, 201, 114, 124, 135, 246, 93, 230, 210, 164, 213, 254, 108, 181, 77, 19, 103, 166],
        &[26, 231, 59, 238, 246],
        &[52, 74, 93, 202, 140, 11, 56, 46, 211, 194, 137, 65, 36, 90, 209],
        &[56, 245, 179, 40, 190, 168, 116, 115],
        &[192, 215, 69, 171, 218, 187, 202, 120, 92, 33, 14, 77, 34, 46, 40, 93, 135, 117, 152],
    ];

    pub fn k_path_test4<'a, Z: ZipperIteration>(mut zipper: Z) {

        zipper.descend_first_k_path(5);
        let mut count = 1;
        while zipper.to_next_k_path(5) {
            count += 1;
        }
        assert_eq!(count, K_PATH_TEST4_KEYS.len());
    }

    /// This test triggers an edge-case because the first path is 15 bytes long, but
    /// `LineListNode::KEY_BYTES_CNT` is 14.  That means the path spills over to two nodes, 1 bytes
    /// before the end.  Then, we do `descend_first_k_path(2)`, meaning we end up straddling the
    /// node boundary, so `to_next_k_path(2)` needs to step back across to the parent node, and
    /// truncate the zipper's key, but not truncate too much
    pub const K_PATH_TEST5_KEYS: &[&[u8]] = &[
        &[3, 193, 4, 194, 1, 43, 3, 193, 8, 194, 1, 45, 194, 1, 46],
        &[3, 193, 4, 194, 1, 43, 3, 193, 34, 193],
    ];

    pub fn k_path_test5<'a, Z: ZipperIteration>(mut zipper: Z) {
        zipper.descend_to(&[3, 193, 4, 194, 1, 43, 3, 193, 8, 194, 1, 45, 194]);
        assert!(zipper.path_exists());
        assert_eq!(zipper.descend_first_k_path(2), true);
        assert_eq!(zipper.path(), &[3, 193, 4, 194, 1, 43, 3, 193, 8, 194, 1, 45, 194, 1, 46]);
        assert_eq!(zipper.to_next_k_path(2), false);
        assert_eq!(zipper.path(), &[3, 193, 4, 194, 1, 43, 3, 193, 8, 194, 1, 45, 194]);
    }

    pub const K_PATH_TEST6_KEYS: &[&[u8]] = &[
        &[2, 197, 97, 120, 105, 111, 109, 3, 193, 61, 4, 193, 97, 192, 192, 3, 193, 75, 192, 3, 193, 84, 192, 3, 193, 75, 128, 131, 193, 49],
        &[2, 197, 97, 120, 105, 111, 109, 3, 193, 61, 4, 193, 97, 192, 192, 3, 193, 84, 3, 193, 75, 192, 192, 3, 193, 75, 128, 131, 193, 49],
    ];

    /// This tests the k_path methods in the context of using them recursively, to shake out
    /// bugs caused by invalidating the iter token
    pub fn k_path_test6<'a, Z: ZipperIteration>(mut zipper: Z) {

        fn test_loop<'a, Z: ZipperMoving + ZipperIteration, AscendF: Fn(&mut Z, usize), DescendF: Fn(&mut Z, &[u8])>(zipper: &mut Z, descend_f: DescendF, ascend_f: AscendF) {
            zipper.reset();

            //L0 descent
            descend_f(zipper, &[2, 197, 97, 120, 105, 111, 109, 3, 193, 61, 4, 193, 97, 192, 192, 3, 193]);
            assert!(zipper.descend_first_k_path(1));
            assert_eq!(zipper.path(), &[2, 197, 97, 120, 105, 111, 109, 3, 193, 61, 4, 193, 97, 192, 192, 3, 193, 75]);

            //L1 descent
            descend_f(zipper, &[192, 3, 193, 84, 192, 3, 193]);
            assert!(zipper.descend_first_k_path(1));
            assert_eq!(zipper.path(), &[2, 197, 97, 120, 105, 111, 109, 3, 193, 61, 4, 193, 97, 192, 192, 3, 193, 75, 192, 3, 193, 84, 192, 3, 193, 75]);

            //L2 descent
            descend_f(zipper, &[128, 131, 193]);
            assert!(zipper.descend_first_k_path(1));
            assert_eq!(zipper.path(), &[2, 197, 97, 120, 105, 111, 109, 3, 193, 61, 4, 193, 97, 192, 192, 3, 193, 75, 192, 3, 193, 84, 192, 3, 193, 75, 128, 131, 193, 49]);

            //L2 next and ascent
            assert!(!zipper.to_next_k_path(1));
            assert_eq!(zipper.path(), &[2, 197, 97, 120, 105, 111, 109, 3, 193, 61, 4, 193, 97, 192, 192, 3, 193, 75, 192, 3, 193, 84, 192, 3, 193, 75, 128, 131, 193]);

            ascend_f(zipper, 3);

            //L1 next and ascent
            assert!(!zipper.to_next_k_path(1));
            assert_eq!(zipper.path(), &[2, 197, 97, 120, 105, 111, 109, 3, 193, 61, 4, 193, 97, 192, 192, 3, 193, 75, 192, 3, 193, 84, 192, 3, 193]);

            ascend_f(zipper, 7);

            //L0 next
            assert!(zipper.to_next_k_path(1));
            assert_eq!(zipper.path(), &[2, 197, 97, 120, 105, 111, 109, 3, 193, 61, 4, 193, 97, 192, 192, 3, 193, 84]);

            ascend_f(zipper, 17);
        }

        //Try with a `descend_to` & `ascend`
        test_loop(&mut zipper,
            |zipper, path| {
                zipper.descend_to(path);
                assert!(zipper.path_exists());
            },
            |zipper, steps| assert!(zipper.ascend(steps)),
        );

        //Try with a `descend_to_byte` & `ascend_byte`
        test_loop(&mut zipper,
            |zipper, path| {
                for byte in path {
                    zipper.descend_to_byte(*byte);
                    assert!(zipper.path_exists());
                }
            },
            |zipper, steps| {
                for _ in 0..steps {
                    assert!(zipper.ascend_byte())
                }
            },
        );

        //Try with a `descend_first_byte` & `ascend_byte`
        test_loop(&mut zipper,
            |zipper, path| {
                for _ in 0..path.len() {
                    assert!(zipper.descend_first_byte());
                }
            },
            |zipper, steps| {
                for _ in 0..steps {
                    assert!(zipper.ascend_byte())
                }
            },
        );
    }

    pub const K_PATH_TEST7_KEYS: &[&[u8]] = &[
        &[2, 197, 97, 120, 105, 111, 109, 3, 193, 61, 4, 193, 97, 192, 192, 3, 193, 75, 192, 3, 193, 84, 192, 3, 193, 75, 128, 131, 193, 49],
        &[2, 197, 97, 120, 105, 111, 109, 3, 193, 61, 4, 193, 97, 192, 192, 3, 193, 84, 3, 193, 75, 192, 192, 3, 193, 75, 128, 131, 193, 49],
    ];

    /// This uses the k_path methods to descend and then re-ascend a trie, one step at a time.
    pub fn k_path_test7<'a, Z: ZipperIteration>(mut zipper: Z) {
        let keys = K_PATH_TEST7_KEYS;

        for i in 0..keys[0].len() {
            assert_eq!(zipper.path(), &keys[0][..i]);
            assert!(zipper.descend_first_k_path(1));
        }
        for i in (0..keys[0].len()).rev() {
            assert_eq!(zipper.path(), &keys[0][..=i]);
            if i != 17 {
                assert!(!zipper.to_next_k_path(1));
            } else {
                assert!(zipper.to_next_k_path(1));
                assert!(!zipper.to_next_k_path(1));
            }
        }
    }

    pub const K_PATH_TEST8_KEYS: &[&[u8]] = &[b"ABCDEFGHIJKLMNOPQRSTUVWXYZ", b"ab",];

    /// Tests `..k_path` after `descend_to_byte`
    pub fn k_path_test8<'a, Z: ZipperIteration>(mut zipper: Z) {

        zipper.reset();
        zipper.descend_to_byte(b'A');
        assert_eq!(zipper.path_exists(), true);
        assert_eq!(zipper.descend_first_k_path(1), true);
        assert_eq!(zipper.path(), b"AB");
        assert_eq!(zipper.to_next_k_path(1), false);
        assert_eq!(zipper.path(), b"A");
    }

    pub const K_PATH_TEST9_KEYS: &[&[u8]] = &[
        &[2, 194, 1, 1, 193, 5],
        &[3, 194, 1, 0, 193, 6, 193, 5],
        &[3, 193, 4, 193],
    ];

    /// Tests `..k_path` in a subtrie without attitional branches to descend, when the outer trie does have branches
    pub fn k_path_test9<'a, Z: ZipperIteration>(mut zipper: Z) {

        zipper.reset();
        assert_eq!(zipper.descend_first_k_path(1), true);
        assert_eq!(zipper.path(), &[1]);
        assert_eq!(zipper.to_next_k_path(1), false);
        assert_eq!(zipper.path(), &[]);
    }
}

#[cfg(all(test, feature = "pathmap-internal-tests"))]
mod tests {
    use super::{alloc::global_alloc, PathMap};
    use super::*;

    super::zipper_moving_tests::zipper_moving_tests!(read_zipper,
        |keys: &[&[u8]]| {
            let mut btm = PathMap::new();
            keys.iter().for_each(|k| { btm.set_val_at(k, ()); });
            btm
        },
        |btm: &mut PathMap<()>, path: &[u8]| -> ReadZipperUntracked<()> {
            btm.read_zipper_at_path(path)
    });

    super::zipper_iteration_tests::zipper_iteration_tests!(read_zipper,
        |keys: &[&[u8]]| {
            let mut btm = PathMap::new();
            keys.iter().for_each(|k| { btm.set_val_at(k, ()); });
            btm
        },
        |btm: &mut PathMap<()>, path: &[u8]| -> ReadZipperUntracked<()> {
            btm.read_zipper_at_path(path)
    });

    super::zipper_moving_tests::zipper_moving_tests!(read_zipper_owned,
        |keys: &[&[u8]]| {
            let mut btm = PathMap::new();
            keys.iter().for_each(|k| { btm.set_val_at(k, ()); });
            btm
        },
        |btm: &mut PathMap<()>, path: &[u8]| -> ReadZipperOwned<()> {
            core::mem::take(btm).into_read_zipper(path)
    });

    super::zipper_iteration_tests::zipper_iteration_tests!(read_zipper_owned,
        |keys: &[&[u8]]| {
            let mut btm = PathMap::new();
            keys.iter().for_each(|k| { btm.set_val_at(k, ()); });
            btm
        },
        |btm: &mut PathMap<()>, path: &[u8]| -> ReadZipperOwned<()> {
            core::mem::take(btm).into_read_zipper(path)
    });

    /// Tests the integrity of values accessed through [ZipperReadOnlyValues::get_val]
    #[test]
    fn zipper_value_access() {
        let mut btm = PathMap::new();
        let rs = ["arrow", "bow", "cannon", "roman", "romane", "romanus", "romulus", "rubens", "ruber", "rubicon", "rubicundus", "rom'i"];
        rs.iter().for_each(|r| { btm.set_val_at(r.as_bytes(), *r); });

        let root_key = b"ro";
        let mut zipper = ReadZipperCore::new_with_node_and_path_in(btm.root().unwrap(), false, root_key, root_key.len(), 0, None, global_alloc());
        assert_eq!(zipper.is_val(), false);
        zipper.descend_to(b"mulus");
        assert_eq!(zipper.is_val(), true);
        assert_eq!(zipper.val(), Some(&"romulus"));

        let root_key = b"roman";
        let mut zipper = ReadZipperCore::new_with_node_and_path_in(btm.root().unwrap(), false, root_key, root_key.len(), 0, None, global_alloc());
        assert_eq!(zipper.is_val(), true);
        assert_eq!(zipper.val(), Some(&"roman"));
        zipper.descend_to(b"e");
        assert_eq!(zipper.is_val(), true);
        assert_eq!(zipper.val(), Some(&"romane"));
        assert_eq!(zipper.ascend(1), true);
        zipper.descend_to(b"u");
        assert_eq!(zipper.is_val(), false);
        assert_eq!(zipper.val(), None);
        zipper.descend_until();
        assert_eq!(zipper.is_val(), true);
        assert_eq!(zipper.val(), Some(&"romanus"));
    }

    /// Tests that a zipper forked at a subtrie will iterate correctly within that subtrie, also tests ReadZipper::IntoIterator impl
    #[test]
    fn read_zipper_special_iter_test1() {
        let mut btm = PathMap::new();
        let rs = ["arrow", "bow", "cannon", "roman", "romane", "romanus", "romulus", "rubens", "ruber", "rubicon", "rubicundus", "rom'i"];
        rs.iter().enumerate().for_each(|(i, r)| { btm.set_val_at(r.as_bytes(), i); });
        let mut zipper = btm.read_zipper();

        //Fork a sub-zipper, and test iteration of that subtree
        zipper.descend_to(b"rub");
        let mut sub_zipper = zipper.fork_read_zipper();
        while let Some(&val) = sub_zipper.to_next_get_val() {
            // println!("{val}  {} = {}", std::str::from_utf8(sub_zipper.path()).unwrap(), std::str::from_utf8(&rs[val].as_bytes()[3..]).unwrap());
            assert_eq!(&rs[val].as_bytes()[3..], sub_zipper.path());
        }
        drop(sub_zipper);

        for (path, &val) in zipper {
            // println!("{val}  {} = {}", std::str::from_utf8(&path).unwrap(), std::str::from_utf8(rs[val].as_bytes()).unwrap());
            assert_eq!(rs[val].as_bytes(), path);
        }
    }

    /// Tests that `to_next_val` will behave correctly when the map contains dangling paths, such as those created by zipper heads
    #[test]
    fn read_zipper_special_iter_test2() {
        //This tests iteration over an empty map, with no activity at all
        let mut map = PathMap::<u64>::new();

        let mut zipper = map.read_zipper();
        assert_eq!(zipper.to_next_val(), false);
        assert_eq!(zipper.to_next_val(), false);
        drop(zipper);

        //Now test some operations that create nodes, but not values
        let map_head = map.zipper_head();
        let _wz = map_head.write_zipper_at_exclusive_path(b"0");
        drop(_wz);
        drop(map_head);

        let mut zipper = map.read_zipper();
        assert_eq!(zipper.to_next_val(), false);
        assert_eq!(zipper.to_next_val(), false);
    }

    /// Tests iterating a subtrie created by a descended WriteZipper
    #[test]
    fn read_zipper_special_iter_test3() {
        const N: usize = 32;

        let mut map = PathMap::<usize>::new();
        let mut zipper = map.write_zipper_at_path(b"in");
        for i in 0usize..N {
            zipper.descend_to(i.to_be_bytes());
            zipper.set_val(i);
            zipper.reset();
        }
        drop(zipper);

        //Test iterating using a ReadZipper that has a root that is not the map root
        let mut reader_z = map.read_zipper_at_path(b"in");
        assert_eq!(reader_z.val_count(), N);
        let mut count = 0;
        while let Some(val) = reader_z.to_next_get_val() {
            assert_eq!(reader_z.get_val(), Some(val));
            assert_eq!(reader_z.get_val(), Some(&count));
            assert_eq!(reader_z.path(), count.to_be_bytes());
            count += 1;
        }
        assert_eq!(count, N);
    }

    /// Test val_count & iteration on a ReadZipper forked off of a WriteZipper
    #[test]
    fn read_zipper_special_iter_test4() {
        const R_KEY_CNT: usize = 9;
        let keys = ["arrow", "bow", "cannon", "roman", "romane", "romanus", "romulus", "rubens", "ruber", "rubicon", "rubicundus", "rom'i"];
        let mut map: PathMap::<()> = keys.into_iter().map(|k| (k, ())).collect();

        // Test val_count & iteration on a ReadZipper
        let rz = map.read_zipper_at_borrowed_path(b"r");
        assert_eq!(rz.val_count(), R_KEY_CNT);
        let mut count = 0;
        for (_path, _) in rz {
            count += 1;
        }
        assert_eq!(count, R_KEY_CNT);

        // Test val_count & iteration on a ReadZipper forked off of a WriteZipper
        let wz = map.write_zipper_at_path(b"r");
        assert_eq!(wz.val_count(), R_KEY_CNT);
        let rz = wz.fork_read_zipper();
        assert_eq!(rz.val_count(), R_KEY_CNT);
        let mut count = 0;
        for (_path, _) in rz {
            count += 1;
        }
        assert_eq!(count, R_KEY_CNT);
    }

    #[test]
    fn read_zipper_path_iter_leaf_paths() {
        pub const ZIPPER_UNIQUE_PATH_KEYS: &[&[u8]] = &[b"arrow", b"bow", b"cannon", b"rom'i", b"romane", b"romanus", b"romulus", b"rubens", b"ruber", b"rubicon", b"rubicundus"];
        let mut map: PathMap<()> = PathMap::new();
        for key in ZIPPER_UNIQUE_PATH_KEYS {
            map.create_path(key);
        }

        let iter_paths: Vec<Vec<u8>> = map.read_zipper().into_path_iter().collect();
        assert_eq!(iter_paths, ZIPPER_UNIQUE_PATH_KEYS);
    }

    /// This test tries to hit an edge case in [ZipperIteration::to_next_val] where we begin iteration in the middle of a node
    #[test]
    fn read_zipper_special_zipper_iter_test5() {
        let mut map = PathMap::<usize>::new();
        let mut zipper = map.write_zipper_at_path(b"in");
        for i in 0usize..2 {
            zipper.descend_to_byte(i as u8);
            zipper.descend_to(i.to_be_bytes());
            zipper.set_val(i);
            zipper.reset();
        }
        drop(zipper);

        let mut reader_z = map.read_zipper_at_path([b'i', b'n', 1]);
        let mut sanity_counter = 0;
        while reader_z.to_next_val() {
            sanity_counter += 1;
        }
        assert_eq!(sanity_counter, 1);
    }

    #[test]
    fn read_zipper_special_byte_iter_test() {
        let keys = vec![[0, 3], [0, 4], [0, 5]];
        let map: PathMap<()> = keys.into_iter().map(|v| (v, ())).collect();

        let mut r0 = map.read_zipper();
        r0.descend_to_byte(0);
        assert_eq!(r0.path_exists(), true);
        let mut r1 = r0.fork_read_zipper();
        assert_eq!(r1.to_next_sibling_byte(), false);
        assert_eq!(r1.child_mask().0[0], (1<<3) | (1<<4) | (1<<5));
        r1.descend_to_byte(3);
        assert_eq!(r1.path_exists(), true);
        assert_eq!(r1.child_mask().0[0], 0);
        assert_eq!(r1.to_next_sibling_byte(), true);
        assert_eq!(r1.origin_path(), &[0, 4]);
        assert_eq!(r1.path(), &[4]);
        assert_eq!(r1.to_next_sibling_byte(), true);
        assert_eq!(r1.to_next_sibling_byte(), false);
    }

    #[test]
    fn path_concat_test() {
        let parent_path = "�parent";
        let exprs = [
            "�parent�Tom�Bob",
            "�parent�Pam�Bob",
            "�parent�Tom�Liz",
            "�parent�Bob�Ann",
            "�parent�Bob�Pat",
            "�parent�Pat�Jim",
            "�female�Pam",
            "�male�Tom",
            "�male�Bob",
            "�female�Liz",
            "�female�Pat",
            "�female�Ann",
            "�male�Jim",
        ];
        let family: PathMap<i32> = exprs.iter().enumerate().map(|(i, k)| (k, i as i32)).collect();

        let mut parent_zipper = family.read_zipper_at_path(parent_path.as_bytes());

        assert!(family.path_exists_at(parent_path));

        let mut full_parent_path = parent_path.as_bytes().to_vec();
        full_parent_path.extend(parent_zipper.path());
        assert!(family.path_exists_at(&full_parent_path));

        while parent_zipper.to_next_val() {
            let mut full_parent_path = parent_path.as_bytes().to_vec();
            full_parent_path.extend(parent_zipper.path());
            assert!(family.contains(&full_parent_path));
            assert_eq!(full_parent_path, parent_zipper.origin_path());
        }
    }

    #[test]
    fn full_path_test() {
        let rs = ["arrow", "bow", "cannon", "roman", "romane", "romanus", "romulus", "rubens", "ruber", "rubicon", "rubicundus", "rom'i"];
        let btm: PathMap<u64> = rs.into_iter().enumerate().map(|(i, k)| (k, i as u64)).collect();

        let mut zipper = btm.read_zipper_at_path(b"roma");
        assert_eq!(format!("roma{}", std::str::from_utf8(zipper.path()).unwrap()), "roma");
        assert_eq!(std::str::from_utf8(zipper.origin_path()).unwrap(), "roma");
        zipper.descend_to(b"n");
        assert_eq!(format!("roma{}", std::str::from_utf8(zipper.path()).unwrap()), "roman");
        assert_eq!(std::str::from_utf8(zipper.origin_path()).unwrap(), "roman");
        zipper.to_next_val();
        assert_eq!(format!("roma{}", std::str::from_utf8(zipper.path()).unwrap()), "romane");
        assert_eq!(std::str::from_utf8(zipper.origin_path()).unwrap(), "romane");
        zipper.to_next_val();
        assert_eq!(format!("roma{}", std::str::from_utf8(zipper.path()).unwrap()), "romanus");
        assert_eq!(std::str::from_utf8(zipper.origin_path()).unwrap(), "romanus");
        zipper.to_next_val();
        assert_eq!(zipper.path().len(), 0);
    }

    #[test]
    fn read_zipper_is_shared_test1() {
        let l0_keys = vec!["stem0", "stem1", "stem2", "strongbad", "strange", "steam", "stevador", "steeple"];
        let l1_keys = vec!["A-mid0", "B-mid1", "C-mid2", "D-midlands", "D-middling", "D-middlemarch"];
        let l2_keys = vec!["X-top0", "X-top1", "X-top2", "X-top3"];
        let top_map: PathMap<()> = l2_keys.iter().map(|v| (v, ())).collect();

        let mut mid_map = PathMap::<()>::new();
        let mut wz = mid_map.write_zipper();
        for key in l1_keys.iter() {
            wz.reset();
            wz.descend_to(key);
            wz.graft_map(top_map.clone());
        }
        drop(wz);

        let mut map = PathMap::<()>::new();
        let mut wz = map.write_zipper();
        for key in l0_keys.iter() {
            wz.reset();
            wz.descend_to(key);
            wz.graft_map(mid_map.clone());
        }
        drop(wz);

        assert_eq!(map.val_count(), l0_keys.len() * l1_keys.len() * l2_keys.len());

        let mut rz = map.read_zipper();
        let mut shared_cnt = 0;
        while rz.to_next_step() {
            if rz.is_shared() {
                // println!("{}", String::from_utf8_lossy(rz.path()));
                shared_cnt += 1;
            }
        }
        assert_eq!(shared_cnt, l0_keys.len() + l0_keys.len() * l1_keys.len());
    }

    /// This behavior is a bit counter-intuitive, but it is correct.
    /// The following happens:
    /// 1. The top_map ["X0", "X1", "X2"] is grafted at "steam" creating ["steamX0",
    ///   "steamX1", steamX2].  At this point, the base of ["X0", "X1", "X2"]
    ///   is shared, because it is shared with the top_map.
    /// 2. The write zipper descends to "steamboat", and modifies it making it unique.
    ///   So the path "steam" now has 2 children, 'X' and 'b', so it's unique
    /// 3. In the process of making the "steam" path writable, and thus unique, the
    ///   "X" in ["X0", "X1", "X2"] becomes the new share point.  So there are 3 share
    ///   points in the trie:
    ///   - "steamX" - Shared twice in the `map` and also one level deep within `top_map`
    ///   - "steamboat" - Sharing the root node of `top_map`
    ///   - "steamboatX" - Sharing the same nodes as "steamX", etc.
    #[test]
    fn read_zipper_is_shared_test2() {
        let l0_keys = vec!["steam", "steamboat"];
        let l1_keys = vec!["X0", "X1", "X2"];
        let top_map: PathMap<()> = l1_keys.iter().map(|v| (v, ())).collect();

        let mut map = PathMap::<()>::new();
        let mut wz = map.write_zipper();
        for key in l0_keys.iter() {
            wz.reset();
            wz.descend_to(key);
            wz.graft_map(top_map.clone());
        }
        drop(wz);

        assert_eq!(map.val_count(), l0_keys.len() * l1_keys.len());

        let mut rz = map.read_zipper();
        let mut shared_cnt = 0;
        while rz.to_next_step() {
            if rz.is_shared() {
                // println!("{}", String::from_utf8_lossy(rz.path()));
                shared_cnt += 1;
            }
        }
        assert_eq!(shared_cnt, 3);
    }

    /// Tests [`ZipperPriv::get_focus`] and [`ZipperPriv::try_borrow_focus`] internal APIs on [`ReadZipperCore`]
    #[test]
    fn read_zipper_focus_nodes() {
        let mut map = PathMap::<()>::new();
        map.set_val_at(b"one.val", ());
        map.set_val_at(b"one.two.val", ());
        map.set_val_at(b"one.two.three.val", ());
        map.set_val_at(b"one.two.three.four.val", ());

        //Test descending a read zipper
        let mut rz = map.read_zipper();
        rz.descend_to(b"one.");

        //We should be at a node boundary, as long as this part of the path got encoded as a PairNode (or a ByteNode)
        let node = rz.try_borrow_focus().unwrap();
        assert_eq!(node.as_tagged().count_branches(&[]), 2);
        let expected_mask: ByteMask = [b't', b'v'].into_iter().collect();
        assert_eq!(node.as_tagged().node_branches_mask(&[]), expected_mask);
        assert!(matches!(rz.get_focus(), AbstractNodeRef::BorrowedRc(_))); //Make sure we get the ODRc
        drop(rz);

        //Test creating a read zipper at the location, using `read_zipper_at_path`
        let rz = map.read_zipper_at_borrowed_path(b"one.two.");

        //We should be at a node boundary, as long as this part of the path got encoded as a PairNode (or a ByteNode)
        let node = rz.try_borrow_focus().unwrap();
        assert_eq!(node.as_tagged().count_branches(&[]), 2);
        let expected_mask: ByteMask = [b't', b'v'].into_iter().collect();
        assert_eq!(node.as_tagged().node_branches_mask(&[]), expected_mask);
        assert!(matches!(rz.get_focus(), AbstractNodeRef::BorrowedRc(_))); //Make sure we get the ODRc
        drop(rz);

        //Test creating a read zipper from a ZipperHead
        let zh = map.zipper_head();
        let rz = zh.read_zipper_at_borrowed_path(b"one.two.three.").unwrap();

        // We should be at a node boundary, as long as this part of the path got encoded as a PairNode (or a ByteNode)
        let node = rz.try_borrow_focus().unwrap();
        assert_eq!(node.as_tagged().count_branches(&[]), 2);
        let expected_mask: ByteMask = [b'f', b'v'].into_iter().collect();
        assert_eq!(node.as_tagged().node_branches_mask(&[]), expected_mask);
        assert!(matches!(rz.get_focus(), AbstractNodeRef::BorrowedRc(_))); //Make sure we get the ODRc
    }

    /// Tests the zipper `val_count` method, including with root values
    /// NOTE: This test isn't in the generic suite because not all zipper types have real implementations of val_count
    const ZIPPER_VAL_COUNT_TEST1_KEYS: &[&[u8]] = &[b"", b"arrow"];
    #[test]
    fn read_zipper_val_count_test1() {
        let map: PathMap<()> = ZIPPER_VAL_COUNT_TEST1_KEYS.into_iter().cloned().collect();
        let mut zipper = map.read_zipper();

        assert_eq!(zipper.path(), b"");
        assert_eq!(zipper.val_count(), 2);
        assert_eq!(zipper.descend_until(), true);
        assert_eq!(zipper.path(), b"arrow");
        assert_eq!(zipper.val_count(), 1);
    }

    #[test]
    fn descend_last_path() {
        let rs = ["arrow", "bow", "cannon", "roman", "romane", "romanus", "romulus", "rubens", "ruber", "rubicon", "rubicundus", "rom'i"];
        let btm: PathMap<u64> = rs.into_iter().enumerate().map(|(i, k)| (k, i as u64)).collect();

        let mut rz = btm.read_zipper();
        assert!(rz.descend_last_path());
        assert_eq!(rz.path(), b"rubicundus");
    }
}
