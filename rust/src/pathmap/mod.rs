
#[cfg(not(any(miri, target_arch = "riscv64")))]
use gxhash::{HashMap, HashMapExt, HashSetExt};

#[cfg(not(any(miri, target_arch = "riscv64")))]
pub(crate) mod gxhash {
    pub use ::gxhash::*;
}

#[cfg(any(miri, target_arch="riscv64"))]
pub(crate) mod gxhash {
    // fallback
    // pub use xxhash_rust::xxh64::{Xxh64 as GxHasher};
    /// Just a simple XOR hasher so miri doesn't explode on all the tests that use GxHash
    #[derive(Clone, Default)]
    pub struct GxHasher { state_lo: u64, state_hi: u64, }
    impl GxHasher {
      pub fn with_seed(seed: i64) -> Self {
        //Reinterpret the bits without any kind of rounding, truncation, extension, etc.
        let seed = u64::from_ne_bytes(seed.to_ne_bytes());
        Self { state_lo: seed ^ 0xA5A5A5A5_A5A5A5A5, state_hi: !seed ^ 0x5A5A5A5A_5A5A5A5A, }
      }
      pub fn finish_u128(&self) -> u128 {
        ((self.state_hi as u128) << 64) | self.state_lo as u128
      }
    }
    impl core::hash::Hasher for GxHasher {
      fn write(&mut self, buf: &[u8]) {
        for &c in buf {
            self.write_u8(c);
        }
      }
      fn write_u8(&mut self, i: u8) {
        self.state_lo = self.state_lo.wrapping_add(i as u64);
        self.state_hi ^= (i as u64).rotate_left(11);
        self.state_lo = self.state_lo.rotate_left(3);
      }
      fn write_u128(&mut self, i: u128) {
        let low = i as u64;
        let high = (i >> 64) as u64;
        self.state_lo = self.state_lo.wrapping_add(low);
        self.state_hi ^= high.rotate_left(17);
        self.state_lo ^= high.rotate_left(9);
      }
      fn finish(&self) -> u64 {
        self.finish_u128() as u64
      }
    }

    pub use std::collections::HashMap;
    pub fn gxhash128(data: &[u8], _seed: i64) -> u128 { xxhash_rust::const_xxh3::xxh3_128(data) }
    pub trait HashMapExt{}
    pub trait HashSetExt{}
}


/// Traits to implement [ring](https://en.wikipedia.org/wiki/Ring_(mathematics)) and other algebraic
/// operations on tries, such as union, intersection, and subtraction
pub mod ring;

/// A collection indexed by paths of bytes, supporting [algebraic](super::ring) operations
mod trie_map;
pub use trie_map::PathMap;

/// Cursors that can move over a trie, to inspect and modify contained elements or entire branches
#[macro_use]
pub mod zipper;

/// Functionality for applying various morphisms to [PathMap] and [Zipper](pathmap::zipper::Zipper)s
pub mod morphisms;

/// Functionality to optimize a trie by finding structural sharing using a temporary [Merkle tree](https://en.wikipedia.org/wiki/Merkle_tree)
pub mod merkleization;

/// Handy conveniences and utilities to use with a [PathMap]
pub mod utils;

/// Extensions to the API that may or may not become permanant
pub mod experimental;

/// Compact representation of the trie
#[cfg(any(feature = "arena_compact", feature = "mork"))]
pub mod arena_compact;

/// Track outstanding zippers to be sure they don't conflict
#[cfg(feature = "zipper_tracking")]
pub mod zipper_tracking;

/// Track outstanding zippers to be sure they don't conflict
#[cfg(not(feature = "zipper_tracking"))]
mod zipper_tracking;

/// Only includes PolyZipper tests.  The real implementation is in the `pathmap-derive` crate
mod poly_zipper;

/// Used to create multiple simultaneous zippers from the same parent
mod zipper_head;

/// Used for creating random paths and tries, according to configurable distributions
#[cfg(feature = "random")]
pub mod random;

/// Features to inspect performance properties of trees, for optimizing
#[cfg(feature = "counters")]
pub mod counters;

/// Shims to allow the use of a custom [`Allocator`](std::alloc::Allocator) type, if running with the `nightly` feature.  Does nothing otherwise
pub mod alloc;

/// Raw trie visualization
#[cfg(feature = "viz")]
pub mod viz;

#[cfg(any(feature = "serialization", feature = "mork"))]
pub mod paths_serialization;

mod trie_node;
mod write_zipper;
mod product_zipper;
mod empty_zipper;
mod prefix_zipper;
mod overlay_zipper;
mod dependent_zipper;
mod trie_ref;
mod dense_byte_node;
pub(crate) mod line_list_node;
mod empty_node;
mod tiny_node;
#[cfg(feature = "bridge_nodes")]
mod bridge_node;

#[cfg(feature = "old_cursor")]
mod old_cursor;

/// A supertrait that encapsulates the bounds for a value that can be put in a [PathMap]
pub trait TrieValue: Clone + Send + Sync + Unpin {}

impl<T> TrieValue for T where T : Clone + Send + Sync + Unpin {}

/// Internal macro to implement Debug on a type that just outputs the type name
macro_rules! impl_name_only_debug {
    (impl $($impl_tail:tt)*) => {
        impl $($impl_tail)* {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                f.debug_struct(core::any::type_name::<Self>()).finish()
            }
        }
    };
}
pub(crate) use impl_name_only_debug;

#[cfg(all(test, feature = "pathmap-internal-tests"))]
mod tests {
    use rand::{Rng, SeedableRng, rngs::StdRng};
    use petta::pathmap::ring::*;
    use petta::pathmap::PathMap;
    use petta::pathmap::zipper::*;

    pub(crate) fn prefix_key(k: &u64) -> &[u8] {
        let bs = (8 - k.leading_zeros()/8) as u8;
        let kp: *const u64 = k;
        unsafe { std::slice::from_raw_parts(kp as *const _, (bs as usize).max(1)) }
    }

    pub(crate) fn from_prefix_key(k: Vec<u8>) -> u64 {
        let mut buf = [0u8; 8];
        unsafe { core::ptr::copy_nonoverlapping(k.as_ptr(), buf.as_mut_ptr(), k.len()); }
        let shift = 64usize.saturating_sub(k.len()*8);
        u64::from_le_bytes(buf) & (!0u64 >> shift)
    }

    #[test]
    fn btm_value_only_subtract_test() {
        let mut l: PathMap<u64> = PathMap::new();
        l.set_val_at(b"0", 0);
        l.set_val_at(b"1", 1);
        l.set_val_at(b"2", 2);
        let mut r: PathMap<u64> = PathMap::new();
        r.set_val_at(b"1", 1);
        r.set_val_at(b"3", 3);
        let l_no_r = l.subtract(&r);
        assert_eq!(l_no_r.get_val_at(b"0"), Some(&0));
        assert_eq!(l_no_r.get_val_at(b"1"), None);
        assert_eq!(l_no_r.get_val_at(b"2"), Some(&2));
        assert_eq!(l_no_r.get_val_at(b"3"), None);
    }

    #[test]
    fn btm_compound_tree_subtract_test() {
        let mut l: PathMap<bool> = PathMap::new();
        l.set_val_at(b"hello", true);
        l.set_val_at(b"hello world", true);
        l.set_val_at(b"hell no we won't go", true);
        let mut r: PathMap<bool> = PathMap::new();
        r.set_val_at(b"hello", true);
        let l_no_r = l.subtract(&r);

        assert_eq!(l_no_r.get_val_at(b"hello"), None);
        assert_eq!(l_no_r.get_val_at(b"hello world"), Some(&true));
        assert_eq!(l_no_r.get_val_at(b"hell no we won't go"), Some(&true));
    }

    #[test]
    fn btm_simple_tree_subtract_test() {
        let mut l: PathMap<bool> = PathMap::new();
        l.set_val_at(b"alligator", true);
        l.set_val_at(b"allegedly", true);
        l.set_val_at(b"albatross", true);
        l.set_val_at(b"albino", true);
        let mut r: PathMap<bool> = PathMap::new();
        r.set_val_at(b"alligator", true);
        r.set_val_at(b"albino", true);
        let l_no_r = l.subtract(&r);

        assert_eq!(l_no_r.val_count(), 2);
        assert_eq!(l_no_r.get_val_at(b"alligator"), None);
        assert_eq!(l_no_r.get_val_at(b"albino"), None);
        assert_eq!(l_no_r.get_val_at(b"allegedly"), Some(&true));
        assert_eq!(l_no_r.get_val_at(b"albatross"), Some(&true));
    }

    #[test]
    fn btm_many_elements_subtract_test() {
        #[cfg(miri)]
        const N: u64 = 20;
        #[cfg(all(feature = "pathmap-internal-tests", not(miri)))]
        const N: u64 = 1000;

        let overlap = 0.5;
        let o = ((1. - overlap) * N as f64) as u64;

        let mut vnl = PathMap::new();
        let mut vnr = PathMap::new();
        for i in 0..N { vnl.set_val_at(prefix_key(&i), i); }
        for i in o..(N+o) { vnr.set_val_at(prefix_key(&i), i); }
        let l_no_r = vnl.subtract(&vnr);

        //Validate the ByteTrieMap::subtract against HashSet::difference
        let vnl_set: std::collections::HashSet<Vec<u8>> = vnl.iter().map(|(k, _)| k).collect();
        let vnr_set: std::collections::HashSet<Vec<u8>> = vnr.iter().map(|(k, _)| k).collect();
        let mut l_no_r_set: Vec<Vec<u8>> = l_no_r.iter().map(|(k, _)| k).collect();
        let mut l_no_r_ref_set: Vec<Vec<u8>> = vnl_set.difference(&vnr_set).cloned().collect();
        l_no_r_set.sort();
        l_no_r_ref_set.sort();
        assert_eq!(l_no_r_set, l_no_r_ref_set);
    }

    #[test]
    fn btm_subtract_after_join() {

        //This entire operation works with only ListNodes
        let r: Vec<Vec<u8>> = vec![
            vec![61, 85, 161, 68, 245, 90, 129],
            vec![70, 91, 37, 155, 181, 227, 100, 255, 66, 129, 158, 241, 183, 96, 59],
        ];
        let r: PathMap<u64> = r.into_iter().map(|v| (v, 0)).collect();

        let l: Vec<Vec<u8>> = vec![
            vec![70, 116, 109, 134, 122, 15, 78, 126, 240, 158, 42, 221],
        ];
        let l: PathMap<u64> = l.into_iter().map(|v| (v, 0)).collect();

        let joined = l.join(&r);
        let remaining = joined.subtract(&r);
        let remaining_keys: Vec<Vec<u8>> = remaining.iter().map(|(k, _v)| k).collect();
        let l_keys: Vec<Vec<u8>> = l.iter().map(|(k, _v)| k).collect();

        assert_eq!(remaining.val_count(), l.val_count());
        for (rem_k, l_k) in remaining_keys.iter().zip(l_keys.iter()) {
            assert_eq!(rem_k, l_k);
        }

        //This ends up upgrading a node to a dense node, So we test those paths here
        let r: Vec<Vec<u8>> = vec![
            vec![61, 85, 161, 68, 245, 90, 129],
            vec![70, 10, 122, 77, 171, 54, 32, 161, 24, 162, 112, 152],
            vec![70, 91, 37, 155, 181, 227, 100, 255, 66, 129, 158, 241, 183, 96, 59],
        ];
        let r: PathMap<u64> = r.into_iter().map(|v| (v, 0)).collect();

        let l: Vec<Vec<u8>> = vec![
            vec![70, 116, 109, 134, 122, 15, 78, 126, 240, 158, 42, 221],
        ];
        let l: PathMap<u64> = l.into_iter().map(|v| (v, 0)).collect();

        let joined = l.join(&r);
        let remaining = joined.subtract(&r);
        let remaining_keys: Vec<Vec<u8>> = remaining.iter().map(|(k, _v)| k).collect();
        let l_keys: Vec<Vec<u8>> = l.iter().map(|(k, _v)| k).collect();

        assert_eq!(remaining.val_count(), l.val_count());
        for (rem_k, l_k) in remaining_keys.iter().zip(l_keys.iter()) {
            assert_eq!(rem_k, l_k);
        }
    }

    #[test]
    fn btm_subtract_after_join_2() {
        #[cfg(miri)]
        const N: u64 = 10;
        #[cfg(all(feature = "pathmap-internal-tests", not(miri)))]
        const N: u64 = 500;

        let mut rng = StdRng::seed_from_u64(1);
        let keys: Vec<Vec<u8>> = (0..N).into_iter().map(|_| {
            let len = (rng.random::<u8>() % 18) + 3; //length between 3 and 20 chars
            (0..len).into_iter().map(|_| rng.random::<u8>()).collect()
        }).collect();

        let mut l: PathMap<u64> = PathMap::new();
        for i in 0..(N/2) { l.set_val_at(&keys[i as usize], i); }
        let mut r: PathMap<u64> = PathMap::new();
        for i in (N/2)..N { r.set_val_at(&keys[i as usize], i); }

        let joined = l.join(&r);
        let remaining = joined.subtract(&r);
        assert_eq!(remaining.val_count(), l.val_count())
    }

    #[test]
    fn btm_join_dangling_branching_factor_test() {
        // Left has a single downstream dangling branch
        let mut left: PathMap<u64> = PathMap::new();
        left.create_path([9u8, 40u8, 1u8]);

        // Right fans out the same prefix with three branches (mix of dangling and valued)
        let mut right: PathMap<u64> = PathMap::new();
        right.create_path([9u8, 10u8]); // dangling branch only
        right.set_val_at([9u8, 11u8], 11);
        right.set_val_at([9u8, 12u8, 0u8], 12);

        let joined = left.join(&right);

        assert!(joined.path_exists_at([9u8, 40u8, 1u8]));
        assert!(joined.path_exists_at([9u8, 10u8]));
        assert_eq!(joined.get_val_at([9u8, 10u8]), None);
        assert_eq!(joined.get_val_at([9u8, 11u8]), Some(&11));
        assert_eq!(joined.get_val_at([9u8, 12u8, 0u8]), Some(&12));

        let mut rz = joined.read_zipper();
        rz.descend_to([9u8]);
        assert_eq!(rz.child_count(), 4);
    }

    #[test]
    fn btm_subtract_dangling_branching_factor_test() {
        // Left has one populated branch under [5] alongside an unrelated top-level key
        let mut left: PathMap<()> = PathMap::new();
        left.create_path([5u8, 0u8, 9u8]);
        left.create_path([8u8]);

        // Right overlaps [5] but introduces additional branches (value + dangling)
        let mut right: PathMap<()> = PathMap::new();
        right.create_path([5u8, 0u8, 9u8]);
        right.create_path([5u8, 1u8, 1u8]);
        right.create_path([5u8, 2u8]); // dangling extra branch

        let remaining = left.subtract(&right);
        assert_eq!(remaining.path_exists_at([5u8, 0u8, 9u8]), false);
        assert_eq!(remaining.path_exists_at([5u8, 1u8, 1u8]), false);
        assert_eq!(remaining.path_exists_at([8u8]), true);
        assert_eq!(remaining.path_exists_at([5u8, 2u8]), false);

        //Try the subtraction the other way
        let remaining = right.subtract(&left);
        assert_eq!(remaining.path_exists_at([5u8, 0u8, 9u8]), false);
        assert_eq!(remaining.path_exists_at([5u8, 1u8, 1u8]), true);
        assert_eq!(remaining.path_exists_at([8u8]), false);
        assert_eq!(remaining.path_exists_at([5u8, 2u8]), true);
    }

    #[test]
    fn btm_test_restrict1() {
        let mut l: PathMap<&str> = PathMap::new();
        l.set_val_at(b"alligator", "alligator");
        l.set_val_at(b"allegedly", "allegedly");
        l.set_val_at(b"albatross", "albatross");
        l.set_val_at(b"albino", "albino");
        let mut r: PathMap<&str> = PathMap::new();
        r.set_val_at(b"all", "all");
        let restricted = l.restrict(&r);

        assert_eq!(restricted.val_count(), 2);
        assert_eq!(restricted.get_val_at(b"alligator"), Some(&"alligator"));
        assert_eq!(restricted.get_val_at(b"albino"), None);
        assert_eq!(restricted.get_val_at(b"allegedly"), Some(&"allegedly"));
        assert_eq!(restricted.get_val_at(b"albatross"), None);
    }

    /// Tests restrictions on a very dense trie
    #[test]
    fn btm_test_restrict2() {

        // These values are base-4 numbers in "little endian"
        let keys = [
            vec![0],    vec![1],    vec![2],    vec![3],    vec![0, 1], vec![1, 1], vec![2, 1], vec![3, 1],
            vec![0, 2], vec![1, 2], vec![2, 2], vec![3, 2], vec![0, 3], vec![1, 3], vec![2, 3], vec![3, 3],
            vec![0, 0, 1], vec![1, 0, 1], vec![2, 0, 1], vec![3, 0, 1]
        ];
        let map: PathMap<i32> = keys.iter().enumerate().map(|(i, k)| (k, i as i32)).collect();

        // Restrict to odd numbers
        let odd_keys = [ vec![1], vec![3]];
        let odd_map: PathMap<i32> = odd_keys.iter().enumerate().map(|(i, k)| (k, i as i32)).collect();
        let restricted = map.restrict(&odd_map);

        assert_eq!(restricted.val_count(), 10);
        assert_eq!(restricted.get_val_at([1]), Some(&1));
        assert_eq!(restricted.get_val_at([3]), Some(&3));
        assert_eq!(restricted.get_val_at([1, 1]), Some(&5));
        assert_eq!(restricted.get_val_at([3, 1]), Some(&7));
        assert_eq!(restricted.get_val_at([1, 2]), Some(&9));
        assert_eq!(restricted.get_val_at([3, 2]), Some(&11));
        assert_eq!(restricted.get_val_at([1, 3]), Some(&13));
        assert_eq!(restricted.get_val_at([3, 3]), Some(&15));
        assert_eq!(restricted.get_val_at([1, 0, 1]), Some(&17));
        assert_eq!(restricted.get_val_at([3, 0, 1]), Some(&19));

        // Restrict to numbers divisible by 4 (exluding 0; 0 technically isn't divisible by 4)
        let div4_keys = [ vec![0, 0], vec![0, 1], vec![0, 2], vec![0, 3]];
        let div4_map: PathMap<i32> = div4_keys.iter().enumerate().map(|(i, k)| (k, i as i32)).collect();
        let restricted = map.restrict(&div4_map);

        assert_eq!(restricted.val_count(), 4);
        assert_eq!(restricted.get_val_at([0]), None);
        assert_eq!(restricted.get_val_at([0, 0]), None);
        assert_eq!(restricted.get_val_at([0, 1]), Some(&4));
        assert_eq!(restricted.get_val_at([0, 2]), Some(&8));
        assert_eq!(restricted.get_val_at([0, 3]), Some(&12));
        assert_eq!(restricted.get_val_at([0, 0, 1]), Some(&16));
    }

    /// Tests restrictions on a fairly sparse trie
    #[test]
    fn btm_test_restrict3() {
        let keys = [
            "a",
            "acting",
            "activated",
            "activation",
            "activities",
            "acute",
            "adaptation",
            "adapter",
        ];
        let map: PathMap<i32> = keys.iter().enumerate().map(|(i, k)| (k, i as i32)).collect();

        // Restrict to words beginning with "act"
        let restrictor = [ "act" ];
        let restrictor_map: PathMap<i32> = restrictor.iter().enumerate().map(|(i, k)| (k, i as i32)).collect();
        let restricted = map.restrict(&restrictor_map);

        assert_eq!(restricted.val_count(), 4);
        assert_eq!(restricted.get_val_at("acting"), Some(&1));
        assert_eq!(restricted.get_val_at("activities"), Some(&4));

        // Restrict to words beginning with "a"
        let restrictor = [ "a" ];
        let restrictor_map: PathMap<i32> = restrictor.iter().enumerate().map(|(i, k)| (k, i as i32)).collect();
        let restricted = map.restrict(&restrictor_map);

        assert_eq!(restricted.val_count(), 8);
        assert_eq!(restricted.get_val_at("a"), Some(&0));
        assert_eq!(restricted.get_val_at("acting"), Some(&1));
        assert_eq!(restricted.get_val_at("activities"), Some(&4));
        assert_eq!(restricted.get_val_at("adapter"), Some(&7));
    }

    /// Tests values that are attached along the paths to other keys, and also tests the absence of keys
    /// after existing values.
    #[test]
    fn path_prefix_test() {
        let mut map = PathMap::<u64>::new();

        map.set_val_at(&[0u8], 1);
        assert_eq!(map.get_val_at(&[0u8]), Some(&1));
        assert_eq!(map.get_val_at(&[0u8, 0u8]), None);
        assert_eq!(map.get_val_at(&[0u8, 0u8, 0u8]), None);

        map.set_val_at(&[0u8, 0u8, 0u8, 0u8], 4);
        assert_eq!(map.get_val_at(&[0u8]), Some(&1));
        assert_eq!(map.get_val_at(&[0u8, 0u8]), None);
        assert_eq!(map.get_val_at(&[0u8, 0u8, 0u8]), None);
        assert_eq!(map.get_val_at(&[0u8, 0u8, 0u8, 0u8]), Some(&4));

        map.set_val_at(&[0u8, 0u8, 0u8, 0u8, 0u8], 5);
        assert_eq!(map.get_val_at(&[0u8, 0u8, 0u8, 0u8]), Some(&4));
        assert_eq!(map.get_val_at(&[0u8, 0u8, 0u8, 0u8, 0u8]), Some(&5));

        map.set_val_at(&[0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8], 9);
        assert_eq!(map.get_val_at(&[0u8]), Some(&1));
        assert_eq!(map.get_val_at(&[0u8, 0u8]), None);
        assert_eq!(map.get_val_at(&[0u8, 0u8, 0u8]), None);
        assert_eq!(map.get_val_at(&[0u8, 0u8, 0u8, 0u8]), Some(&4));
        assert_eq!(map.get_val_at(&[0u8, 0u8, 0u8, 0u8, 0u8]), Some(&5));
        assert_eq!(map.get_val_at(&[0u8, 0u8, 0u8, 0u8, 0u8, 0u8]), None);
        assert_eq!(map.get_val_at(&[0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8]), None);
        assert_eq!(map.get_val_at(&[0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8]), None);
        assert_eq!(map.get_val_at(&[0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8]), Some(&9));
        assert_eq!(map.get_val_at(&[0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8]), None);
    }

    #[test]
    fn map_meet_dangling_branching_factor_test1() {
        // Left contains a path without a split
        let mut left: PathMap<u64> = PathMap::new();
        left.create_path([7u8, 1u8, 0u8]);

        // Right has a 3-way split and fans out at level 1
        let mut right: PathMap<u64> = PathMap::new();
        right.set_val_at([7u8, 1u8, 0u8], 10);
        right.set_val_at([7u8, 2u8, 0u8], 20);
        right.create_path([7u8, 3u8]);

        let intersection = left.meet(&right);
        assert_eq!(intersection.path_exists_at([7u8, 1u8, 0u8]), true); //Should have had its value removed, but the path should remain
        assert_eq!(intersection.get_val_at([7u8, 1u8, 0u8]), None);
        assert_eq!(intersection.path_exists_at([7u8, 2u8, 0u8]), false);
        assert_eq!(intersection.path_exists_at([7u8, 3u8]), false);

        //Make sure the result is the same with the opposite operand order
        let intersection = right.meet(&left);
        assert_eq!(intersection.path_exists_at([7u8, 1u8, 0u8]), true);
        assert_eq!(intersection.get_val_at([7u8, 1u8, 0u8]), None);
        assert_eq!(intersection.path_exists_at([7u8, 2u8, 0u8]), false);
        assert_eq!(intersection.path_exists_at([7u8, 3u8]), false);

        // TEST 2.  Same as above, but with less nesting
        let mut left: PathMap<u64> = PathMap::new();
        left.create_path([7u8, 1u8]);
        let mut right: PathMap<u64> = PathMap::new();
        right.set_val_at([7u8, 1u8], 10);
        right.set_val_at([7u8, 2u8], 20);
        right.create_path([7u8, 3u8]);

        let intersection = left.meet(&right);
        assert_eq!(intersection.path_exists_at([7u8, 1u8]), true); //Should have had its value removed, but the path should remain
        assert_eq!(intersection.get_val_at([7u8, 1u8]), None);
        assert_eq!(intersection.path_exists_at([7u8, 2u8]), false);
        assert_eq!(intersection.path_exists_at([7u8, 3u8]), false);

        //Make sure the result is the same with the opposite operand order
        let intersection = right.meet(&left);
        assert_eq!(intersection.path_exists_at([7u8, 1u8]), true);
        assert_eq!(intersection.get_val_at([7u8, 1u8]), None);
        assert_eq!(intersection.path_exists_at([7u8, 2u8]), false);
        assert_eq!(intersection.path_exists_at([7u8, 3u8]), false);
    }

    #[test]
    fn map_meet_dangling_branching_factor_test2() {
        //Test 1: Path subsets
        let mut left: PathMap<()> = PathMap::new();
        left.create_path(b"OneTwo");
        let mut right: PathMap<()> = PathMap::new();
        right.create_path(b"OneTwoThree");

        let intersection = left.meet(&right);
        assert_eq!(intersection.path_exists_at(b"OneTwo"), true);
        assert_eq!(intersection.path_exists_at(b"OneTwoThree"), false);
        assert_eq!(intersection.path_exists_at(b"OneTwoT"), false);
    }

    #[test]
    fn map_meet_after_join_test() {
        #[cfg(miri)]
        const N: u64 = 20;
        #[cfg(all(feature = "pathmap-internal-tests", not(miri)))]
        const N: u64 = 1000;

        let mut l: PathMap<u64> = PathMap::new();
        for i in 0..(N/2) { l.set_val_at(prefix_key(&i), i); }
        let mut r: PathMap<u64> = PathMap::new();
        for i in (N/2)..N { r.set_val_at(prefix_key(&i), i); }

        let joined = l.join(&r);
        let met = joined.meet(&l);
        for (met, l) in met.iter().zip(l.iter()) {
            assert_eq!(met, l);
        }

        let met = met.meet(&r);
        assert!(met.is_empty());
    }

    #[test]
    fn map_meet_big_test() {
        #[cfg(miri)]
        const N: u64 = 20;
        #[cfg(all(feature = "pathmap-internal-tests", not(miri)))]
        const N: u64 = 16000;

        let overlap = 0.5;
        let o = ((1. - overlap) * N as f64) as u64;

        let mut rng = StdRng::seed_from_u64(1);
        let keys: Vec<Vec<u8>> = (0..N+o).into_iter().map(|_| {
            let len = (rng.random::<u8>() % 18) + 3; //length between 3 and 20 chars
            (0..len).into_iter().map(|_| rng.random::<u8>()).collect()
        }).collect();

        let mut l: PathMap<u64> = PathMap::new();
        for i in 0..N { l.set_val_at(&keys[i as usize], i); }
        let mut r: PathMap<u64> = PathMap::new();
        for i in o..(N+o) { r.set_val_at(&keys[i as usize], i); }

        let intersection = l.meet(&r);
        assert_eq!(intersection.val_count(), (N/2) as usize);
    }

    /// This test is a minimal repro case for a bug where a list node is intersected with a byte node,
    /// and both slots in the list node match items in the byte node, but the byte node carries extra elements
    #[test]
    fn map_meet_lil_test() {
        let l_keys = [
            vec![207, 27],  //NON-OVERLAP!
            vec![207, 117], //Overlap
            vec![207, 142], //Overlap
            // vec![208, 250], //Overlap
            // vec![213, 63],  //Overlap
        ];
        let r_keys = [
            vec![207, 117], //Overlap
            vec![207, 142], //Overlap
            // vec![208, 157], //NON-OVERLAP!
            // vec![208, 250], //Overlap
            // vec![213, 63],  //Overlap
        ];
        let l: PathMap<u64> = l_keys.into_iter().map(|v| (v, 0)).collect();
        let r: PathMap<u64> = r_keys.into_iter().map(|v| (v, 0)).collect();

        let intersection = l.meet(&r);
        assert_eq!(intersection.val_count(), 2);
    }

    #[test]
    fn btm_ops_test() {
        #[cfg(miri)]
        const N: u64 = 20;
        #[cfg(all(feature = "pathmap-internal-tests", not(miri)))]
        const N: u64 = 5000;

        for n in (0..N).into_iter().step_by(97) {
            // println!("n={n}");

            let overlap = 0.5;
            let o = ((1. - overlap) * n as f64) as u64;
            {
                let mut vnl = PathMap::new();
                let mut vnr = PathMap::new();
                for i in 0..n { vnl.set_val_at(prefix_key(&i), i); }
                // println!("{:?}", vnl.root);
                for i in 0..n { assert_eq!(vnl.get_val_at(prefix_key(&i)), Some(i).as_ref()); }
                for i in n..2*n { assert_eq!(vnl.get_val_at(prefix_key(&i)), None); }
                let mut c: Vec<u64> = Vec::with_capacity(n as usize);
                vnl.iter().for_each(|(k, v)| {
                    assert!(*v < n);
                    assert_eq!(k, prefix_key(&v));
                    c.push(from_prefix_key(k.clone()));
                });
                c.sort();
                assert_eq!(c, (0..n).collect::<Vec<u64>>());
                for i in o..(n+o) { vnr.set_val_at(prefix_key(&i), i); }

                let j: PathMap<u64> = vnl.join(&vnr);
                let m = vnl.meet(&vnr);
                let l_no_r = vnl.subtract(&vnr);

                for i in 0..o { assert_eq!(l_no_r.get_val_at(prefix_key(&i)), vnl.get_val_at(prefix_key(&i))); }
                for i in o..(n+o) { assert!(!l_no_r.contains(prefix_key(&i))); }

                for i in o..n { assert!(vnl.contains(prefix_key(&i)) && vnr.contains(prefix_key(&i))); }
                for i in 0..o { assert!(vnl.contains(prefix_key(&i)) && !vnr.contains(prefix_key(&i))); }
                for i in n..(n+o) { assert!(!vnl.contains(prefix_key(&i)) && vnr.contains(prefix_key(&i))); }
                for i in 0..(2*n) { assert_eq!(j.contains(prefix_key(&i)), (vnl.contains(prefix_key(&i)) || vnr.contains(prefix_key(&i)))); }
                for i in 0..(2*n) { assert_eq!(m.contains(prefix_key(&i)), (vnl.contains(prefix_key(&i)) && vnr.contains(prefix_key(&i)))); }
                for i in 0..(n+o) { assert_eq!(j.get_val_at(prefix_key(&i)).map(|v| *v), vnl.get_val_at(prefix_key(&i)).pjoin(&vnr.get_val_at(prefix_key(&i))).into_option([vnl.get_val_at(prefix_key(&i)).cloned(), vnr.get_val_at(prefix_key(&i)).cloned()]).flatten()); }
                for i in o..n { assert_eq!(m.get_val_at(prefix_key(&i)).map(|v| *v), vnl.get_val_at(prefix_key(&i)).pmeet(&vnr.get_val_at(prefix_key(&i))).into_option([vnl.get_val_at(prefix_key(&i)).cloned(), vnr.get_val_at(prefix_key(&i)).cloned()]).flatten()); }
                // for i in 0..(2*N) { println!("{} {} {} {}", i, r.contains(i), vnl.contains(i), vnr.contains(i)); } // assert!(r.contains(i));
            }
        }
    }

    /// This tests longer and longer keys to see if / where we blow the stack
    #[test]
    fn map_very_long_key_test() {

        let test_key_len = |len: usize| {
            let mut map: PathMap<u64> = PathMap::new();
            let mut z = map.write_zipper();
            let key = vec![0u8; len];
            z.descend_to(&key);
            z.set_val(42);
            drop(z);
            assert_eq!(map.get_val_at(&key), Some(&42));
        };

        test_key_len(1024); //2^10 bytes
        test_key_len(2048); //2^11 bytes

        //all_dense_nodes are terrible at chaining, but there isn't much point in an optimized path for them
        #[cfg(not(feature = "all_dense_nodes"))]
        #[cfg(all(feature = "pathmap-internal-tests", not(miri)))]
        {
            test_key_len(4096); //2^12 bytes
            test_key_len(16384); //2^14 bytes
            test_key_len(32768); //2^15 bytes //Failed here with recursive drop
            test_key_len(65536); //2^16 bytes
            test_key_len(262144); //2^18 bytes
            test_key_len(1048576); //2^20 bytes
            // test_key_len(4194304); //2^22 bytes //Disabled from here so tests run quickly
            // test_key_len(16777216); //2^24 bytes
            // test_key_len(67108864); //2^26 bytes
            // test_key_len(268435456); //2^28 bytes
            // test_key_len(1073741824); //2^30 bytes - 1GB keys
            // test_key_len(2147483648); //2^31 bytes - 2GB keys
            // test_key_len(4294967296); //2^32 bytes - 4GB keys
            // test_key_len(8589934592); //2^33 bytes - 8GB keys
            // test_key_len(17179869184); //2^34 bytes - Still no failure at 16GB keys
        }
    }

}
