//! Unified hasher: gxhash (fast) or fallback (portable).
//! Re-exports external gxhash crate or provides pure-Rust fallback.

#[cfg(all(not(any(miri, target_arch = "riscv64")), feature = "fast-hasher"))]
pub use gxhash::*;

#[cfg(not(all(not(any(miri, target_arch = "riscv64")), feature = "fast-hasher")))]
mod fallback {
    pub use std::collections::{HashMap, HashSet};

    #[derive(Clone, Default)]
    pub struct GxHasher {
        state_lo: u64,
        state_hi: u64,
    }

    impl GxHasher {
        pub fn with_seed(seed: i64) -> Self {
            let seed = u64::from_ne_bytes(seed.to_ne_bytes());
            Self { state_lo: seed ^ 0xA5A5A5A5_A5A5A5A5, state_hi: !seed ^ 0x5A5A5A5A_5A5A5A5A }
        }

        pub fn finish_u128(&self) -> u128 {
            ((self.state_hi as u128) << 64) | self.state_lo as u128
        }
    }

    impl std::hash::Hasher for GxHasher {
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

    pub fn gxhash128(data: &[u8], _seed: i64) -> u128 {
        xxhash_rust::const_xxh3::xxh3_128(data)
    }

    pub trait HashMapExt: Sized {
        fn new() -> Self;
        fn with_capacity(capacity: usize) -> Self;
    }

    pub trait HashSetExt: Sized {
        fn new() -> Self;
        fn with_capacity(capacity: usize) -> Self;
    }

    impl<K, V> HashMapExt for HashMap<K, V, GxBuildHasher> {
        fn new() -> Self {
            HashMap::with_hasher(GxBuildHasher)
        }
        fn with_capacity(capacity: usize) -> Self {
            HashMap::with_capacity_and_hasher(capacity, GxBuildHasher)
        }
    }

    impl<T> HashSetExt for HashSet<T, GxBuildHasher> {
        fn new() -> Self {
            HashSet::with_hasher(GxBuildHasher)
        }
        fn with_capacity(capacity: usize) -> Self {
            HashSet::with_capacity_and_hasher(capacity, GxBuildHasher)
        }
    }

    #[derive(Clone, Default)]
    pub struct GxBuildHasher;

    impl std::hash::BuildHasher for GxBuildHasher {
        type Hasher = GxHasher;
        fn build_hasher(&self) -> GxHasher {
            GxHasher::default()
        }
    }
}

#[cfg(not(all(not(any(miri, target_arch = "riscv64")), feature = "fast-hasher")))]
pub use fallback::*;
