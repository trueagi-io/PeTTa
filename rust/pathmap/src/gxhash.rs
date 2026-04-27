//! Unified hasher: gxhash (fast) or std hasher (portable).

#[cfg(all(not(any(miri, target_arch = "riscv64")), feature = "fast-hasher"))]
pub use gxhash::*;

#[cfg(not(all(not(any(miri, target_arch = "riscv64")), feature = "fast-hasher")))]
mod fallback {
    use std::collections::{HashMap, HashSet};
    
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

        fn write_u16(&mut self, i: u16) { self.write_u8(i as u8); self.write_u8((i >> 8) as u8); }
        fn write_u32(&mut self, i: u32) { self.write_u128(i as u128); }
        fn write_u64(&mut self, i: u64) { self.write_u128(i as u128); }
        fn write_u128(&mut self, i: u128) {
            let low = i as u64;
            let high = (i >> 64) as u64;
            self.state_lo = self.state_lo.wrapping_add(low);
            self.state_hi ^= high.rotate_left(17);
            self.state_lo ^= high.rotate_left(9);
        }
        fn write_i8(&mut self, i: i8) { self.write_u8(i as u8); }
        fn write_i16(&mut self, i: i16) { self.write_u16(i as u16); }
        fn write_i32(&mut self, i: i32) { self.write_u32(i as u32); }
        fn write_i64(&mut self, i: i64) { self.write_u64(i as u64); }
        fn write_i128(&mut self, i: i128) { self.write_u128(i as u128); }
        fn write_str(&mut self, s: &str) { self.write(s.as_bytes()); }
    }

    #[derive(Clone, Default)]
    pub struct GxBuildHasher(GxHasher);

    impl std::hash::BuildHasher for GxBuildHasher {
        type Hasher = GxHasher;
        fn build_hasher(&self) -> Self::Hasher {
            self.0.clone()
        }
    }

    impl std::hash::BuildHasherDefault for GxBuildHasher {
        fn build_hasher_default() -> Self::Hasher {
            GxHasher::default()
        }
    }

    pub type HashMap<K, V> = HashMap<K, V, GxBuildHasher>;
    pub type HashSet<T> = HashSet<T, GxBuildHasher>;

    pub trait HashMapExt<K, V> {
        fn new() -> Self;
        fn with_capacity(cap: usize) -> Self;
    }

    impl<K, V> HashMapExt<K, V> for HashMap<K, V> {
        fn new() -> Self { HashMap::with_hasher(GxBuildHasher::default()) }
        fn with_capacity(cap: usize) -> Self { HashMap::with_capacity_and_hasher(cap, GxBuildHasher::default()) }
    }
}

#[cfg(not(all(not(any(miri, target_arch = "riscv64")), feature = "fast-hasher")))]
pub use fallback::{GxHasher, HashMap, HashSet, HashMapExt, GxBuildHasher};

#[cfg(all(not(any(miri, target_arch = "riscv64")), feature = "fast-hasher"))]
pub type GxHashMap<K, V> = gxhash::GxHashMap<K, V>;

#[cfg(not(all(not(any(miri, target_arch = "riscv64")), feature = "fast-hasher")))]
pub type GxHashMap<K, V> = fallback::HashMap<K, V>;