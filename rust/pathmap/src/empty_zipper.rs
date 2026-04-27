use super::utils::ByteMask;
use super::zipper::*;

/// A [`Zipper`] type that moves over a completely empty trie
#[derive(Clone, Default)]
pub struct EmptyZipper {
    path_start_idx: usize,
    path: Vec<u8>, //NOTE: we could init this lazily and take a borrowed path, but I can't see a use case where that would matter
}

impl EmptyZipper {
    /// Returns a new `EmptyZipper` starting at the root
    pub fn new() -> Self {
        Self::default()
    }
    /// Returns a new `EmptyZipper` with the provided [`root_prefix_path`](ZipperAbsolutePath::root_prefix_path)
    pub fn new_at_path<K: AsRef<[u8]>>(path: K) -> Self {
        let path = path.as_ref();
        Self { path_start_idx: path.len(), path: path.to_vec() }
    }
}

impl Zipper for EmptyZipper {
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

impl ZipperMoving for EmptyZipper {
    fn at_root(&self) -> bool {
        self.path.len() == self.path_start_idx
    }
    fn reset(&mut self) {
        self.path.truncate(self.path_start_idx)
    }
    fn path(&self) -> &[u8] {
        &self.path[self.path_start_idx..]
    }
    fn val_count(&self) -> usize {
        0
    }
    fn descend_to<K: AsRef<[u8]>>(&mut self, k: K) {
        self.path.extend_from_slice(k.as_ref());
    }
    fn descend_to_byte(&mut self, k: u8) {
        self.path.push(k);
    }
    fn descend_indexed_byte(&mut self, _idx: usize) -> bool {
        false
    }
    fn descend_first_byte(&mut self) -> bool {
        false
    }
    fn descend_until(&mut self) -> bool {
        false
    }
    fn ascend(&mut self, steps: usize) -> bool {
        if steps > self.path.len() - self.path_start_idx {
            self.reset();
            false
        } else {
            self.path.truncate(self.path.len() - self.path_start_idx - steps);
            true
        }
    }
    fn ascend_byte(&mut self) -> bool {
        if self.path.len() > self.path_start_idx {
            self.path.pop();
            true
        } else {
            false
        }
    }
    fn ascend_until(&mut self) -> bool {
        if self.at_root() {
            false
        } else {
            self.reset();
            true
        }
    }
    fn ascend_until_branch(&mut self) -> bool {
        self.ascend_until()
    }
    fn to_next_sibling_byte(&mut self) -> bool {
        false
    }
    fn to_prev_sibling_byte(&mut self) -> bool {
        false
    }
}

impl ZipperAbsolutePath for EmptyZipper {
    fn origin_path(&self) -> &[u8] {
        &self.path
    }
    fn root_prefix_path(&self) -> &[u8] {
        &self.path[..self.path_start_idx]
    }
}

impl ZipperIteration for EmptyZipper {
    fn to_next_val(&mut self) -> bool {
        false
    }
    fn descend_first_k_path(&mut self, _k: usize) -> bool {
        false
    }
    fn to_next_k_path(&mut self, _k: usize) -> bool {
        false
    }
}

impl<V> ZipperValues<V> for EmptyZipper {
    fn val(&self) -> Option<&V> {
        None
    }
}

impl<V> ZipperForking<V> for EmptyZipper {
    type ReadZipperT<'a> = EmptyZipper;
    fn fork_read_zipper<'a>(&'a self) -> Self::ReadZipperT<'a> {
        Self::new_at_path(self.origin_path())
    }
}

impl<'a, V: Clone + Send + Sync> ZipperReadOnlyValues<'a, V> for EmptyZipper {
    fn get_val(&self) -> Option<&'a V> {
        None
    }
}

impl<'a, V: Clone + Send + Sync> ZipperReadOnlyConditionalValues<'a, V> for EmptyZipper {
    type WitnessT = ();
    fn witness<'w>(&self) -> Self::WitnessT {}
    fn get_val_with_witness<'w>(&self, _witness: &'w Self::WitnessT) -> Option<&'w V>
    where
        'a: 'w,
    {
        None
    }
}

impl<'a, V: Clone + Send + Sync> ZipperReadOnlyIteration<'a, V> for EmptyZipper {
    fn to_next_get_val(&mut self) -> Option<&'a V> {
        None
    }
}

impl<'a, V: Clone + Send + Sync> ZipperReadOnlyConditionalIteration<'a, V> for EmptyZipper {
    fn to_next_get_val_with_witness<'w>(&mut self, _witness: &'w Self::WitnessT) -> Option<&'w V>
    where
        'a: 'w,
    {
        None
    }
}

impl ZipperPathBuffer for EmptyZipper {
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

super::impl_name_only_debug!(
    impl core::fmt::Debug for EmptyZipper
);
