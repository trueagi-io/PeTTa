use super::super::super::utils::ByteMask;
use super::super::super::zipper::*;

/// A [Zipper] type that wraps two other zippers that are expected to behave identically,
/// and panics when they don't.  Useful to debug a Zipper implementation.
pub struct DiffZipper<A: Zipper, B: Zipper> {
    a: A,
    b: B,
    log_moves: bool,
}

impl<A: Zipper, B: Zipper> DiffZipper<A, B> {
    pub fn new(a: A, b: B, log_moves: bool) -> Self {
        Self { a, b, log_moves }
    }
}

impl<A: Zipper, B: Zipper> Zipper for DiffZipper<A, B> {
    fn path_exists(&self) -> bool {
        let a = self.a.path_exists();
        let b = self.b.path_exists();
        assert_eq!(a, b);
        a
    }
    fn is_val(&self) -> bool {
        let a = self.a.is_val();
        let b = self.b.is_val();
        assert_eq!(a, b);
        a
    }
    fn child_count(&self) -> usize {
        let a = self.a.child_count();
        let b = self.b.child_count();
        assert_eq!(a, b);
        a
    }
    fn child_mask(&self) -> ByteMask {
        let a = self.a.child_mask();
        let b = self.b.child_mask();
        assert_eq!(a, b);
        a
    }
}

impl<A: Zipper + ZipperMoving, B: Zipper + ZipperMoving> ZipperMoving for DiffZipper<A, B> {
    fn at_root(&self) -> bool {
        let a = self.a.at_root();
        let b = self.b.at_root();
        assert_eq!(a, b);
        a
    }
    fn reset(&mut self) {
        self.a.reset();
        self.b.reset();
        if self.log_moves {
            println!("DiffZipper: reset")
        }
    }
    fn path(&self) -> &[u8] {
        let a = self.a.path();
        let b = self.b.path();
        assert_eq!(a, b);
        a
    }
    fn val_count(&self) -> usize {
        let a = self.a.val_count();
        let b = self.b.val_count();
        assert_eq!(a, b);
        a
    }
    fn descend_to<P: AsRef<[u8]>>(&mut self, path: P) {
        let path = path.as_ref();
        self.a.descend_to(path);
        self.b.descend_to(path);
        if self.log_moves {
            println!("DiffZipper: descend_to path={path:?}")
        }
        assert_eq!(self.a.path_exists(), self.b.path_exists());
    }
    fn descend_to_existing<P: AsRef<[u8]>>(&mut self, path: P) -> usize {
        let path = path.as_ref();
        let a = self.a.descend_to_existing(path);
        let b = self.b.descend_to_existing(path);
        if self.log_moves {
            println!("DiffZipper: descend_to_existing path={path:?}")
        }
        assert_eq!(a, b);
        a
    }
    fn descend_to_val<K: AsRef<[u8]>>(&mut self, path: K) -> usize {
        let path = path.as_ref();
        let a = self.a.descend_to_val(path);
        let b = self.b.descend_to_val(path);
        if self.log_moves {
            println!("DiffZipper: descend_to_val path={path:?}")
        }
        assert_eq!(a, b);
        a
    }
    fn descend_to_byte(&mut self, k: u8) {
        self.a.descend_to_byte(k);
        self.b.descend_to_byte(k);
        if self.log_moves {
            println!("DiffZipper: descend_to_byte k={k}")
        }
        assert_eq!(self.a.path_exists(), self.b.path_exists());
    }
    fn descend_indexed_byte(&mut self, idx: usize) -> bool {
        let a = self.a.descend_indexed_byte(idx);
        let b = self.b.descend_indexed_byte(idx);
        if self.log_moves {
            println!("DiffZipper: descend_indexed_byte idx={idx}")
        }
        assert_eq!(a, b);
        a
    }
    fn descend_first_byte(&mut self) -> bool {
        let a = self.a.descend_first_byte();
        let b = self.b.descend_first_byte();
        if self.log_moves {
            println!("DiffZipper: descend_first_byte")
        }
        assert_eq!(a, b);
        a
    }
    fn descend_until(&mut self) -> bool {
        let a = self.a.descend_until();
        let b = self.b.descend_until();
        if self.log_moves {
            println!("DiffZipper: descend_until")
        }
        assert_eq!(a, b);
        a
    }
    fn ascend(&mut self, steps: usize) -> bool {
        let a = self.a.ascend(steps);
        let b = self.b.ascend(steps);
        if self.log_moves {
            println!("DiffZipper: ascend steps={steps}")
        }
        assert_eq!(a, b);
        a
    }
    fn ascend_byte(&mut self) -> bool {
        let a = self.a.ascend_byte();
        let b = self.b.ascend_byte();
        if self.log_moves {
            println!("DiffZipper: ascend_byte")
        }
        assert_eq!(a, b);
        a
    }
    fn ascend_until(&mut self) -> bool {
        let a = self.a.ascend_until();
        let b = self.b.ascend_until();
        if self.log_moves {
            println!("DiffZipper: ascend_until")
        }
        assert_eq!(a, b);
        a
    }
    fn ascend_until_branch(&mut self) -> bool {
        let a = self.a.ascend_until_branch();
        let b = self.b.ascend_until_branch();
        if self.log_moves {
            println!("DiffZipper: ascend_until_branch")
        }
        assert_eq!(a, b);
        a
    }
    fn to_next_sibling_byte(&mut self) -> bool {
        let a = self.a.to_next_sibling_byte();
        let b = self.b.to_next_sibling_byte();
        if self.log_moves {
            println!("DiffZipper: to_next_sibling_byte")
        }
        assert_eq!(a, b);
        a
    }
    fn to_prev_sibling_byte(&mut self) -> bool {
        let a = self.a.to_prev_sibling_byte();
        let b = self.b.to_prev_sibling_byte();
        if self.log_moves {
            println!("DiffZipper: to_prev_sibling_byte")
        }
        assert_eq!(a, b);
        a
    }
}

impl<A: Zipper + ZipperAbsolutePath, B: Zipper + ZipperAbsolutePath> ZipperAbsolutePath
    for DiffZipper<A, B>
{
    fn origin_path(&self) -> &[u8] {
        let a = self.a.origin_path();
        let b = self.b.origin_path();
        assert_eq!(a, b);
        a
    }
    fn root_prefix_path(&self) -> &[u8] {
        let a = self.a.root_prefix_path();
        let b = self.b.root_prefix_path();
        assert_eq!(a, b);
        a
    }
}

impl<A: Zipper + ZipperPathBuffer, B: Zipper + ZipperPathBuffer> ZipperPathBuffer
    for DiffZipper<A, B>
{
    unsafe fn origin_path_assert_len(&self, len: usize) -> &[u8] {
        let a = unsafe { self.a.origin_path_assert_len(len) };
        let b = unsafe { self.b.origin_path_assert_len(len) };
        assert_eq!(a, b);
        a
    }
    fn reserve_buffers(&mut self, path_len: usize, stack_depth: usize) {
        self.a.reserve_buffers(path_len, stack_depth);
        self.b.reserve_buffers(path_len, stack_depth);
    }

    fn prepare_buffers(&mut self) {
        self.a.prepare_buffers();
        self.b.prepare_buffers();
    }
}

impl<A: Zipper + ZipperIteration, B: Zipper + ZipperIteration> ZipperIteration
    for DiffZipper<A, B>
{
    fn to_next_val(&mut self) -> bool {
        let a = self.a.to_next_val();
        let b = self.b.to_next_val();
        if self.log_moves {
            println!("DiffZipper: to_next_val")
        }
        assert_eq!(a, b);
        a
    }
    fn descend_first_k_path(&mut self, k: usize) -> bool {
        let a = self.a.descend_first_k_path(k);
        let b = self.b.descend_first_k_path(k);
        if self.log_moves {
            println!("DiffZipper: descend_first_k_path k={k}")
        }
        assert_eq!(a, b);
        a
    }
    fn to_next_k_path(&mut self, k: usize) -> bool {
        let a = self.a.to_next_k_path(k);
        let b = self.b.to_next_k_path(k);
        if self.log_moves {
            println!("DiffZipper: to_next_k_path k={k}")
        }
        assert_eq!(a, b);
        a
    }
}

impl<PZL: ZipperProduct, PZR: ZipperProduct> ZipperProduct for DiffZipper<PZL, PZR> {
    fn focus_factor(&self) -> usize {
        let a = self.a.focus_factor();
        let b = self.b.focus_factor();
        assert_eq!(a, b);
        a
    }
    fn factor_count(&self) -> usize {
        let a = self.a.factor_count();
        let b = self.b.factor_count();
        assert_eq!(a, b);
        a
    }
    fn path_indices(&self) -> &[usize] {
        let a = self.a.path_indices();
        let b = self.b.path_indices();
        assert_eq!(a, b);
        a
    }
}
