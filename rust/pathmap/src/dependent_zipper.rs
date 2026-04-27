use super::trie_core::r#ref::TrieRef;

use super::PathMap;
use super::alloc::{GlobalAlloc, global_alloc};

// note, this is almost identical in implementation to ProductZipperG
// It's very likely, for maintainability, we'll want to implement ProductZipperG as a simple policy over DependentProductZipperG
// However, all my attempts at this failed so far because of the enroll enter/exit closure types in the struct
use super::utils::ByteMask;
use super::zipper::*;

/// A [Zipper] type that moves through a Cartesian product trie created by extending each path in a primary
/// trie with the computed virtual trie, doing it recursively for all returned tries
///
/// Compared to [ProductZipperG], this allows the factors zippers to be calculated on the fly, and depend on the prefix.
///
/// GOAT: The shape of this API is still experimental.  Creating new owned zippers is not great for performance and
/// there is probably a design that allows mutable references to be returned from an object with an appropriate lifetime.
#[derive(Clone)]
pub struct DependentProductZipperG<'trie, PrimaryZ, SecondaryZ, V, C, F>
where
    V: Clone + Send + Sync,
{
    factor_paths: Vec<usize>,
    primary: PrimaryZ,
    secondary: Vec<SecondaryZ>,
    enroll_payload: Option<C>,
    enroll: F,
    _marker: core::marker::PhantomData<(&'trie V, F)>,
}

impl<
    'trie,
    PrimaryZ,
    SecondaryZ,
    V,
    C,
    F: Clone + for<'a> FnOnce(C, &'a [u8], usize) -> (C, Option<SecondaryZ>),
> DependentProductZipperG<'trie, PrimaryZ, SecondaryZ, V, C, F>
where
    V: Clone + Send + Sync,
    PrimaryZ: ZipperMoving,
    SecondaryZ: ZipperMoving,
{
    /// Creates a new `DependentProductZipperG` from the provided enroll function
    pub fn new_enroll(primary: PrimaryZ, enroll_payload: C, enroll: F) -> Self
    where
        PrimaryZ: ZipperValues<V>,
    {
        Self {
            factor_paths: Vec::new(),
            primary,
            secondary: vec![],
            enroll_payload: Some(enroll_payload),
            enroll,
            _marker: core::marker::PhantomData,
        }
    }

    /// Returns the index of the factor containing the `DependentProductZipperG` focus
    ///
    /// Returns `0` if the focus is in the primary factor.  The returned value will always be
    /// `zipper.focus_factor() < zipper.factor_count()`.
    pub fn focus_factor(&self) -> usize {
        self.factor_idx(true).map_or(0, |x| x + 1)
    }

    /// Actual focus factor calculation.
    /// Returns a valid index into `self.factor_paths`, truncating to parents if requested.
    fn factor_idx(&self, truncate_up: bool) -> Option<usize> {
        let len = self.path().len();
        let mut factor = self.factor_paths.len().checked_sub(1)?;
        while truncate_up && self.factor_paths[factor] == len {
            factor = factor.checked_sub(1)?;
        }
        Some(factor)
    }

    /// Returns the number of factors composing the `DependentProductZipperG`
    ///
    /// The minimum returned value will be 1 because the primary factor is counted.
    pub fn factor_count(&self) -> usize {
        self.secondary.len() + 1
    }

    /// Returns a slice of the path indices that represent the end-points of the portion of the path from each
    /// factor
    ///
    /// The returned slice will have a length of [`focus_factor`](Self::focus_factor), so the factor
    /// containing the current focus has will not be included.
    ///
    /// Indices will be offsets into the buffer returned by [path](ZipperMoving::path).  To get an offset into
    /// [origin_path](ZipperAbsolutePath::origin_path), add the length of the prefix path from
    /// [root_prefix_path](ZipperAbsolutePath::root_prefix_path).
    pub fn path_indices(&self) -> &[usize] {
        &self.factor_paths
    }

    /// Returns `true` if the last active factor zipper is positioned at the end of a valid path
    /// (i.e. a stitch point to the next zipper)
    fn is_path_end(&self) -> bool {
        if let Some(idx) = self.factor_idx(false) {
            self.secondary[idx].child_count() == 0 && self.secondary[idx].path_exists()
        } else {
            self.primary.child_count() == 0 && self.primary.path_exists()
        }
    }

    /// Remove top factors if they are at root
    fn exit_factors(&mut self) -> bool {
        let len = self.path().len();
        let mut exited = false;
        while self.factor_paths.last() == Some(&len) {
            self.factor_paths.pop();
            self.secondary.pop();
            exited = true;
        }
        exited
    }

    /// Enter factors at current location if we're on the end of the factor's path
    fn enter_factors(&mut self) -> bool {
        let len = self.path().len();
        // enter the next factor if we can
        let mut entered = false;
        if self.is_path_end() {
            // this clone is hideous, but I don't remember how to get rid of it
            let (payload, ret) = self.enroll.clone()(
                self.enroll_payload.take().unwrap(),
                self.path() as _,
                self.secondary.len(),
            );
            let _ = self.enroll_payload.insert(payload);
            if let Some(nz) = ret {
                self.factor_paths.push(len);
                self.secondary.push(nz);
                entered = true;
            }
        }
        entered
    }

    /// A combination between `ascend_until` and `ascend_until_branch`.
    /// if `allow_stop_on_val` is `true`, behaves as `ascend_until`
    fn ascend_cond(&mut self, allow_stop_on_val: bool) -> bool {
        let mut plen = self.path().len();
        loop {
            while self.factor_paths.last() == Some(&plen) {
                self.factor_paths.pop();
            }
            if let Some(idx) = self.factor_idx(false) {
                let zipper = &mut self.secondary[idx];
                let before = zipper.path().len();
                let rv = if allow_stop_on_val {
                    zipper.ascend_until()
                } else {
                    zipper.ascend_until_branch()
                };
                let delta = before - zipper.path().len();
                plen -= delta;
                self.primary.ascend(delta);
                if rv && (self.child_count() != 1 || (allow_stop_on_val && self.is_val())) {
                    return true;
                }
            } else {
                return if allow_stop_on_val {
                    self.primary.ascend_until()
                } else {
                    self.primary.ascend_until_branch()
                };
            }
        }
    }

    /// a combination between `to_next_sibling` and `to_prev_sibling`
    fn to_sibling_byte(&mut self, next: bool) -> bool {
        let Some(&byte) = self.path().last() else {
            return false;
        };
        assert!(self.ascend(1), "must ascend");
        let child_mask = self.child_mask();
        let Some(sibling_byte) =
            (if next { child_mask.next_bit(byte) } else { child_mask.prev_bit(byte) })
        else {
            self.descend_to_byte(byte);
            return false;
        };
        self.descend_to_byte(sibling_byte);
        true
    }
}

impl<
    'trie,
    PrimaryZ,
    SecondaryZ,
    V,
    C,
    F: Clone + for<'a> FnOnce(C, &'a [u8], usize) -> (C, Option<SecondaryZ>),
> ZipperAbsolutePath for DependentProductZipperG<'trie, PrimaryZ, SecondaryZ, V, C, F>
where
    V: Clone + Send + Sync,
    PrimaryZ: ZipperAbsolutePath,
    SecondaryZ: ZipperMoving,
{
    fn origin_path(&self) -> &[u8] {
        self.primary.origin_path()
    }
    fn root_prefix_path(&self) -> &[u8] {
        self.primary.root_prefix_path()
    }
}

impl<
    'trie,
    PrimaryZ,
    SecondaryZ,
    V,
    C,
    F: Clone + for<'a> FnOnce(C, &'a [u8], usize) -> (C, Option<SecondaryZ>),
> ZipperConcrete for DependentProductZipperG<'trie, PrimaryZ, SecondaryZ, V, C, F>
where
    V: Clone + Send + Sync,
    PrimaryZ: ZipperMoving + ZipperConcrete,
    SecondaryZ: ZipperMoving + ZipperConcrete,
{
    fn shared_node_id(&self) -> Option<u64> {
        if let Some(idx) = self.factor_idx(true) {
            self.secondary[idx].shared_node_id()
        } else {
            self.primary.shared_node_id()
        }
    }
    fn is_shared(&self) -> bool {
        if let Some(idx) = self.factor_idx(true) {
            self.secondary[idx].is_shared()
        } else {
            self.primary.is_shared()
        }
    }
}

impl<
    'trie,
    PrimaryZ,
    SecondaryZ,
    V,
    C,
    F: Clone + for<'a> FnOnce(C, &'a [u8], usize) -> (C, Option<SecondaryZ>),
> ZipperPathBuffer for DependentProductZipperG<'trie, PrimaryZ, SecondaryZ, V, C, F>
where
    V: Clone + Send + Sync,
    PrimaryZ: ZipperMoving + ZipperPathBuffer,
    SecondaryZ: ZipperMoving + ZipperPathBuffer,
{
    unsafe fn origin_path_assert_len(&self, len: usize) -> &[u8] {
        unsafe { self.primary.origin_path_assert_len(len) }
    }
    fn prepare_buffers(&mut self) {
        self.primary.prepare_buffers()
    }
    fn reserve_buffers(&mut self, path_len: usize, stack_depth: usize) {
        self.primary.reserve_buffers(path_len, stack_depth)
    }
}

impl<
    'trie,
    PrimaryZ,
    SecondaryZ,
    V,
    C,
    F: Clone + for<'a> FnOnce(C, &'a [u8], usize) -> (C, Option<SecondaryZ>),
> ZipperValues<V> for DependentProductZipperG<'trie, PrimaryZ, SecondaryZ, V, C, F>
where
    V: Clone + Send + Sync,
    PrimaryZ: ZipperMoving + ZipperValues<V>,
    SecondaryZ: ZipperMoving + ZipperValues<V>,
{
    fn val(&self) -> Option<&V> {
        if let Some(idx) = self.factor_idx(true) {
            self.secondary[idx].val()
        } else {
            self.primary.val()
        }
    }
}

impl<
    'trie,
    PrimaryZ,
    SecondaryZ,
    V,
    C,
    F: Clone + for<'a> FnOnce(C, &'a [u8], usize) -> (C, Option<SecondaryZ>),
> ZipperReadOnlyValues<'trie, V> for DependentProductZipperG<'trie, PrimaryZ, SecondaryZ, V, C, F>
where
    V: Clone + Send + Sync,
    PrimaryZ: ZipperMoving + ZipperReadOnlyValues<'trie, V>,
    SecondaryZ: ZipperMoving + ZipperReadOnlyValues<'trie, V>,
{
    fn get_val(&self) -> Option<&'trie V> {
        if let Some(idx) = self.factor_idx(true) {
            self.secondary[idx].get_val()
        } else {
            self.primary.get_val()
        }
    }
}

impl<
    'trie,
    PrimaryZ,
    SecondaryZ,
    V,
    C,
    F: Clone + for<'a> FnOnce(C, &'a [u8], usize) -> (C, Option<SecondaryZ>),
> ZipperReadOnlyConditionalValues<'trie, V>
    for DependentProductZipperG<'trie, PrimaryZ, SecondaryZ, V, C, F>
where
    V: Clone + Send + Sync,
    PrimaryZ: ZipperMoving + ZipperReadOnlyConditionalValues<'trie, V>,
    SecondaryZ: ZipperMoving + ZipperReadOnlyConditionalValues<'trie, V>,
{
    type WitnessT = (PrimaryZ::WitnessT, Vec<SecondaryZ::WitnessT>);
    fn witness<'w>(&self) -> Self::WitnessT {
        let primary_witness = self.primary.witness();
        let secondary_witnesses =
            self.secondary.iter().map(|secondary| secondary.witness()).collect();
        (primary_witness, secondary_witnesses)
    }
    fn get_val_with_witness<'w>(&self, witness: &'w Self::WitnessT) -> Option<&'w V>
    where
        'trie: 'w,
    {
        if let Some(idx) = self.factor_idx(true) {
            self.secondary[idx].get_val_with_witness(&witness.1[idx])
        } else {
            self.primary.get_val_with_witness(&witness.0)
        }
    }
}

impl<
    'trie,
    PrimaryZ,
    SecondaryZ,
    V,
    C,
    F: Clone + for<'a> FnOnce(C, &'a [u8], usize) -> (C, Option<SecondaryZ>),
> Zipper for DependentProductZipperG<'trie, PrimaryZ, SecondaryZ, V, C, F>
where
    V: Clone + Send + Sync,
    PrimaryZ: ZipperMoving + Zipper,
    SecondaryZ: ZipperMoving + Zipper,
{
    fn path_exists(&self) -> bool {
        if let Some(idx) = self.factor_idx(true) {
            self.secondary[idx].path_exists()
        } else {
            self.primary.path_exists()
        }
    }
    fn is_val(&self) -> bool {
        if let Some(idx) = self.factor_idx(true) {
            self.secondary[idx].is_val()
        } else {
            self.primary.is_val()
        }
    }
    fn child_count(&self) -> usize {
        if let Some(idx) = self.factor_idx(false) {
            self.secondary[idx].child_count()
        } else {
            self.primary.child_count()
        }
    }
    fn child_mask(&self) -> ByteMask {
        if let Some(idx) = self.factor_idx(false) {
            self.secondary[idx].child_mask()
        } else {
            self.primary.child_mask()
        }
    }
}

impl<
    'trie,
    PrimaryZ,
    SecondaryZ,
    V,
    C,
    F: Clone + for<'a> FnOnce(C, &'a [u8], usize) -> (C, Option<SecondaryZ>),
> ZipperMoving for DependentProductZipperG<'trie, PrimaryZ, SecondaryZ, V, C, F>
where
    V: Clone + Send + Sync,
    PrimaryZ: ZipperMoving,
    SecondaryZ: ZipperMoving,
{
    fn at_root(&self) -> bool {
        self.path().is_empty()
    }
    fn reset(&mut self) {
        self.factor_paths.clear();
        for secondary in &mut self.secondary {
            secondary.reset();
        }
        self.primary.reset();
    }
    #[inline]
    fn path(&self) -> &[u8] {
        self.primary.path()
    }
    fn val_count(&self) -> usize {
        unimplemented!("method will probably get removed")
    }
    fn descend_to_existing<K: AsRef<[u8]>>(&mut self, path: K) -> usize {
        let mut path = path.as_ref();
        let mut descended = 0;
        'descend: while !path.is_empty() {
            self.enter_factors();
            let good;
            if let Some(idx) = self.factor_idx(false) {
                good = self.secondary[idx].descend_to_existing(path);
                self.primary.descend_to(&path[..good]);
            } else {
                good = self.primary.descend_to_existing(path);
            };
            if good == 0 {
                break 'descend;
            }
            descended += good;
            path = &path[good..];
        }
        self.enter_factors();
        descended
    }
    fn descend_to<K: AsRef<[u8]>>(&mut self, path: K) {
        let path = path.as_ref();
        let good = self.descend_to_existing(path);
        if good == path.len() {
            return;
        }
        let rest = &path[good..];
        if let Some(idx) = self.factor_idx(false) {
            self.secondary[idx].descend_to(rest);
        }

        self.primary.descend_to(rest);
    }
    #[inline]
    fn descend_to_byte(&mut self, k: u8) {
        self.descend_to([k])
    }
    fn descend_indexed_byte(&mut self, child_idx: usize) -> bool {
        let mask = self.child_mask();
        let Some(byte) = mask.indexed_bit::<true>(child_idx) else {
            return false;
        };
        self.descend_to_byte(byte);
        true
    }
    #[inline]
    fn descend_first_byte(&mut self) -> bool {
        self.descend_indexed_byte(0)
    }
    fn descend_until(&mut self) -> bool {
        let mut moved = false;
        self.enter_factors();
        while self.child_count() == 1 {
            moved |= if let Some(idx) = self.factor_idx(false) {
                let zipper = &mut self.secondary[idx];
                let before = zipper.path().len();
                let rv = zipper.descend_until();
                let path = zipper.path();
                if path.len() > before {
                    self.primary.descend_to(&path[before..]);
                }
                rv
            } else {
                self.primary.descend_until()
            };
            self.enter_factors();
            if self.is_val() {
                break;
            }
        }
        moved
    }
    #[inline]
    fn to_next_sibling_byte(&mut self) -> bool {
        self.to_sibling_byte(true)
    }
    #[inline]
    fn to_prev_sibling_byte(&mut self) -> bool {
        self.to_sibling_byte(false)
    }
    fn ascend(&mut self, mut steps: usize) -> bool {
        while steps > 0 {
            self.exit_factors();
            if let Some(idx) = self.factor_idx(false) {
                let len = self.path().len() - self.factor_paths[idx];
                let delta = len.min(steps);
                self.secondary[idx].ascend(delta);
                self.primary.ascend(delta);
                steps -= delta;
            } else {
                return self.primary.ascend(steps);
            }
        }
        true
    }
    #[inline]
    fn ascend_byte(&mut self) -> bool {
        self.ascend(1)
    }
    #[inline]
    fn ascend_until(&mut self) -> bool {
        self.ascend_cond(true)
    }
    #[inline]
    fn ascend_until_branch(&mut self) -> bool {
        self.ascend_cond(false)
    }
}

impl<
    'trie,
    PrimaryZ,
    SecondaryZ,
    V,
    C,
    F: Clone + for<'a> FnOnce(C, &'a [u8], usize) -> (C, Option<SecondaryZ>),
> ZipperIteration for DependentProductZipperG<'trie, PrimaryZ, SecondaryZ, V, C, F>
where
    V: Clone + Send + Sync,
    PrimaryZ: ZipperIteration,
    SecondaryZ: ZipperIteration,
{
} //Use the default impl for all methods

impl<
    'trie,
    PrimaryZ,
    SecondaryZ,
    V: Clone + Send + Sync + Unpin,
    C,
    F: Clone + for<'a> FnOnce(C, &'a [u8], usize) -> (C, Option<SecondaryZ>),
> ZipperSubtries<V, GlobalAlloc> for DependentProductZipperG<'trie, PrimaryZ, SecondaryZ, V, C, F>
where
    V: Clone + Send + Sync,
    PrimaryZ: ZipperMoving + ZipperValues<V>,
    SecondaryZ: ZipperMoving + ZipperValues<V>,
{
    fn native_subtries(&self) -> bool {
        false
    }
    fn try_make_map(&self) -> Option<PathMap<V, GlobalAlloc>> {
        None
    }
    fn trie_ref(&self) -> Option<TrieRef<'_, V, GlobalAlloc>> {
        None
    }
    fn alloc(&self) -> GlobalAlloc {
        global_alloc()
    }
}

#[cfg(all(test, feature = "pathmap-internal-tests"))]
mod tests {
    use super::PathMap;
    use super::zipper::*;

    #[test]
    fn dep_test_1() {
        let mut btm = PathMap::new();
        let rs = [
            "arrow",
            "bow",
            "cannon",
            "roman",
            "romane",
            "romanus",
            "romulus",
            "rubens",
            "ruber",
            "rubicon",
            "rubicundus",
            "rom'i",
        ];
        rs.iter().enumerate().for_each(|(i, r)| {
            btm.set_val_at(r.as_bytes(), i);
        });

        let mut dpz =
            DependentProductZipperG::new_enroll(btm.into_read_zipper(&[]), (), |_, _, c| {
                if c == 0 {
                    ((), Some(PathMap::single(".postfix", 0).into_read_zipper(&[])))
                } else {
                    ((), None)
                }
            });

        let mut full = String::new();
        while dpz.to_next_val() {
            if dpz.child_count() == 0 {
                full.push_str(std::str::from_utf8(dpz.path()).unwrap());
                full.push('\n');
            }
        }
        assert_eq!(
            full,
            "arrow.postfix
bow.postfix
cannon.postfix
rom'i.postfix
romane.postfix
romanus.postfix
romulus.postfix
rubens.postfix
ruber.postfix
rubicon.postfix
rubicundus.postfix
"
        )
    }

    #[test]
    fn dep_test_2() {
        let mut btm = PathMap::new();
        let rs = [
            "arrow",
            "bow",
            "cannon",
            "roman",
            "romane",
            "romanus",
            "romulus",
            "rubens",
            "ruber",
            "rubicon",
            "rubicundus",
            "rom'i",
        ];
        rs.iter().enumerate().for_each(|(i, r)| {
            btm.set_val_at(r.as_bytes(), i);
        });

        let mut dpz =
            DependentProductZipperG::new_enroll(btm.into_read_zipper(&[]), (), |_, p, c| {
                if c == 0 {
                    ((), Some(PathMap::single(p, 0).into_read_zipper(&[])))
                } else {
                    ((), None)
                }
            });

        let mut full = String::new();
        while dpz.to_next_val() {
            if dpz.child_count() == 0 {
                full.push_str(std::str::from_utf8(dpz.path()).unwrap());
                full.push('\n');
            }
        }
        assert_eq!(
            full,
            "arrowarrow
bowbow
cannoncannon
rom'irom'i
romaneromane
romanusromanus
romulusromulus
rubensrubens
ruberruber
rubiconrubicon
rubicundusrubicundus
"
        )
    }
}
