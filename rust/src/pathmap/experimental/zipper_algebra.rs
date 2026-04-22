//! Experimental zipper algebra operations.
//! 
//! Contains `ZipperAlgebraExt`, `zipper_join`, `zipper_meet`, and `zipper_subtract`.
//! These are test stubs and may contain incomplete implementations.

#![allow(unused_variables, dead_code)]

use super::super::{
    alloc::{Allocator, GlobalAlloc},
    ring::{AlgebraicResult, COUNTER_IDENT, DistributiveLattice, Lattice, SELF_IDENT},
    utils::ByteMask,
    zipper::{
        ReadZipperUntracked, Zipper, ZipperInfallibleSubtries, ZipperMoving, ZipperValues,
        ZipperWriting,
    },
};

/// Extension trait providing algebraic merge operations on radix-256 trie zippers.
///
/// This trait exposes high-level operations such as [`join`](Self::join),
/// [`meet`](Self::meet), and [`subtract`](Self::subtract) directly on zipper
/// instances, allowing them to be invoked in a method-oriented style:
///
/// ```ignore
/// lhs.join(&mut rhs, &mut out);
/// lhs.meet(&mut rhs, &mut out);
/// lhs.subtract(&mut rhs, &mut out);
/// ```
///
/// # Overview
///
/// All operations are implemented as *lockstep traversals* over two tries using
/// zipper navigation. They exploit the lexicographic ordering of child edges
/// (bytes `0..=255`) to efficiently merge, intersect, or subtract subtries
/// without visiting unrelated regions.
///
/// Each method delegates to a corresponding free function ([`zipper_join`],
/// [`zipper_meet`], [`zipper_subtract`]), preserving their performance
/// characteristics and semantics.
///
/// # Semantics
///
/// The provided operations correspond to common lattice and set-like behaviors:
///
/// - [`join`](Self::join): least upper bound (union-like merge),
/// - [`meet`](Self::meet): greatest lower bound (intersection),
/// - [`subtract`](Self::subtract): asymmetric difference (`lhs \ rhs`).
///
/// All operations write their result into a separate output zipper implementing
/// [`ZipperWriting`].
///
/// # Notes
///
/// - The operations are *asymmetric* with respect to the receiver (`self`) and
///   the `rhs` argument, which is particularly relevant for
///   [`subtract`](Self::subtract).
/// - The output zipper is written incrementally during traversal and must be
///   positioned consistently with the input zippers.
///
/// # See also
///
/// - [`zipper_join`]
/// - [`zipper_meet`]
/// - [`zipper_subtract`]
pub trait ZipperAlgebraExt<V: Clone + Send + Sync, A: Allocator = GlobalAlloc>:
    ZipperInfallibleSubtries<V, A> + ZipperMoving + Sized
{
    #[inline]
    fn join<ZR, Out>(&mut self, rhs: &mut ZR, out: &mut Out)
    where
        V: Lattice,
        ZR: ZipperInfallibleSubtries<V, A> + ZipperMoving,
        Out: ZipperWriting<V, A>,
    {
        zipper_join(self, rhs, out);
    }

    #[inline]
    fn meet<ZR, Out>(&mut self, rhs: &mut ZR, out: &mut Out)
    where
        V: Lattice,
        ZR: ZipperInfallibleSubtries<V, A> + ZipperMoving,
        Out: ZipperWriting<V, A>,
    {
        zipper_meet(self, rhs, out);
    }

    #[inline]
    fn subtract<ZR, Out>(&mut self, rhs: &mut ZR, out: &mut Out)
    where
        V: DistributiveLattice,
        ZR: ZipperInfallibleSubtries<V, A> + ZipperMoving,
        Out: ZipperWriting<V, A>,
    {
        zipper_subtract(self, rhs, out);
    }
}

impl<V: Clone + Send + Sync + Unpin, A: Allocator> ZipperAlgebraExt<V, A>
    for ReadZipperUntracked<'_, '_, V, A>
{
}

/// Performs an ordered join (least upper bound) of two radix-256 tries using zipper traversal.
///
/// This function merges two tries by simultaneously traversing them in lexicographic order,
/// exploiting the ordering of child edges (bytes `0..=255`) to avoid unnecessary descent.
///
/// # Value semantics
///
/// When both tries contain a value at the same key, they are merged using the [`Lattice`]
/// operation [`Lattice::pjoin`]. The result is interpreted as follows:
///
/// - [`AlgebraicResult::None`] → no value is written,
/// - [`AlgebraicResult::Identity`] → one of the inputs is reused (based on identity mask),
/// - [`AlgebraicResult::Element`] → the computed value is written.
///
/// If only one side contains a value, it is propagated unchanged.
///
/// # Complexity
///
/// Let:
/// - `h` be the maximum key length,
/// - `d` be the size of overlapping subtries,
/// - `f` be the size of the frontier (distinct child edges encountered).
///
/// Then:
///
/// - Best case (disjoint tries): **O(h)**
/// - Typical case: **O(h + f)**
/// - Worst case (identical structure): **O(n)**
///
/// The algorithm avoids visiting entire disjoint subtrees by grafting them directly.
///
/// # Notes
///
/// This is a stackless depth-first traversal implemented via zippers.
/// The outer loop manages ascent (unwinding), while the inner loop
/// exhausts the current node's children.
///
pub fn zipper_join<V, ZL, ZR, Out, A>(lhs: &mut ZL, rhs: &mut ZR, out: &mut Out)
where
    V: Lattice + Clone + Send + Sync,
    A: Allocator,
    ZL: ZipperInfallibleSubtries<V, A> + ZipperMoving,
    ZR: ZipperInfallibleSubtries<V, A> + ZipperMoving,
    Out: ZipperWriting<V, A>,
{
    fn join_values<V, ZL, ZR, Out, A>(lhs: &ZL, rhs: &ZR, out: &mut Out)
    where
        V: Lattice + Clone + Send + Sync,
        A: Allocator,
        ZL: ZipperValues<V>,
        ZR: ZipperValues<V>,
        Out: ZipperWriting<V, A>,
    {
        if let Some(lv) = lhs.val() {
            if let Some(rv) = rhs.val() {
                match lv.pjoin(rv) {
                    AlgebraicResult::None => {}
                    AlgebraicResult::Identity(mask) => {
                        if mask & SELF_IDENT != 0 {
                            out.set_val(lv.clone());
                        } else if mask & COUNTER_IDENT != 0 {
                            out.set_val(rv.clone());
                        }
                    }
                    AlgebraicResult::Element(v) => {
                        out.set_val(v);
                    }
                }
            } else {
                out.set_val(lv.clone());
            }
        } else if let Some(rv) = rhs.val() {
            out.set_val(rv.clone());
        }
    }

    // join root values before descending
    join_values(lhs, rhs, out);

    let mut k = 0;
    let mut lhs_mask = lhs.child_mask();
    let mut rhs_mask = rhs.child_mask();
    let mut lhs_idx = 0;
    let mut rhs_idx = 0;

    // At each node, the algorithm treats the sets of child edges of `lhs` and `rhs` as two
    // sorted sequences and performs a merge-like traversal:
    //
    // - If a range of edges exists only in one side, the corresponding subtries are *grafted*
    //   into the output without further traversal.
    // - If both sides contain the same edge, the algorithm descends into that child,
    //   recursively merging the corresponding subtries.
    // - Descent is simulated iteratively using zipper movement (`descend_to_byte` /
    //   `ascend_byte`) and an explicit depth counter (`k`).
    'ascend: loop {
        'merge_level: loop {
            let lhs_next = lhs_mask.indexed_bit::<true>(lhs_idx as usize);
            let rhs_next = rhs_mask.indexed_bit::<true>(rhs_idx as usize);

            match lhs_next {
                Some(lhs_byte) => match rhs_next {
                    Some(rhs_byte) if lhs_byte < rhs_byte => {
                        out.graft_children(lhs, ByteMask::from_range(lhs_byte..rhs_byte));
                        lhs_idx = lhs_mask.index_of(rhs_byte);
                    }
                    Some(rhs_byte) if lhs_byte > rhs_byte => {
                        out.graft_children(rhs, ByteMask::from_range(rhs_byte..lhs_byte));
                        rhs_idx = rhs_mask.index_of(lhs_byte);
                    }
                    Some(rhs_byte) => {
                        // equal → descend
                        out.descend_to_byte(lhs_byte);

                        lhs.descend_to_byte(lhs_byte);
                        rhs.descend_to_byte(lhs_byte);

                        join_values(lhs, rhs, out);

                        lhs_mask = lhs.child_mask();
                        rhs_mask = rhs.child_mask();

                        lhs_idx = 0;
                        rhs_idx = 0;

                        k += 1;
                        continue 'merge_level;
                    }
                    None => {
                        out.graft_children(lhs, ByteMask::from_range(lhs_byte..));
                        break 'merge_level;
                    }
                },
                None => match rhs_next {
                    Some(rhs_byte) => {
                        out.graft_children(rhs, ByteMask::from_range(rhs_byte..));
                        break 'merge_level;
                    }
                    None => break 'merge_level,
                },
            }
        }

        // If we are at root and no deeper recursion pending, we're done
        if k == 0 {
            break 'ascend;
        }

        let byte_from = *lhs.path().last().expect("non-empty path when k > 0");

        rhs.ascend_byte();
        rhs_mask = rhs.child_mask();
        rhs_idx = rhs_mask.index_of(byte_from) + 1;

        lhs.ascend_byte();
        lhs_mask = lhs.child_mask();
        lhs_idx = lhs_mask.index_of(byte_from) + 1;

        out.ascend_byte();
        k -= 1;
    }
}

/// Performs an ordered meet (greatest lower bound) of two radix-256 tries using zipper traversal.
///
/// This function intersects two tries by simultaneously traversing them in lexicographic order,
/// exploiting the ordering of child edges (bytes `0..=255`) to avoid unnecessary descent.
///
/// # Value semantics
///
/// When both tries contain a value at the same key, they are merged using the [`Lattice`]
/// operation [`Lattice::pmeet`]. The result is interpreted as follows:
///
/// - [`AlgebraicResult::None`] → no value is written,
/// - [`AlgebraicResult::Identity`] → one of the inputs is reused (based on identity mask),
/// - [`AlgebraicResult::Element`] → the computed value is written.
///
/// If a value is present on only one side, it is discarded.
///
/// # Complexity
///
/// Let:
/// - `h` be the maximum key length,
/// - `d` be the size of overlapping subtries,
/// - `f` be the size of the shared frontier (common child edges).
///
/// Then:
///
/// - Best case (disjoint tries): **O(h)**
/// - Typical case: **O(h + f)**
/// - Worst case (identical structure): **O(n)**
///
/// The algorithm avoids visiting disjoint subtrees by skipping them entirely.
///
/// # Notes
///
/// This is a stackless depth-first traversal implemented via zippers.
/// The outer loop manages ascent (unwinding), while the inner loop
/// processes only shared child edges at each node.
///
/// Unlike [`zipper_join`], this operation descends exclusively into edges
/// present in *both* tries, forming the intersection of their structures.
///
pub fn zipper_meet<V, ZL, ZR, Out, A>(lhs: &mut ZL, rhs: &mut ZR, out: &mut Out)
where
    V: Lattice + Clone + Send + Sync,
    A: Allocator,
    ZL: ZipperInfallibleSubtries<V, A> + ZipperMoving,
    ZR: ZipperInfallibleSubtries<V, A> + ZipperMoving,
    Out: ZipperWriting<V, A>,
{
    fn meet_values<V, ZL, ZR, Out, A>(lhs: &ZL, rhs: &ZR, out: &mut Out)
    where
        V: Lattice + Clone + Send + Sync,
        A: Allocator,
        ZL: ZipperValues<V>,
        ZR: ZipperValues<V>,
        Out: ZipperWriting<V, A>,
    {
        if let Some(lv) = lhs.val() {
            if let Some(rv) = rhs.val() {
                match lv.pmeet(rv) {
                    AlgebraicResult::None => {}
                    AlgebraicResult::Identity(mask) => {
                        if mask & SELF_IDENT != 0 {
                            out.set_val(lv.clone());
                        } else if mask & COUNTER_IDENT != 0 {
                            out.set_val(rv.clone());
                        }
                    }
                    AlgebraicResult::Element(v) => {
                        out.set_val(v);
                    }
                }
            }
        }
    }

    // meet root values before descending
    meet_values(lhs, rhs, out);

    let mut k = 0;
    let mut lhs_mask = lhs.child_mask();
    let mut rhs_mask = rhs.child_mask();
    let mut lhs_idx = 0;
    let mut rhs_idx = 0;

    // At each node, the algorithm treats the sets of child edges of `lhs` and `rhs` as two
    // sorted sequences and performs a merge-like traversal:
    //
    // - If a range of edges exists only in one side, the corresponding subtries are skipped
    //   without further traversal.
    // - If both sides contain the same edge, the algorithm descends into that child,
    //   recursively merging the corresponding subtries.
    // - Descent is simulated iteratively using zipper movement (`descend_to_byte` /
    //   `ascend_byte`) and an explicit depth counter (`k`).
    'ascend: loop {
        'merge_level: loop {
            let lhs_next = lhs_mask.indexed_bit::<true>(lhs_idx as usize);
            let rhs_next = rhs_mask.indexed_bit::<true>(rhs_idx as usize);

            match lhs_next {
                Some(lhs_byte) => match rhs_next {
                    Some(rhs_byte) if lhs_byte < rhs_byte => {
                        // skip lhs-only range
                        lhs_idx = lhs_mask.index_of(rhs_byte);
                    }
                    Some(rhs_byte) if lhs_byte > rhs_byte => {
                        // skip rhs-only range
                        rhs_idx = rhs_mask.index_of(lhs_byte);
                    }
                    Some(rhs_byte) => {
                        // equal → descend
                        out.descend_to_byte(lhs_byte);

                        lhs.descend_to_byte(lhs_byte);
                        rhs.descend_to_byte(lhs_byte);

                        meet_values(lhs, rhs, out);

                        lhs_mask = lhs.child_mask();
                        rhs_mask = rhs.child_mask();

                        lhs_idx = 0;
                        rhs_idx = 0;

                        k += 1;
                        continue 'merge_level;
                    }
                    None => break 'merge_level,
                },
                None => break 'merge_level,
            }
        }

        if k == 0 {
            break 'ascend;
        }

        let byte_from = *lhs.path().last().expect("non-empty path when k > 0");

        rhs.ascend_byte();
        rhs_mask = rhs.child_mask();
        rhs_idx = rhs_mask.index_of(byte_from) + 1;

        lhs.ascend_byte();
        lhs_mask = lhs.child_mask();
        lhs_idx = lhs_mask.index_of(byte_from) + 1;

        out.ascend_byte();
        k -= 1;
    }
}

/// Performs an ordered subtraction (set difference, `lhs \ rhs`) of two radix-256 tries
/// using zipper traversal.
///
/// This function subtracts the structure and values of `rhs` from `lhs` by simultaneously
/// traversing both tries in lexicographic order. It exploits the ordering of child edges
/// (bytes `0..=255`) to avoid unnecessary descent.
///
/// # Value semantics
///
/// Values are handled asymmetrically:
///
/// - If only `lhs` contains a value, it is preserved unchanged.
/// - If only `rhs` contains a value, it is ignored.
/// - If both tries contain a value at the same key, they are comined using the [`DistributiveLattice`]
///   operation [`DistributiveLattice::psubtract`]. The result is interpreted as follows:
///
///   - [`AlgebraicResult::None`] → no value is written,
///   - [`AlgebraicResult::Identity`] → only lhs is preserved (based on identity mask),
///   - [`AlgebraicResult::Element`] → the computed value is written.
///
/// # Complexity
///
/// Let:
/// - `h` be the maximum key length,
/// - `d` be the size of overlapping subtries,
/// - `f` be the size of the frontier (distinct child edges encountered).
///
/// Then:
///
/// - Best case (disjoint tries): **O(h)**
/// - Typical case: **O(h + f)**
/// - Worst case (identical structure): **O(n)**
///
/// The algorithm avoids visiting disjoint subtrees by grafting `lhs`-only regions
/// and skipping `rhs`-only regions entirely.
///
/// # Notes
///
/// This is a stackless depth-first traversal implemented via zippers.
/// The outer loop manages ascent (unwinding), while the inner loop
/// exhausts the current node's children.
///
/// Unlike [`zipper_join`] and [`zipper_meet`], this operation is asymmetric:
/// it preserves only the parts of `lhs` that are not overlapped by `rhs`,
/// effectively removing any shared structure or values.
///
pub fn zipper_subtract<V, ZL, ZR, Out, A>(lhs: &mut ZL, rhs: &mut ZR, out: &mut Out)
where
    V: DistributiveLattice + Clone + Send + Sync,
    A: Allocator,
    ZL: ZipperInfallibleSubtries<V, A> + ZipperMoving,
    ZR: ZipperInfallibleSubtries<V, A> + ZipperMoving,
    Out: ZipperWriting<V, A>,
{
    fn combine_values<V, ZL, ZR, Out, A>(lhs: &ZL, rhs: &ZR, out: &mut Out)
    where
        V: DistributiveLattice + Clone + Send + Sync,
        A: Allocator,
        ZL: ZipperValues<V>,
        ZR: ZipperValues<V>,
        Out: ZipperWriting<V, A>,
    {
        if let Some(lv) = lhs.val() {
            if let Some(rv) = rhs.val() {
                match lv.psubtract(rv) {
                    AlgebraicResult::None => {}
                    AlgebraicResult::Identity(mask) => {
                        if mask & SELF_IDENT != 0 {
                            out.set_val(lv.clone());
                        }
                    }
                    AlgebraicResult::Element(v) => {
                        out.set_val(v);
                    }
                }
            } else {
                // lhs-only → keep
                out.set_val(lv.clone());
            }
        }
    }

    // combine root values before descending
    combine_values(lhs, rhs, out);

    let mut k = 0;
    let mut lhs_mask = lhs.child_mask();
    let mut rhs_mask = rhs.child_mask();
    let mut lhs_idx = 0;
    let mut rhs_idx = 0;

    // The traversal follows a merge-like strategy over ordered child edges:
    //
    // - Subtries that exist only in `lhs` are *grafted* directly into the output.
    // - Subtries that exist only in `rhs` are skipped.
    // - When both sides contain the same edge, the algorithm descends into that child
    //   and continues subtracting recursively.
    'ascend: loop {
        'merge_level: loop {
            let lhs_next = lhs_mask.indexed_bit::<true>(lhs_idx as usize);
            let rhs_next = rhs_mask.indexed_bit::<true>(rhs_idx as usize);

            match lhs_next {
                Some(lhs_byte) => match rhs_next {
                    Some(rhs_byte) if lhs_byte < rhs_byte => {
                        out.graft_children(lhs, ByteMask::from_range(lhs_byte..rhs_byte));
                        lhs_idx = lhs_mask.index_of(rhs_byte);
                    }
                    Some(rhs_byte) if lhs_byte > rhs_byte => {
                        // skip rhs-only range
                        rhs_idx = rhs_mask.index_of(lhs_byte);
                    }
                    Some(rhs_byte) => {
                        // equal → descend
                        out.descend_to_byte(lhs_byte);

                        lhs.descend_to_byte(lhs_byte);
                        rhs.descend_to_byte(lhs_byte);

                        combine_values(lhs, rhs, out);

                        lhs_mask = lhs.child_mask();
                        rhs_mask = rhs.child_mask();

                        lhs_idx = 0;
                        rhs_idx = 0;

                        k += 1;
                        continue 'merge_level;
                    }
                    None => {
                        out.graft_children(lhs, ByteMask::from_range(lhs_byte..));
                        break 'merge_level;
                    }
                },
                None => break 'merge_level,
            }
        }

        if k == 0 {
            break 'ascend;
        }

        let byte_from = *lhs.path().last().expect("non-empty path when k > 0");

        rhs.ascend_byte();
        rhs_mask = rhs.child_mask();
        rhs_idx = rhs_mask.index_of(byte_from) + 1;

        lhs.ascend_byte();
        lhs_mask = lhs.child_mask();
        lhs_idx = lhs_mask.index_of(byte_from) + 1;

        out.ascend_byte();
        k -= 1;
    }
}

#[cfg(all(test, feature = "pathmap-internal-tests"))]
mod tests {
    use super::super::{
        PathMap,
        zipper::{ReadZipperUntracked, WriteZipperUntracked},
    };

    type Paths = &'static [(&'static [u8], u64)];
    type Test = (Paths, Paths);

    fn mk_test(test: &Test) -> (PathMap<u64>, PathMap<u64>) {
        (PathMap::from_iter(test.0), PathMap::from_iter(test.1))
    }

    fn check<
        'x,
        T: IntoIterator<Item = &'x (&'x [u8], u64)>,
        F: for<'a> FnOnce(
            &mut ReadZipperUntracked<'a, 'x, u64>,
            &mut ReadZipperUntracked<'a, 'x, u64>,
            &mut WriteZipperUntracked<'a, 'x, u64>,
        ),
    >(
        test: &Test,
        expected: T,
        op: F,
    ) {
        let (left, right) = mk_test(test);

        let mut result = PathMap::new();

        let mut lhs = left.read_zipper();
        let mut rhs = right.read_zipper();
        let mut out = result.write_zipper();

        op(&mut lhs, &mut rhs, &mut out);

        let mut result_copy = result.clone();

        for (expected_path, expected_val) in expected {
            assert!(
                result.path_exists_at(expected_path),
                "Path {expected_path:#?} does NOT exist in {result:#?}"
            );
            let actual_val = result.get_val_at(expected_path);
            assert_eq!(
                actual_val,
                Some(expected_val),
                "Value at {expected_path:#?}"
            );

            result_copy.remove_val_at(expected_path, true);
        }

        assert!(
            result_copy.is_empty(),
            "Paths unaccounted for are present in the result: {result_copy:#?}"
        );
    }

    const DISJOINT_PATHS: Test = (
        &[
            (&[0x00], 0),
            (&[0x00, 0x00], 1),
            (&[0x00, 0x00, 0x00], 2),
            (&[0x00, 0x00, 0x00, 0x00], 3),
        ],
        &[
            (&[0xFF], 0),
            (&[0xFF, 0x00], 1),
            (&[0xFF, 0x00, 0x00], 2),
            (&[0xFF, 0x00, 0x00, 0x00], 3),
        ],
    );

    const PATHS_WITH_SHARED_PREFIX: Test = (
        &[(b"aaaaa0", 0), (b"bbbbbbbb0", 1)],
        &[(b"aaaaa1", 0), (b"bbbbb1", 1), (b"bbbbbbbb1", 2)],
    );

    const INTERLEAVING_PATHS: Test = (
        &[(&[0], 0), (&[2], 1), (&[4], 2), (&[6], 3)],
        &[(&[1], 0), (&[3], 1), (&[5], 2), (&[7], 3)],
    );

    const ONE_SIDED_PATHS: Test = (
        &[
            (&[0x00], 0),
            (&[0x00, 0x01], 1),
            (&[0x00, 0x01, 0x02], 2),
            (&[0x00, 0x01, 0x02, 0x03], 3),
            (&[0x01], 4),
            (&[0x01, 0x02], 5),
            (&[0x01, 0x02, 0x03], 6),
            (&[0x01, 0x02, 0x03, 0x04], 7),
            (&[0x01, 0x02, 0x03, 0x04, 0x05], 8),
            (&[0x01, 0x02, 0x03, 0x04, 0x05, 0x06], 9),
        ],
        &[
            (&[0x00], 0),
            (&[0x00, 0x01, 0x02, 0x03], 1),
            (&[0x01, 0x02, 0x03, 0x04, 0x05], 2),
        ],
    );

    const ALMOST_IDENTICAL_PATHS: Test = (
        &[
            (b"abcdefg", 0),
            (b"hijklmnop", 1),
            (b"qrstuwvxyz", 2),
            (b"0", 3),
            (b"1", 4),
            (b"2", 5),
            (b"3", 6),
            (b"4", 7),
            (b"5", 8),
            (b"6789", 9),
        ],
        &[
            (b"abcdefg", 0),
            (b"qrstuwvxyz", 2),
            (b"0", 3),
            (b"1", 4),
            (b"4", 7),
            (b"5", 8),
            (b"6789", 9),
        ],
    );

    const LHS_EMPTY: Test = (&[], &[(&[1], 0), (&[2], 1)]);

    const RHS_EMPTY: Test = (&[(&[1], 0), (&[2], 1)], &[]);

    const PATHS_WITH_SAME_PREFIX_DIFFERENT_CHILDREN: Test = (
        &[
            (&[1, 2, 3], 0),
            (&[1, 2, 3, 4], 1),
            (&[1, 2, 3, 10, 11, 12], 2),
        ],
        &[
            (&[1, 2, 3], 10),
            (&[1, 2, 3, 5], 11),
            (&[1, 2, 3, 10, 11, 0], 12),
        ],
    );

    const ZIGZAG_PATHS: Test = (
        &[
            (&[1, 1], 0),
            (&[2], 1),
            (&[2, 1], 2),
            (&[3], 3),
            (&[3, 2, 1], 4),
            (&[4], 4),
            (&[4, 3, 2, 1], 5),
        ],
        &[
            (&[1], 0),
            (&[1, 2], 1),
            (&[2, 1], 2),
            (&[3], 3),
            (&[3, 4], 4),
            (&[4, 3], 5),
        ],
    );

    const PATHS_WITH_ROOT_VALS_AND_CHILDREN: Test =
        (&[(&[], 1), (&[1], 10)], &[(&[], 2), (&[1], 20)]);

    mod join {
        use super::*;
        use super::experimental::zipper_algebra::{ZipperAlgebraExt, zipper_join};

        #[test]
        fn test_disjoint() {
            check(
                &DISJOINT_PATHS,
                &[DISJOINT_PATHS.0, DISJOINT_PATHS.1].concat(),
                |lhs, rhs, out| lhs.join(rhs, out),
            );
        }

        #[test]
        fn test_deep_shared_prefix_then_split() {
            check(
                &PATHS_WITH_SHARED_PREFIX,
                &[PATHS_WITH_SHARED_PREFIX.0, PATHS_WITH_SHARED_PREFIX.1].concat(),
                |lhs, rhs, out| lhs.join(rhs, out),
            );
        }

        #[test]
        fn test_interleaving_paths() {
            check(
                &INTERLEAVING_PATHS,
                &[INTERLEAVING_PATHS.0, INTERLEAVING_PATHS.1].concat(),
                |lhs, rhs, out| lhs.join(rhs, out),
            );
        }

        #[test]
        fn test_one_side_empty_at_many_levels() {
            check(&ONE_SIDED_PATHS, ONE_SIDED_PATHS.0, |lhs, rhs, out| {
                lhs.join(rhs, out)
            });
        }

        #[test]
        fn test_almost_identical_paths() {
            check(
                &ALMOST_IDENTICAL_PATHS,
                ALMOST_IDENTICAL_PATHS.0,
                |lhs, rhs, out| lhs.join(rhs, out),
            );
        }

        #[test]
        fn test_one_side_empty() {
            check(&LHS_EMPTY, LHS_EMPTY.1, |lhs, rhs, out| lhs.join(rhs, out));
            check(&RHS_EMPTY, RHS_EMPTY.0, |lhs, rhs, out| lhs.join(rhs, out));
        }

        #[test]
        fn test_exact_overlap_divergent_subtries() {
            let expected: Paths = &[
                (&[1, 2, 3], 0),
                (&[1, 2, 3, 4], 1),
                (&[1, 2, 3, 5], 11),
                (&[1, 2, 3, 10, 11, 0], 12),
                (&[1, 2, 3, 10, 11, 12], 2),
            ];
            check(
                &PATHS_WITH_SAME_PREFIX_DIFFERENT_CHILDREN,
                expected,
                |lhs, rhs, out| lhs.join(rhs, out),
            );
        }

        #[test]
        fn test_zigzag() {
            check(
                &ZIGZAG_PATHS,
                &[ZIGZAG_PATHS.0, ZIGZAG_PATHS.1].concat(),
                |lhs, rhs, out| lhs.join(rhs, out),
            );
        }

        #[test]
        fn test_root_values() {
            check(
                &PATHS_WITH_ROOT_VALS_AND_CHILDREN,
                PATHS_WITH_ROOT_VALS_AND_CHILDREN.0,
                |lhs, rhs, out| lhs.join(rhs, out),
            );
        }
    }

    mod meet {
        use super::*;
        use super::experimental::zipper_algebra::{ZipperAlgebraExt, zipper_meet};

        #[test]
        fn test_disjoint() {
            check(&DISJOINT_PATHS, [], |lhs, rhs, out| {
                lhs.meet(rhs, out);
            });
        }

        #[test]
        fn test_deep_shared_prefix_then_split() {
            check(&PATHS_WITH_SHARED_PREFIX, [], |lhs, rhs, out| {
                lhs.meet(rhs, out);
            });
        }

        #[test]
        fn test_interleaving_paths() {
            check(&INTERLEAVING_PATHS, [], |lhs, rhs, out| {
                lhs.meet(rhs, out);
            });
        }

        #[test]
        fn test_one_side_empty_at_many_levels() {
            let expected: Paths = &[
                (&[0x00], 0),
                (&[0x00, 0x01, 0x02, 0x03], 3),
                (&[0x01, 0x02, 0x03, 0x04, 0x05], 8),
            ];
            check(&ONE_SIDED_PATHS, expected, |lhs, rhs, out| {
                lhs.meet(rhs, out);
            });
        }

        #[test]
        fn test_almost_identical_paths() {
            check(
                &ALMOST_IDENTICAL_PATHS,
                ALMOST_IDENTICAL_PATHS.1,
                |lhs, rhs, out| lhs.meet(rhs, out),
            );
        }

        #[test]
        fn test_one_side_empty() {
            check(&LHS_EMPTY, [], |lhs, rhs, out| lhs.meet(rhs, out));
            check(&RHS_EMPTY, [], |lhs, rhs, out| lhs.meet(rhs, out));
        }

        #[test]
        fn test_exact_overlap_divergent_subtries() {
            let expected: Paths = &[(&[1, 2, 3], 0)];
            check(
                &PATHS_WITH_SAME_PREFIX_DIFFERENT_CHILDREN,
                expected,
                |lhs, rhs, out| lhs.meet(rhs, out),
            );
        }

        #[test]
        fn test_zigzag() {
            let expected: Paths = &[(&[2, 1], 2), (&[3], 3)];
            check(&ZIGZAG_PATHS, expected, |lhs, rhs, out| {
                lhs.meet(rhs, out);
            });
        }

        #[test]
        fn test_root_values() {
            check(
                &PATHS_WITH_ROOT_VALS_AND_CHILDREN,
                PATHS_WITH_ROOT_VALS_AND_CHILDREN.0,
                |lhs, rhs, out| lhs.meet(rhs, out),
            );
        }
    }

    mod subtract {
        use super::*;
        use super::experimental::zipper_algebra::{ZipperAlgebraExt, zipper_subtract};

        #[test]
        fn test_disjoint() {
            check(&DISJOINT_PATHS, DISJOINT_PATHS.0, |lhs, rhs, out| {
                lhs.subtract(rhs, out);
            });
        }

        #[test]
        fn test_deep_shared_prefix_then_split() {
            check(
                &PATHS_WITH_SHARED_PREFIX,
                PATHS_WITH_SHARED_PREFIX.0,
                |lhs, rhs, out| lhs.subtract(rhs, out),
            );
        }

        #[test]
        fn test_interleaving_paths() {
            check(
                &INTERLEAVING_PATHS,
                INTERLEAVING_PATHS.0,
                |lhs, rhs, out| lhs.subtract(rhs, out),
            );
        }

        #[test]
        fn test_one_side_empty_at_many_levels() {
            let expected: Paths = &[
                (&[0x00, 0x01], 1),
                (&[0x00, 0x01, 0x02], 2),
                (&[0x00, 0x01, 0x02, 0x03], 3),
                (&[0x01], 4),
                (&[0x01, 0x02], 5),
                (&[0x01, 0x02, 0x03], 6),
                (&[0x01, 0x02, 0x03, 0x04], 7),
                (&[0x01, 0x02, 0x03, 0x04, 0x05], 8),
                (&[0x01, 0x02, 0x03, 0x04, 0x05, 0x06], 9),
            ];
            check(&ONE_SIDED_PATHS, expected, |lhs, rhs, out| {
                lhs.subtract(rhs, out)
            });
        }

        #[test]
        fn test_almost_identical_paths() {
            let expected: Paths = &[(b"hijklmnop", 1), (b"2", 5), (b"3", 6)];
            check(&ALMOST_IDENTICAL_PATHS, expected, |lhs, rhs, out| {
                lhs.subtract(rhs, out)
            });
        }

        #[test]
        fn test_one_side_empty() {
            check(&LHS_EMPTY, [], |lhs, rhs, out| lhs.subtract(rhs, out));
            check(&RHS_EMPTY, RHS_EMPTY.0, |lhs, rhs, out| {
                lhs.subtract(rhs, out)
            });
        }

        #[test]
        fn test_exact_overlap_divergent_subtries() {
            check(
                &PATHS_WITH_SAME_PREFIX_DIFFERENT_CHILDREN,
                PATHS_WITH_SAME_PREFIX_DIFFERENT_CHILDREN.0,
                |lhs, rhs, out| lhs.subtract(rhs, out),
            );
        }

        #[test]
        fn test_zigzag() {
            let expected: Paths = &[
                (&[1, 1], 0),
                (&[2], 1),
                (&[3, 2, 1], 4),
                (&[4], 4),
                (&[4, 3, 2, 1], 5),
            ];
            check(&ZIGZAG_PATHS, expected, |lhs, rhs, out| {
                lhs.subtract(rhs, out)
            });
        }

        #[test]
        fn test_root_values() {
            check(
                &PATHS_WITH_ROOT_VALS_AND_CHILDREN,
                PATHS_WITH_ROOT_VALS_AND_CHILDREN.0,
                |lhs, rhs, out| lhs.subtract(rhs, out),
            );
        }
    }
}
