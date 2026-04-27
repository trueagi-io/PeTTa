//! Fuzzing and randomized testing utilities for `PathMap`.
//!

use super::super::PathMap;
use super::trie_map::TrieValue;
use super::utils::{BitMask, ByteMask};
use super::zipper::{
    ReadZipperUntracked, Zipper, ZipperMoving, ZipperReadOnlyIteration, ZipperReadOnlyValues,
};
use rand::Rng;
use rand::distr::Uniform;
use rand_distr::Distribution;
use std::marker::PhantomData;

// Re-export generic combinators
pub use distr_combinators::*;

// ===================================================================================================
// Generating Random PathMaps
// ===================================================================================================

/// A [`Distribution`] that generates a `PathMap` by performing independent samples of paths and values.
///
/// This struct creates a `PathMap` populated with a fixed number of paths ending in values, sampled
/// independently from the provided path (`pd`) and value (`vd`) distributions.
#[derive(Clone)]
pub struct RandomTrie<
    T: TrieValue,
    PathD: Distribution<Vec<u8>> + Clone,
    ValueD: Distribution<T> + Clone,
> {
    /// The number of independent samples to attempt to insert into the generated `PathMap`.
    pub size: usize,
    /// Distribution for generating paths (keys).
    pub pd: PathD,
    /// Distribution for generating values.
    pub vd: ValueD,
    /// PhantomData to hold the type `T`.
    pub ph: PhantomData<T>,
}
impl<T: TrieValue, PathD: Distribution<Vec<u8>> + Clone, ValueD: Distribution<T> + Clone>
    Distribution<PathMap<T>> for RandomTrie<T, PathD, ValueD>
{
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> PathMap<T> {
        let mut btm = PathMap::new();
        for _ in 0..self.size {
            btm.set_val_at(&self.pd.sample(rng)[..], self.vd.sample(rng));
        }
        btm
    }
}

/*
// fancier Trie Distributions WIP
pub struct GrowTrie<T, SproutD : Fn(&PathMap<T>) -> Distribution<PathMap<T>>> { seed: PathMap<T>, sd: SproutD }
impl <T, SproutD : Fn(T) -> Distribution<&PathMap<T>>> Distribution<PathMap<T>> for GrowTrie<T, SproutD> {
  fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> PathMap<T> {
    let mut btm = PathMap::new();
    for i in 0..self.size {
      btm.set_val_at(&self.pd.sample(rng)[..], self.vd.sample(rng));
    }
    btm
  }
}*/

// ===================================================================================================
// Sampling from PathMaps
// ===================================================================================================

/// A [`Distribution`] that samples a value from a [`PathMap`], uniformly among all values.
///
/// Sampling returns a tuple containing the full path and the value.
/// This distribution ensures that every value in the map has an equal probability of being selected.
///
/// Sampling is $O(N)$ where $N$ is the number of values in the map, as it must traverse the trie to
/// find the $k$-th element.
#[derive(Clone)]
pub struct FairTrieValue<T: TrieValue> {
    /// The source `PathMap` to sample from.
    pub source: PathMap<T>,
}
impl<T: TrieValue> Distribution<(Vec<u8>, T)> for FairTrieValue<T> {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> (Vec<u8>, T) {
        // it's much cheaper to draw many samples at once, but the current Distribution API is broken
        let mut rz = self.source.read_zipper();
        let size = rz.val_count();
        let target = rng.random_range(0..size);
        let mut i = 0;
        while let Some(t) = rz.to_next_get_val() {
            if i == target {
                return (rz.path().to_vec(), t.clone());
            }
            i += 1;
        }
        unreachable!();
    }
}

/// A [`Distribution`] that samples a value from a [`PathMap`] by descending the trie from the root
/// in an order dictated by `policy`, stopping at the first value encountered.
///
/// Sampling returns a tuple containing the full path and the value.  At each split, the `policy`
/// is consulted to choose the distribution over the next downstream bytes.
#[derive(Clone)]
pub struct DescendFirstTrieValue<
    T: TrieValue,
    ByteD: Distribution<u8> + Clone,
    P: Fn(&ReadZipperUntracked<T>) -> ByteD,
> {
    /// The source `PathMap` to sample from.
    pub source: PathMap<T>,
    /// The policy function used to determine the next descent step.
    pub policy: P,
}
impl<T: TrieValue, ByteD: Distribution<u8> + Clone, P: Fn(&ReadZipperUntracked<T>) -> ByteD>
    Distribution<(Vec<u8>, T)> for DescendFirstTrieValue<T, ByteD, P>
{
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> (Vec<u8>, T) {
        let mut rz = self.source.read_zipper();
        while !rz.is_val() {
            let b = (self.policy)(&rz).sample(rng);
            rz.descend_to_byte(b);
        }
        (rz.path().to_vec(), rz.get_val().unwrap().clone())
    }
}
pub fn unbiased_descend_first_policy<T: TrieValue>(
    rz: &ReadZipperUntracked<T>,
) -> Categorical<u8, Uniform<usize>> {
    let bm = rz.child_mask();
    Categorical {
        elements: bm.iter().collect(),
        ed: Uniform::try_from(0..bm.count_bits()).unwrap(),
    }
}

/// A [`Distribution`] that samples a path uniformly from all path bytes in the trie.
///
/// Sampling returns a tuple containing the full path and an optional value if one exists.
/// Every path byte in the trie is sampled with equal probability, including those with
/// downward continuations and those that are terminal (leaves). The empty path (root)
/// is also a candidate.
///
/// Use this distribution when you need to pick a random location *within* the trie structure,
/// regardless of whether that location holds a value or is a leaf.
///
/// # Probability Distribution
///
/// The sampling is uniform over all $N$ path bytes.
/// - A path byte with 100 downstream continuation paths has the same probability ($\frac{1}{N}$) of being *returned*
///   as a terminal path byte.
/// - However, the probability of *descending into* a branch is proportional to the size of that branch's subtree.
///
/// # Performance
///
/// Sampling is $O(N)$ where $N$ is the total number of path bytes in the trie.
#[derive(Clone)]
pub struct FairTriePath<T: TrieValue> {
    /// The source `PathMap` to sample from.
    pub source: PathMap<T>,
}
impl<T: TrieValue + 'static> Distribution<(Vec<u8>, Option<T>)> for FairTriePath<T> {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> (Vec<u8>, Option<T>) {
        use super::morphisms::Catamorphism;
        // it's much cheaper to draw many samples at once, but the current Distribution API is broken
        let size = Catamorphism::into_cata_cached(
            self.source.clone(),
            |_: &ByteMask, ws: &mut [usize], _mv: Option<&T>| ws.iter().sum::<usize>() + 1,
        );
        let target = rng.random_range(0..size);
        let mut i = 0;
        Catamorphism::into_cata_side_effect_fallible(
            self.source.clone(),
            |_: &ByteMask, _, mv: Option<&T>, path: &[u8]| {
                if i == target {
                    Err((path.to_vec(), mv.cloned()))
                } else {
                    i += 1;
                    Ok(())
                }
            },
        )
        .unwrap_err()
    }
}

/// A [`Distribution`] that samples a value from a [`PathMap`] by descending the trie from the root.
///
/// Unlike [`DescendFirstTrieValue`], this distribution allows for more complex termination logic.
/// At each step, the `policy` can decide to:
/// 1. Return a success value `Ok(byte)`, causing the traversal to descend to that byte.
/// 2. Return a failure/terminal value `Err(S)`, causing the traversal to stop and return the current path and the value `S`.
///
/// This allows for constructing random walks that depend on the local structure of the trie.
#[derive(Clone)]
pub struct DescendTriePath<
    T: TrieValue,
    S,
    SByteD: Distribution<Result<u8, S>> + Clone,
    P: Fn(&ReadZipperUntracked<T>) -> SByteD,
> {
    /// The source `PathMap` to sample from.
    pub source: PathMap<T>,
    /// The policy function that determines the next step (descend or terminate).
    pub policy: P,
    /// PhantomData for the terminal value type `S`.
    pub ph: PhantomData<S>,
}
impl<
    T: TrieValue,
    S,
    SByteD: Distribution<Result<u8, S>> + Clone,
    P: Fn(&ReadZipperUntracked<T>) -> SByteD,
> Distribution<(Vec<u8>, S)> for DescendTriePath<T, S, SByteD, P>
{
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> (Vec<u8>, S) {
        let mut rz = self.source.read_zipper();
        loop {
            match (self.policy)(&rz).sample(rng) {
                Ok(b) => {
                    rz.descend_to_byte(b);
                }
                Err(s) => return (rz.path().to_vec(), s),
            }
        }
    }
}
/// A policy for `DescendTriePath` that uniformly descends the trie until a leaf path byte (a path byte with
/// no children) is reached
///
/// At each step, it checks if there are any child path bytes. If there are, it randomly selects one
/// with uniform probability and continues the descent. If there are no children, it terminates
/// and returns the value associated with that terminal path byte.
///
/// Panics if it reaches a terminal path byte that does not have an associated value.
pub fn unbiased_descend_last_policy<T: TrieValue>(
    rz: &ReadZipperUntracked<T>,
) -> Choice2<
    u8,
    Categorical<u8, Uniform<usize>>,
    T,
    Mapped<Option<T>, T, Degenerate<Option<T>>, fn(Option<T>) -> T>,
    Degenerate<bool>,
> {
    let bm = rz.child_mask();
    let options: Vec<u8> = bm.iter().collect();
    let noptions = options.len();

    Choice2 {
        db: Degenerate { element: noptions > 0 },
        // safety: Uniform stores integers, and while you can't sample from lower=upper=0, the memory is valid
        dx: Categorical {
            elements: options,
            ed: Uniform::try_from(0..noptions).unwrap_or(Uniform::try_from(0..=0).unwrap()),
        },
        dy: Mapped {
            d: Degenerate { element: rz.get_val().cloned() },
            f: |v| v.unwrap(),
            pd: PhantomData::default(),
        },
        pd: PhantomData::default(),
    }
}

#[cfg(all(test, feature = "pathmap-internal-tests"))]
mod tests {
    use super::random::*;
    use super::ring::Lattice;
    use super::zipper::{ZipperSubtries, ZipperWriting};
    use rand::SeedableRng;
    use rand::rngs::StdRng;
    use rand_distr::{Triangular, Uniform};
    use std::hint::black_box;

    #[test]
    fn fixed_length() {
        let mut rng = StdRng::from_seed([0; 32]);
        let path_gen = Repeated {
            lengthd: Degenerate { element: 3 },
            itemd: Categorical {
                elements: "abcd".as_bytes().to_vec(),
                ed: Uniform::try_from(0..4).unwrap(),
            },
            pd: PhantomData::default(),
        };
        let trie_gen = RandomTrie {
            size: 10,
            pd: path_gen,
            vd: Degenerate { element: () },
            ph: PhantomData::default(),
        };
        let trie = trie_gen.sample(&mut rng);
        let res = ["aaa", "aac", "bba", "bdd", "cbb", "cbd", "dab", "dac", "dca"];
        trie.iter().zip(res).for_each(|(x, y)| assert_eq!(x.0.as_slice(), y.as_bytes()));
    }

    #[test]
    fn variable_length() {
        let mut rng = StdRng::from_seed([0; 32]);
        let path_gen = Filtered {
            d: Sentinel {
                mbd: Mapped {
                    d: Categorical {
                        elements: "abcd\0".as_bytes().to_vec(),
                        ed: Uniform::try_from(0..5).unwrap(),
                    },
                    f: |x| if x == b'\0' { None } else { Some(x) },
                    pd: PhantomData::default(),
                },
            },
            p: |x| !x.is_empty(),
            pd: PhantomData::default(),
        };
        let trie_gen = RandomTrie {
            size: 10,
            pd: path_gen,
            vd: Degenerate { element: () },
            ph: PhantomData::default(),
        };
        let trie = trie_gen.sample(&mut rng);
        // println!("{:?}", trie.iter().map(|(p, _)| String::from_utf8(p).unwrap()).collect::<Vec<_>>());
        let res = [
            "aa",
            "acbdddacbcbdbad",
            "bbddad",
            "bd",
            "caccb",
            "cba",
            "cbcdbccb",
            "dadbcdbcaaadb",
            "dbabbdaabc",
            "dbb",
        ];
        trie.iter().zip(res).for_each(|(x, y)| assert_eq!(x.0.as_slice(), y.as_bytes()));
    }

    #[test]
    fn fair_trie_value() {
        #[cfg(all(feature = "pathmap-internal-tests", not(miri)))]
        const SAMPLES: usize = 100000;
        #[cfg(miri)]
        const SAMPLES: usize = 100;
        let rng = StdRng::from_seed([0; 32]);

        let pairs = &[("abc", 0), ("abd", 1), ("ax", 2), ("ay", 3), ("A1", 4), ("A2", 5)];
        let btm = PathMap::from_iter(pairs.iter().map(|(s, i)| (s.as_bytes(), i)));
        let stv = FairTrieValue { source: btm };
        let hist =
            Histogram::from_iter(stv.sample_iter(rng).map(|(_, v)| v).take(pairs.len() * SAMPLES));
        let achieved: Vec<usize> = hist
            .iter()
            .map(|(_k, c)| ((c as f64) / ((SAMPLES / 100) as f64)).round() as usize)
            .collect();

        achieved.into_iter().for_each(|c| {
            let err_bar = ((5 - SAMPLES.ilog10()).pow(2) * 2) as usize;
            assert!(c >= 100 - err_bar);
            assert!(c <= 100 + err_bar);
        });
    }

    #[test]
    fn random_descend_first_trie_value() {
        #[cfg(all(feature = "pathmap-internal-tests", not(miri)))]
        const SAMPLES: usize = 100000;
        #[cfg(miri)]
        const SAMPLES: usize = 800;
        let rng = StdRng::from_seed([0; 32]);

        let pairs =
            &[("abc", 0), ("abcd", 10), ("abd", 1), ("ax", 2), ("ay", 3), ("A1", 4), ("A2", 5)];
        // TRIE STRUCTURE & PROBABILITIES
        // ============================
        // The walker chooses a child uniformly at random at each step.
        //
        // Root (1.0)
        // ├── 'A' (0.5)
        // │   ├── '1' (0.25) -> Value "A1" [HIGH FREQUENCY]
        // │   └── '2' (0.25) -> Value "A2" [HIGH FREQUENCY]
        // └── 'a' (0.5)
        //     ├── 'x' (0.166...) -> Value "ax" [MEDIUM FREQUENCY]
        //     ├── 'y' (0.166...) -> Value "ay" [MEDIUM FREQUENCY]
        //     └── 'b' (0.166...)
        //         ├── 'c' (0.083...) -> Value "abc" [LOW FREQUENCY]
        //         │   └── 'd' ... (Ignored: descend_first stops at "abc")
        //         └── 'd' (0.083...) -> Value "abd" [LOW FREQUENCY]
        //
        // Note: "abcd" is never returned because the descent stops at the first value encountered ("abc").

        let btm = PathMap::from_iter(pairs.iter().map(|(s, i)| (s.as_bytes(), i)));
        let stv = DescendFirstTrieValue { source: btm, policy: unbiased_descend_first_policy };
        let hist = Histogram::from_iter(stv.sample_iter(rng).map(|(_, v)| *v).take(6 * SAMPLES));
        // println!("{:?}", hist.table());
        let achieved: Vec<(i32, i32)> = hist
            .iter()
            .map(|(k, c)| (*k, ((c as f64) / ((SAMPLES / 10) as f64)).round() as i32))
            .collect();

        // The ultimate order is down the particular random number sequence, but we know that
        // "abc" and "abd" should each get roughly 5, sample-normalized (Low)
        // "ax" and "ay" should each get roughly 10, (Medium)
        // "A1" and "A2" should each get roughly 15. (High)

        // Low (Rank 0, 1) -> abc, abd (Keys 0, 1) -> Count 5
        assert!(achieved[0].0 == 0 || achieved[0].0 == 1);
        assert!(achieved[1].0 == 0 || achieved[1].0 == 1);
        assert!(achieved[0].1 == 5 && achieved[1].1 == 5);

        // Med (Rank 2, 3) -> ax, ay (Keys 2, 3) -> Count 10
        assert!(achieved[2].0 == 2 || achieved[2].0 == 3);
        assert!(achieved[3].0 == 2 || achieved[3].0 == 3);
        assert!(achieved[2].1 == 10 && achieved[3].1 == 10);

        // High (Rank 4, 5) -> A1, A2 (Keys 4, 5) -> Count 15
        assert!(achieved[4].0 == 4 || achieved[4].0 == 5);
        assert!(achieved[5].0 == 4 || achieved[5].0 == 5);
        assert!(achieved[4].1 == 15 && achieved[5].1 == 15);
    }

    #[test]
    fn descend_last_trie_value() {
        #[cfg(all(feature = "pathmap-internal-tests", not(miri)))]
        const SAMPLES: usize = 100000;
        #[cfg(miri)]
        const SAMPLES: usize = 800;

        let rng = StdRng::from_seed([0; 32]);
        let btm = PathMap::from_iter(
            [("abc", 0), ("abcd", 10), ("abd", 1), ("ax", 2), ("ay", 3), ("A1", 4), ("A2", 5)]
                .iter()
                .map(|(s, i)| (s.as_bytes(), i)),
        );
        let stv = DescendTriePath {
            source: btm,
            policy: unbiased_descend_last_policy,
            ph: Default::default(),
        };
        let hist = Histogram::from_iter(stv.sample_iter(rng).map(|(_, v)| *v).take(6 * SAMPLES));
        // println!("{:?}", hist.table());
        let achieved: Vec<(i32, i32)> = hist
            .iter()
            .map(|(k, c)| (*k, ((c as f64) / ((SAMPLES / 10) as f64)).round() as i32))
            .collect();

        // The ultimate order is down the particular random number sequence, but we know that
        // "abc" is never returned (has child). "abcd" and "abd" are returned (Leaves).
        // "abcd" and "abd" should be Low (~5)
        // "ax" and "ay" should each get roughly 10 (Medium)
        // "A1" and "A2" should each get roughly 15 (High)

        // Check Lows (Rank 0, 1) -> abcd, abd (Keys 10, 1) -> Count 5
        assert!(achieved[0].1 < 8);
        assert!(achieved[1].1 < 8);
        assert!(achieved[0].0 == 1 || achieved[0].0 == 10);
        assert!(achieved[1].0 == 1 || achieved[1].0 == 10);

        // Check Meds (Rank 2, 3) -> ax, ay (Keys 2, 3) -> Count 10
        assert!(achieved[2].0 == 2 || achieved[2].0 == 3);
        assert!(achieved[3].0 == 2 || achieved[3].0 == 3);
        assert!(achieved[2].1 == 10 && achieved[3].1 == 10);

        // Check Highs (Rank 4, 5) -> A1, A2 (Keys 4, 5) -> Count 15
        assert!(achieved[4].0 == 4 || achieved[4].0 == 5);
        assert!(achieved[5].0 == 4 || achieved[5].0 == 5);
        assert!(achieved[4].1 == 15 && achieved[5].1 == 15);
    }

    #[test]
    fn fair_trie_path() {
        #[cfg(all(feature = "pathmap-internal-tests", not(miri)))]
        const SAMPLES: usize = 100000;
        #[cfg(miri)]
        const SAMPLES: usize = 100;

        let rng = StdRng::from_seed([0; 32]);
        let btm = PathMap::from_iter(
            [("abc", 0), ("abd", 1), ("ax", 2), ("ay", 3), ("A1", 4), ("A2", 5)]
                .iter()
                .map(|(s, i)| (s.as_bytes(), i)),
        );
        let stv = FairTriePath { source: btm };
        let hist = Histogram::from_iter(stv.sample_iter(rng).map(|(p, _)| p).take(10 * SAMPLES));
        let achieved: Vec<usize> = hist
            .into_iter()
            .map(|(_k, c)| ((c as f64) / ((SAMPLES / 100) as f64)).round() as usize)
            .collect();

        achieved.into_iter().for_each(|c| {
            let err_bar = ((5 - SAMPLES.ilog10()).pow(2) * 2) as usize;
            assert!(c >= 100 - err_bar);
            assert!(c <= 100 + err_bar);
        });
    }

    #[test]
    fn resample_trie() {
        const SAMPLES: usize = 10;
        let mut rng = StdRng::from_seed([0; 32]);
        let mut btm = PathMap::new();
        let rs = [
            "Abbotsford",
            "Abbottabad",
            "Abcoude",
            "Abdul Hakim",
            "Abdulino",
            "Abdullahnagar",
            "Abdurahmoni Jomi",
            "Abejorral",
            "Abelardo Luz",
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
        let lengths = Triangular::new(1f32, 5., 1.5).unwrap();
        let submaps = Collected {
            d: Product2 {
                dx: FairTriePath { source: btm.clone() },
                dy: lengths,
                f: |(path, v), l| {
                    if v.is_none() && path.len() == l.round() as usize { Some(path) } else { None }
                },
                pd: PhantomData::default(),
            },
            pf: |mp| mp.map(|p| btm.read_zipper_at_path(p).try_make_map().unwrap()),
            pd: PhantomData::default(),
        };

        // println!("{:?}", submap.iter().map(|(p, v)| (String::from_utf8(p).unwrap(), v)).collect::<Vec<_>>())
        assert_eq!(
            submaps
                .clone()
                .sample_iter(rng.clone())
                .map(|x: PathMap<usize>| x
                    .iter()
                    .map(|(p, v)| (String::from_utf8(p).unwrap(), *v))
                    .collect::<Vec<_>>())
                .take(4)
                .collect::<Vec<_>>(),
            vec![
                vec![("otsford".to_string(), 0), ("ottabad".to_string(), 1)],
                vec![
                    ("bbotsford".to_string(), 0),
                    ("bbottabad".to_string(), 1),
                    ("bcoude".to_string(), 2),
                    ("bdul Hakim".to_string(), 3),
                    ("bdulino".to_string(), 4),
                    ("bdullahnagar".to_string(), 5),
                    ("bdurahmoni Jomi".to_string(), 6),
                    ("bejorral".to_string(), 7),
                    ("belardo Luz".to_string(), 8)
                ],
                vec![
                    ("'i".to_string(), 17),
                    ("an".to_string(), 9),
                    ("ane".to_string(), 10),
                    ("anus".to_string(), 11),
                    ("ulus".to_string(), 12)
                ],
                vec![("oude".to_string(), 2)]
            ]
        );

        let resampled = Concentrated {
            dx: Product2 {
                dx: FairTriePath { source: btm.clone() },
                dy: submaps,
                f: |(p, _), sm| {
                    let mut r = PathMap::new();
                    r.write_zipper_at_path(&p[..]).graft_map(sm);
                    r
                },
                pd: PhantomData::default(),
            },
            z: (PathMap::new(), 0),
            fa: |state: &mut (_, _), sm| {
                let (a, c) = state;
                a.join_into(sm);
                *c += 1;
                if *c == SAMPLES { Some(std::mem::take(a)) } else { None }
            },
            pd: PhantomData::default(),
        };

        let resampled10 = resampled.sample(&mut rng);
        assert_eq!(
            [
                "Abbotsahmoni Jomi",
                "Abbottabens",
                "Abbottaber",
                "Abbottabicon",
                "Abbottabicundus",
                "Abdul Hakimm'i",
                "Abdul Hakimman",
                "Abdul Hakimmane",
                "Abdul Hakimmanus",
                "Abdul Hakimmulus",
                "Abdurahens",
                "Abduraher",
                "Abdurahicon",
                "Abdurahicundus",
                "Abdurahmoni Jom'i",
                "Abdurahmoni Joman",
                "Abdurahmoni Jomane",
                "Abdurahmoni Jomanus",
                "Abdurahmoni Jomulus",
                "Abdurahmoni jorral",
                "Abdurahmoni lardo Luz",
                "Abdurahmoniens",
                "Abdurahmonier",
                "Abdurahmoniicon",
                "Abdurahmoniicundus",
                "Abdurahmonoude",
                "Abelus",
                "romuoude"
            ][..],
            resampled10.iter().map(|(p, _)| String::from_utf8(p).unwrap()).collect::<Vec<_>>()
        );
    }

    #[test]
    fn remove_bug_reproduction() {
        const N_TRIES: usize = 10;
        const N_PATHS: usize = 10;
        const N_REMOVES: usize = 10;
        let rng = StdRng::from_seed([0; 32]);
        let path_gen = Filtered {
            d: Sentinel {
                mbd: Mapped {
                    d: Categorical {
                        elements: "abcd\0".as_bytes().to_vec(),
                        ed: Uniform::try_from(0..5).unwrap(),
                    },
                    f: |x| if x == b'\0' { None } else { Some(x) },
                    pd: PhantomData::default(),
                },
            },
            p: |x| !x.is_empty(),
            pd: PhantomData::default(),
        };
        let trie_gen = RandomTrie {
            size: N_PATHS,
            pd: path_gen.clone(),
            vd: Degenerate { element: () },
            ph: PhantomData::default(),
        };

        trie_gen.sample_iter(rng.clone()).take(N_TRIES).for_each(|mut trie| {
            // println!("let mut btm = PathMap::from_iter({:?}.iter().map(|(p, v)| (p.as_bytes(), v)));", trie.iter().map(|(p, v)| (String::from_utf8(p).unwrap(), v)).collect::<Vec<_>>());
            path_gen.clone().sample_iter(rng.clone()).take(N_REMOVES).for_each(|path| {
                // println!("btm.remove_val_at({:?}.as_bytes());", String::from_utf8(path.clone()).unwrap());
                trie.remove_val_at(path, true);
            });
            black_box(trie);
        })
    }

    #[test]
    fn zipper_basic_0() {
        #[cfg(all(feature = "pathmap-internal-tests", not(miri)))]
        const N_TRIES: usize = 100;
        #[cfg(miri)]
        const N_TRIES: usize = 10;

        #[cfg(all(feature = "pathmap-internal-tests", not(miri)))]
        const N_PATHS: usize = 100;
        #[cfg(miri)]
        const N_PATHS: usize = 10;

        #[cfg(all(feature = "pathmap-internal-tests", not(miri)))]
        const N_DESCENDS: usize = 100;
        #[cfg(miri)]
        const N_DESCENDS: usize = 10;

        let rng = StdRng::from_seed([0; 32]);
        let rng_ = StdRng::from_seed([!0; 32]);
        let path_gen = Filtered {
            d: Sentinel {
                mbd: Mapped {
                    d: Categorical {
                        elements: "abcd\0".as_bytes().to_vec(),
                        ed: Uniform::try_from(0..5).unwrap(),
                    },
                    f: |x| if x == b'\0' { None } else { Some(x) },
                    pd: PhantomData::default(),
                },
            },
            p: |x| !x.is_empty(),
            pd: PhantomData::default(),
        };
        let trie_gen = RandomTrie {
            size: N_PATHS,
            pd: path_gen.clone(),
            vd: Degenerate { element: () },
            ph: PhantomData::default(),
        };

        // ACTION := DESCEND_TO p | ASCEND i
        // { RZ.descend_to(p); RZ.ascend(p.len()) } =:= {}
        // { RZ.descend_to(p1); RZ.descend_to(p2) } =:= { RZ.descend_to(p1.concat(p2)) }
        // { RZ.ascend(i); RZ.ascend(j) } =:= { RZ.ascend(i+j) }
        // { RZ = TRIE.read_zipper(); RZ.ascend(k) } =:= { RZ = TRIE.read_zipper(); }
        // { RZ = TRIE.read_zipper(); ACT(RZ); RZ.reset() } =:= { RZ = TRIE.read_zipper(); }
        // { RZ = TRIE.read_zipper(); ACT(RZ); RZ.reset() } =:= { RZ = TRIE.read_zipper(); }
        // { RZ.reset(); RZ.descend_to(p) } =:= { RZ.move_to(p); }

        trie_gen.sample_iter(rng.clone()).take(N_TRIES).for_each(|trie| {
            // println!("let mut btm = PathMap::from_iter({:?}.iter().map(|(p, v)| (p.as_bytes(), v)));", trie.iter().map(|(p, v)| (String::from_utf8(p).unwrap(), v)).collect::<Vec<_>>());
            let mut rz = trie.read_zipper();
            path_gen.clone().sample_iter(rng.clone()).take(N_DESCENDS).for_each(|path| {
                rz.descend_to(&path[..]);
                assert_eq!(rz.get_val(), trie.get_val_at(&path[..]));
                path_gen.clone().sample_iter(rng_.clone()).take(N_DESCENDS).for_each(|path| {
                    rz.descend_to(&path[..]);
                    rz.ascend(path.len());
                });
                assert_eq!(rz.path(), &path[..]);
                assert_eq!(rz.get_val(), trie.get_val_at(&path[..]));
                path_gen.clone().sample_iter(rng_.clone()).take(N_DESCENDS).for_each(|path| {
                    // println!("prev {:?}", rz.path());
                    rz.move_to_path(&path[..]);
                    assert_eq!(rz.path(), &path[..]);
                    assert_eq!(rz.get_val(), trie.get_val_at(&path[..]));
                });
                rz.reset();
            });
            drop(rz);
            black_box(trie);
        })
    }

    #[test]
    fn zipper_basic_1() {
        #[cfg(all(feature = "pathmap-internal-tests", not(miri)))]
        const N_TRIES: usize = 100;
        #[cfg(miri)]
        const N_TRIES: usize = 10;

        #[cfg(all(feature = "pathmap-internal-tests", not(miri)))]
        const N_PATHS: usize = 100;
        #[cfg(miri)]
        const N_PATHS: usize = 10;

        #[cfg(all(feature = "pathmap-internal-tests", not(miri)))]
        const N_DESCENDS: usize = 100;
        #[cfg(miri)]
        const N_DESCENDS: usize = 10;

        let rng = StdRng::from_seed([0; 32]);
        // let rng_ = StdRng::from_seed([!0; 32]);
        let path_gen = Filtered {
            d: Sentinel {
                mbd: Mapped {
                    d: Categorical {
                        elements: "abcd\0".as_bytes().to_vec(),
                        ed: Uniform::try_from(0..5).unwrap(),
                    },
                    f: |x| if x == b'\0' { None } else { Some(x) },
                    pd: PhantomData::default(),
                },
            },
            p: |x| !x.is_empty(),
            pd: PhantomData::default(),
        };
        let trie_gen = RandomTrie {
            size: N_PATHS,
            pd: path_gen.clone(),
            vd: Degenerate { element: () },
            ph: PhantomData::default(),
        };

        trie_gen.sample_iter(rng.clone()).take(N_TRIES).for_each(|mut trie| {
            path_gen.clone().sample_iter(rng.clone()).take(N_DESCENDS).for_each(|path| {
                let mut wz = trie.write_zipper_at_path(&path[..]);
                black_box(wz.get_val_or_set_mut(()));
                drop(wz);
            });
            black_box(trie);
        })
    }
}
