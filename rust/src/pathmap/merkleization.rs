
//GOAT: Internal Discussion: What do we ultimately want this API to look like?
//
// * Firstly, we probably ought to make a writable trait, in the vein of `ZipperSubtries` that allows
// merkleization on any object that has a mutable trie node (PathMaps, WriteZippers, etc.)
// I've wanted that anyway for a while because I felt like a lot of the API between ZipperWriting and
// PathMap should be merged together.
//
// * Secondly, I think we ought to allow the client to exfiltrate and store the `memo`, wrapped in some
// kind of black box.  That way, they could perform "gradual merkleization", as an opportunistic background
// process.
//
// However, the `memo` is liable to get huge in real-world situations, therefore we also want some kind of
// way to implement a heuristic to evict nodes that are unlikely to be duplicated.  I'm not totally sure what
// that heuristic(s) would be.  My first guess would be to try an LRU cache, but also, higher nodes are less
// likely to be identical, but also we get more "bang" if we find higher-level sharing.  So perhaps finding
// a that's shared means we make sure its parents are refreshed into the cache...  Anyway, gotta try stuff
// and measure.
//
// One additional consideration if we go the persistent memo route is that the very existence of the memo
// structure, under the present implementation, would increase the node refcounts and lead to copying... So
// we might want to explore something like a "weak" pointer to keep in the memo (semantics wouldn't be exactly
// the same as an Rc `weak`, but a similar idea)

use super::alloc::Allocator;
use super::trie_core::node::{TrieNodeODRc, NODE_ITER_FINISHED};
use super::gxhash;

/// Statistics created after merkleization
#[derive(Default, Debug)]
pub struct MerkleizeResult {
    /// The hash of the entire trie beneath the root
    pub hash: u128,
    /// The number of shared node references that replaced identical copies during the merkleization
    pub reused: usize,
    //GOAT, not sure how to describe this
    pub cloned: usize,
    //GOAT, not sure how to describe this
    pub replaced: usize,
}

pub(crate) fn merkleize_impl<V, A>(
    counters: &mut MerkleizeResult,
    memo: &mut gxhash::HashMap<u128, TrieNodeODRc<V, A>>,
    node: &TrieNodeODRc<V, A>,
    value: Option<&V>,
) -> (u128, Option<TrieNodeODRc<V, A>>)
    where
        V: Clone + Send + Sync + std::hash::Hash,
        A: Allocator,
{
    // hash = (value, [(path, child_hash)])
    use std::hash::{Hash};
    use std::collections::hash_map::Entry;
    const INITIAL_SEED: i64 = 0;
    let mut hasher = gxhash::GxHasher::with_seed(INITIAL_SEED);
    value.hash(&mut hasher);
    let mut replacement = None;

    let node_ref = node.as_tagged();
    let mut it = node_ref.new_iter_token();
    while it != NODE_ITER_FINISHED {
        let (next, path, child, val) = node_ref.next_items(it);
        it = next;
        path.hash(&mut hasher);
        let (child_hash, replace);
        if let Some(child) = child {
            (child_hash, replace) = merkleize_impl(counters, memo, child, val);
            if let Some(replace) = replace {
                let node = replacement.get_or_insert_with(|| {
                    counters.cloned += 1;
                    node.clone()
                });
                counters.replaced += 1;
                node.make_mut().node_replace_child(path, replace);
            }
        } else {
            // value and no child -> pretend there's an empty node
            let mut hasher = gxhash::GxHasher::with_seed(INITIAL_SEED);
            val.hash(&mut hasher);
            child_hash = hasher.finish_u128();
        }
        child_hash.hash(&mut hasher);
    }
    let hash = hasher.finish_u128();
    match memo.entry(hash) {
        Entry::Vacant(entry) => {
            counters.cloned += 1;
            if let Some(replacement) = &replacement {
                entry.insert(replacement.clone());
            } else {
                entry.insert(node.clone());
            }
        }
        // if we've seen the hash before, do the replacement
        Entry::Occupied(entry) => {
            counters.reused += 1;
            replacement = Some(entry.get().clone());
        }
    }
    (hash, replacement)
}

#[cfg(all(test, feature = "pathmap-internal-tests"))]
mod tests {

    #[test]
    fn test_btm_merkleize() {
        let paths: &[&[u8]] = &[
            b"axx",
            b"ayy",
            b"bxx",
            b"byy",
            b"cxx",
            b"cyy",
            b"ddxx",
            b"ddyy",
        ];
        let paths = paths.iter()
            .map(|&path| (path, ()));
        let mut btm = super::PathMap::from_iter(paths);
        #[cfg(feature="viz")] {
            let mut before = Vec::new();
            use super::viz::{viz_maps, DrawConfig};
            viz_maps(&[btm.clone()], &DrawConfig::default(), &mut before).unwrap();
            eprintln!("before:");
            eprintln!("```ignoremermaid\n{}```ignore", std::str::from_utf8(&before).unwrap());
        }
        let result = btm.merkleize();
        eprintln!("merkleize result: {result:?}\n");
        #[cfg(feature="viz")] {
            use super::viz::{viz_maps, DrawConfig};
            let mut after = Vec::new();
            viz_maps(&[btm], &DrawConfig::default(), &mut after).unwrap();
            eprintln!("after:");
            eprintln!("```ignoremermaid\n{}```ignore", std::str::from_utf8(&after).unwrap());
        }
    }
}