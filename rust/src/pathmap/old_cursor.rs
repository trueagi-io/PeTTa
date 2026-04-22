//!
//! Temporary file, porting old cursor to new trie.  ONLY WORKS WITH `all_dense_nodes` feature
//!

use super::super::PathMap;
use super::trie_core::node::{TaggedNodeRef, NODE_ITER_FINISHED};
use super::trie_core::dense_byte::{DenseByteNode, OrdinaryCoFree, CoFree};
use super::alloc::GlobalAlloc;

/// An iterator-like object that traverses key-value pairs in a [PathMap], however only one
/// returned reference may exist at a given time
pub struct AllDenseCursor<'a, V: Clone + Send + Sync> where V : Clone {
    prefix: Vec<u8>,
    btnis: Vec<ByteTrieNodeIter<'a, V>>,
    nopush: bool
}

impl <'a, V : Clone + Send + Sync + Unpin> AllDenseCursor<'a, V> {
    pub fn new(btm: &'a PathMap<V>) -> Self {
        btm.ensure_root();
        Self {
            prefix: vec![],
            btnis: vec![ByteTrieNodeIter::new(btm.root().unwrap().as_tagged().as_dense().unwrap())],
            nopush: false
        }
    }
}

impl <'a, V : Clone + Send + Sync> AllDenseCursor<'a, V> {
    pub fn next(&mut self) -> Option<(&[u8], &'a V)> {
        loop {
            match self.btnis.last_mut() {
                None => { return None }
                Some(last) => {
                    match last.next() {
                        None => {
                            // decrease view len with one
                            self.prefix.pop();
                            self.btnis.pop();
                        }
                        Some((b, cf)) => {
                            if self.nopush {
                                *self.prefix.last_mut().unwrap() = b;
                            } else {
                                self.prefix.push(b);
                            }

                            match cf.rec() {
                                None => {
                                    self.nopush = true;
                                }
                                Some(rec) => {
                                    self.nopush = false;
                                    self.btnis.push(ByteTrieNodeIter::new(rec.as_tagged().as_dense().unwrap()));
                                }
                            }

                            match cf.val() {
                                None => {}
                                Some(v) => {
                                    return Some((&self.prefix, v))
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

pub struct ByteTrieNodeIter<'a, V: Clone + Send + Sync> {
    i: u8,
    w: u64,
    btn: &'a DenseByteNode<V, GlobalAlloc>
}

impl <'a, V: Clone + Send + Sync> ByteTrieNodeIter<'a, V> {
    fn new(btn: &'a DenseByteNode<V, GlobalAlloc>) -> Self {
        Self {
            i: 0,
            w: btn.mask.0[0],
            btn: btn
        }
    }
}

impl <'a, V : Clone + Send + Sync> Iterator for ByteTrieNodeIter<'a, V> {
    type Item = (u8, &'a OrdinaryCoFree<V, GlobalAlloc>);

    fn next(&mut self) -> Option<(u8, &'a OrdinaryCoFree<V, GlobalAlloc>)> {
        loop {
            if self.w != 0 {
                let wi = self.w.trailing_zeros() as u8;
                self.w ^= 1u64 << wi;
                let index = self.i*64 + wi;
                return Some((index, unsafe{ self.btn.get_unchecked(index) } ))
            } else if self.i < 3 {
                self.i += 1;
                self.w = unsafe { *self.btn.mask.0.get_unchecked(self.i as usize) };
            } else {
                return None
            }
        }
    }
}

// //-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
// // Abstracted OldCursor  (Part way to abstraction without losing much perf)
// //-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

// pub struct AbstractedOldCursor<'a, V> where V : Clone {
//     prefix: Vec<u8>,
//     // btnis: Vec<AbstractedByteTrieNodeIter<'a, V>>,
//     // btnis: Vec<(usize, AbstractedNodeIter<'a, V>)>, //2. Fast enough
//     btnis: Vec<(AbstractNodeRef<'a, V>, u128)>, //3. Still Fast enough
//     nopush: bool
// }

// impl <'a, V : Clone> AbstractedOldCursor<'a, V> {
//     pub fn new(btm: &'a PathMap<V>) -> Self {
//         // Part of 3.
//         let node = AbstractNodeRef::DenseByteNode(btm.root().borrow().as_dense().unwrap());
//         let token = node.new_iter_token();

//         Self {
//             prefix: vec![],
//             // btnis: vec![AbstractedByteTrieNodeIter::new(btm.root().borrow())],
//             // btnis: vec![AbstractedByteTrieNodeIter::new(btm.root().borrow().as_dense().unwrap())],
//             // btnis: vec![(42, AbstractedNodeIter{ byte: ManuallyDrop::new(AbstractedByteTrieNodeIter::new(btm.root().borrow().as_dense().unwrap())) })], //Part of 2
//             btnis: vec![(node, token)], //Part of 3
//             nopush: false
//         }
//     }
// }

// impl <'a, V : Clone> AbstractedOldCursor<'a, V> {
//     pub fn next(&mut self) -> Option<(&[u8], &'a V)> {
//         loop {
//             match self.btnis.last_mut() {
//                 None => { return None }
//                 Some((node, token)) => {
//                     let (new_token, k, cf) = node.next_val(*token);

//                     if new_token == 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF {
//                         self.prefix.pop();
//                         self.btnis.pop();
//                     } else {
//                         *token = new_token;

//                         if self.nopush {
//                             *self.prefix.last_mut().unwrap() = k;
//                         } else {
//                             self.prefix.push(k);
//                         }

//                         match &cf.rec {
//                             None => {
//                                 self.nopush = true;
//                             }
//                             Some(rec) => {
//                                 self.nopush = false;
//                                 // self.btnis.push(AbstractedByteTrieNodeIter::new(rec.borrow()));
//                                 // self.btnis.push(AbstractedByteTrieNodeIter::new(rec.borrow().as_dense().unwrap()));

//                                 //Part of 2
//                                 // self.btnis.push((42, AbstractedNodeIter{ byte: ManuallyDrop::new(AbstractedByteTrieNodeIter::new(rec.borrow().as_dense().unwrap())) }));

//                                 //Part of 3
//                                 let child_node = AbstractNodeRef::DenseByteNode(rec.borrow().as_dense().unwrap());
//                                 let child_token = child_node.new_iter_token();
//                                 self.btnis.push((child_node, child_token));
//                             }
//                         }

//                         match &cf.value {
//                             None => {}
//                             Some(v) => {
//                                 return Some((&self.prefix, v))
//                             }
//                         }
//                     }
//                 }
//             }
//         }
//     }
// }

// //Part of 3
// pub enum AbstractNodeRef<'a, V> {
//     DenseByteNode(&'a DenseByteNode<V>),
//     Other
// }

// impl<'a, V: Clone> AbstractNodeRef<'a, V> {
//     fn next_val(&self, token: u128) -> (u128, u8, &'a super::dense_byte_node::CoFree<V>) {
//         match self {
//             Self::DenseByteNode(node) => node.next_cf(token),
//             _ => panic!()
//         }
//     }
//     fn new_iter_token(&self) -> u128 {
//         match self {
//             Self::DenseByteNode(node) => node.new_iter_token(),
//             _ => panic!()
//         }
//     }
// }

// //Part of 1 & 2
// // pub union AbstractedNodeIter<'a, V> {
// //     byte: ManuallyDrop<AbstractedByteTrieNodeIter<'a, V>>,
// //     something_else: ()
// // }

// // pub struct AbstractedByteTrieNodeIter<'a, V> {
// //     token: u128,
// //     // node: &'a dyn TrieNode<V>
// //     node: &'a DenseByteNode<V>
// // }

// // impl <'a, V: Clone> AbstractedByteTrieNodeIter<'a, V> {
// //     // fn new(node: &'a dyn TrieNode<V>) -> Self {
// //     fn new(node: &'a DenseByteNode<V>) -> Self {
// //         Self {
// //             token: node.new_iter_token(),
// //             node
// //         }
// //     }
// // }

// // impl <'a, V : Clone> Iterator for AbstractedByteTrieNodeIter<'a, V> {
// //     type Item = (u8, &'a CoFree<V>);

// //     fn next(&mut self) -> Option<(u8, &'a CoFree<V>)> {
// //         let (token, key, cf) = self.node.next_cf(self.token);
// //         if token == 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF {
// //             None
// //         } else {
// //             self.token = token;
// //             Some((key, cf))
// //         }
// //     }
// // }

//-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
// Abstracted OldCursor
//-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

pub struct PathMapCursor<'a, V: Clone + Send + Sync> {
    prefix_buf: Vec<u8>,
    btnis: Vec<(TaggedNodeRef<'a, V, GlobalAlloc>, u128, usize)>,
}

impl <'a, V : Clone + Send + Sync + Unpin> PathMapCursor<'a, V> {
    pub fn new(btm: &'a PathMap<V>) -> Self {
        const EXPECTED_DEPTH: usize = 16;
        const EXPECTED_PATH_LEN: usize = 256;
        btm.ensure_root();
        let node = btm.root().unwrap().as_tagged();
        let token = node.new_iter_token();
        let mut btnis = Vec::with_capacity(EXPECTED_DEPTH);
        btnis.push((node, token, 0));
        Self {
            prefix_buf: Vec::with_capacity(EXPECTED_PATH_LEN),
            btnis,
        }
    }
}

impl <'a, V : Clone + Send + Sync> PathMapCursor<'a, V> {
    pub fn next(&mut self) -> Option<(&[u8], &'a V)> {
        loop {
            match self.btnis.last_mut() {
                None => { return None }
                Some((node, token, key_start)) => {
                    let (new_token, key_bytes, rec, value) = node.next_items(*token);

                    if new_token == NODE_ITER_FINISHED {
                        self.btnis.pop();
                    } else {
                        *token = new_token;

                        self.prefix_buf.truncate(*key_start);
                        self.prefix_buf.extend(key_bytes);

                        match rec {
                            None => {},
                            Some(rec) => {
                                let child_node = rec.as_tagged();
                                let child_token = child_node.new_iter_token();
                                self.btnis.push((child_node, child_token, self.prefix_buf.len()));
                            },
                        }

                        match value {
                            Some(v) => return Some((&self.prefix_buf, v)),
                            None => {}
                        }
                    }
                }
            }
        }
    }
}


