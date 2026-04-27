use core::cell::UnsafeCell;

use super::PathMap;
use super::alloc::{Allocator, GlobalAlloc};
use super::trie_core::dense_byte::CellByteNode;
use super::trie_core::node::TrieNodeODRc;
use super::trie_core::node::*;
use super::zipper::zipper_priv::ZipperPriv;
use super::zipper::*;
use super::zipper_tracking::*;

pub trait ZipperCreation<'trie, V: Clone + Send + Sync, A: Allocator = GlobalAlloc> {
    /// Creates a new read-only [Zipper] with the path specified from the `ZipperHead`
    fn read_zipper_at_path<'a, K: AsRef<[u8]>>(
        &'a self,
        path: K,
    ) -> Result<ReadZipperTracked<'a, 'static, V, A>, Conflict>
    where
        'trie: 'a;

    /// A more efficient version of [read_zipper_at_path](ZipperCreation::read_zipper_at_path), where the returned
    /// zipper is constrained by the `'path` lifetime
    fn read_zipper_at_borrowed_path<'a, 'path>(
        &'a self,
        path: &'path [u8],
    ) -> Result<ReadZipperTracked<'a, 'path, V, A>, Conflict>
    where
        'trie: 'a;

    /// Creates a new read-only [Zipper] with the path specified from the `ZipperHead`, where the caller
    /// guarantees that there are and there never will be any conflicts with any [write zippers](ZipperWriting)s at this time
    /// or any time before the returned zipper is dropped
    ///
    /// The returned type is [ReadZipperTracked] although the tracking logic will be skipped in release mode.
    unsafe fn read_zipper_at_path_unchecked<'a, K: AsRef<[u8]>>(
        &'a self,
        path: K,
    ) -> ReadZipperTracked<'a, 'static, V, A>
    where
        'trie: 'a;

    /// A more efficient version of [read_zipper_at_path_unchecked](ZipperCreation::read_zipper_at_path_unchecked),
    /// where the returned zipper is constrained by the `'path` lifetime
    ///
    /// The returned type is [ReadZipperTracked] although the tracking logic will be skipped in release mode.
    unsafe fn read_zipper_at_borrowed_path_unchecked<'a, 'path>(
        &'a self,
        path: &'path [u8],
    ) -> ReadZipperTracked<'a, 'path, V, A>
    where
        'trie: 'a;

    //GOAT-TrackedOwnedZippers: This is a proposed feature to create owned variants of zippers, but still
    // track them using the ZipperHead infrastructure.  Creating owned zippers safely is easy to do and
    // doesn't require a new API.  However, those zippers will operate outside the management of a tracker,
    // and therefore you won't be able to use the tracker to protect regions of the trie.  Instead, conflicts
    // will result in reading an old version of the trie, or writing to a new location that overwrites existing
    // data when it's re-merged.  This isn't "corruption" at the pathmap level, but might be considered
    // corruption by the calling code.
    //
    // /// Creates a new [ReadZipperOwned] with the path specified from the `ZipperHead`
    // ///
    // /// This method has the advantage that the returned zipper will have a `'static` lifetime, making it possible
    // /// to safely send across async (tokio) threads, etc.  However, it has additional overhead vs. other
    // /// read-zipper creation methods such as [read_zipper_at_path](ZipperCreation::read_zipper_at_path).
    // fn owned_read_zipper_at_path<K: AsRef<[u8]>>(&self, path: K) -> Result<ReadZipperOwned<V>, Conflict>;

    /// Creates a new [write zippers](ZipperWriting) with the specified path from the `ZipperHead`
    fn write_zipper_at_exclusive_path<'a, K: AsRef<[u8]>>(
        &'a self,
        path: K,
    ) -> Result<WriteZipperTracked<'a, 'static, V, A>, Conflict>
    where
        'trie: 'a;

    /// Creates a new [write zippers](ZipperWriting) with the specified path from the `ZipperHead`, where the
    /// caller guarantees that no existing zippers may access the specified path at any time before the
    /// write zipper is dropped
    ///
    /// NOTE: Zippers created by this method are still tracked in debug mode.  `_unchecked` isn't permission to
    /// break the rules, it's just an optimization that affects when to spend time enforcing them.
    unsafe fn write_zipper_at_exclusive_path_unchecked<'a, K: AsRef<[u8]>>(
        &'a self,
        path: K,
    ) -> WriteZipperTracked<'a, 'static, V, A>
    where
        'trie: 'a;

    //GOAT-TrackedOwnedZippers
    // /// Creates a [WriteZipperOwned] from the specified path by temporarily cutting the trie
    // ///
    // /// This method creates a `'static` lifetime write zipper, which is useful to send across async (tokio)
    // /// threads, etc.  However, it comes with additional cost and requires the zipper to be replaced by calling
    // /// [replace_owned_write_zipper](ZipperCreation::replace_owned_write_zipper).
    // ///
    // /// If the zipper is not replaced (and is dropped instead) the effect will be the same as calling both
    // /// [WriteZipper::remove_branches], and [WriteZipper::remove_val].
    // fn take_owned_write_zipper_at_exclusive_path<K: AsRef<[u8]>>(&self, path: K) -> Result<WriteZipperOwned<V>, Conflict>;

    // /// Consumes a [WriteZipperOwned], and returns it to the trie from which it came
    // ///
    // /// This method is the inverse of [take_owned_write_zipper_at_exclusive_path](ZipperCreation::take_owned_write_zipper_at_exclusive_path).
    // ///
    // /// May panic if `zipper` did not originate from the `self` `ZipperHead`.
    // fn replace_owned_write_zipper(&self, zipper: WriteZipperOwned<V>);

    /// Reclaims ownership of a write zipper that was provided by the `ZipperHead` to ensure the zipper's
    /// root prefix path is pruned
    ///
    /// This method is necessary because the act of creating a write zipper at a path will create the parent
    /// path from the zipper head, but then a dangling path may remain after the zipper is dropped.
    ///
    /// May panic if `zipper` did not originate from the `self` `ZipperHead`.
    fn cleanup_write_zipper<Z: ZipperWriting<V, A> + ZipperAbsolutePath>(&self, z: Z);
}

trait ZipperCreationPriv<'trie, V, A: Allocator> {
    /// Internal method to access the WriteZipperCore within the ZipperHead
    fn with_inner_core_z<'a, Out, F>(&'a self, func: F) -> Out
    where
        F: FnOnce(&mut WriteZipperCore<'trie, 'static, V, A>) -> Out,
        V: 'trie;
    fn tracker_paths(&self) -> &SharedTrackerPaths;
}

/// Used to make multiple simultaneous zippers in the same map
//
//IMPLEMENTATION NOTE: The way to think about distribution of responsibility between WriteZipper and
// ZipperHead is that WriteZipper is responsible for the trie integrity machinery.  The purpose of the
// ZipperHead is to provide an client-facing API machanism to coordinate multiple zippers in the same tree
// safely.  Therefore it is possible to have a ZipperHead that sits at an ordinary node, or even in the
// middle of a node, however creating a WriteZipper means the node at the root of the WriteZipper must be
// upgraded to a CellByteNode.
pub struct ZipperHead<'parent, 'trie, V: Clone + Send + Sync, A: Allocator + 'trie = GlobalAlloc> {
    z: UnsafeCell<OwnedOrBorrowedMut<'parent, WriteZipperCore<'trie, 'static, V, A>>>,
    tracker_paths: SharedTrackerPaths,
}

// `ZipperHead` can be `Send` but absolutely must not be `Sync`!
unsafe impl<V: Clone + Send + Sync + Unpin, A: Allocator> Send for ZipperHead<'_, '_, V, A> {}

impl<'parent, 'trie: 'parent, V: Clone + Send + Sync + Unpin, A: Allocator>
    ZipperHead<'parent, 'trie, V, A>
{
    /// Internal method to create a new borrowed ZipperHead from a WriteZipper
    pub(crate) fn new_borrowed(
        parent_z: &'parent mut WriteZipperCore<'trie, 'static, V, A>,
    ) -> Self {
        Self {
            z: UnsafeCell::new(OwnedOrBorrowedMut::Borrowed(parent_z)),
            tracker_paths: SharedTrackerPaths::default(),
        }
    }

    /// Internal method to create a new ZipperHead by taking ownership of a WriteZipper
    pub(crate) fn new_owned(z: WriteZipperCore<'trie, 'static, V, A>) -> Self {
        Self {
            z: UnsafeCell::new(OwnedOrBorrowedMut::Owned(z)),
            tracker_paths: SharedTrackerPaths::default(),
        }
    }
}

enum OwnedOrBorrowedMut<'a, T> {
    Owned(T),
    Borrowed(&'a mut T),
}

impl<'trie, V: Clone + Send + Sync, A: Allocator> ZipperCreationPriv<'trie, V, A>
    for ZipperHead<'_, 'trie, V, A>
{
    fn with_inner_core_z<'a, Out, F>(&'a self, func: F) -> Out
    where
        F: FnOnce(&mut WriteZipperCore<'trie, 'static, V, A>) -> Out,
        V: 'trie,
    {
        let owned_or_borrowed_ref = unsafe { &mut *self.z.get() };
        let core_z = match owned_or_borrowed_ref {
            OwnedOrBorrowedMut::Owned(z) => z,
            OwnedOrBorrowedMut::Borrowed(z) => z,
        };
        func(core_z)
    }
    fn tracker_paths(&self) -> &SharedTrackerPaths {
        &self.tracker_paths
    }
}

super::impl_name_only_debug!(
    impl<V: Clone + Send + Sync + Unpin, A: Allocator> core::fmt::Debug for ZipperHead<'_, '_, V, A>
);

/// Similar to [ZipperHead], but owns the trie from which zippers are granted
///
/// `ZipperHeadOwned` is useful when managing the lifetime of an ordinary `ZipperHead` is unwieldy, as
/// often occurs in multi-threaded situations.
///
/// TODO: `ZipperHeadOwned` should be benchmarked against an ordinary `ZipperHead` to see how much
/// performance is lost.  There is a `Mutex` in `ZipperHeadOwned` so that it can be `Sync`, while the
/// ordinary `ZipperHead` uses an `UnsafeCell`.  However in a scenario where all the zipper-creation
/// activity was happening from the same thread, it's unclear how much cost in involved locking an
/// unlocking the mutex.
pub struct ZipperHeadOwned<V: Clone + Send + Sync + 'static, A: Allocator + 'static = GlobalAlloc> {
    z: std::sync::Mutex<WriteZipperOwned<V, A>>,
    tracker_paths: SharedTrackerPaths,
}

impl<'parent, 'trie: 'parent, V: Clone + Send + Sync + Unpin, A: Allocator> ZipperHeadOwned<V, A> {
    /// Internal method to create a new `ZipperHeadOwned` from a `WriteZipperOwned`
    pub(crate) fn new(mut z: WriteZipperOwned<V, A>) -> Self {
        // Make sure the zipper's path buffers are initialized if the path is non-zero length
        debug_assert!(z.core().key.node_key().is_empty() || z.core().key.prefix_buf.capacity() > 0);
        Self { z: std::sync::Mutex::new(z), tracker_paths: SharedTrackerPaths::default() }
    }
    /// Consumes the `ZipperHeadOwned` and returns a map containing the trie created by the zippers from
    /// the zipper head
    pub fn into_map(self) -> PathMap<V, A> {
        self.z.into_inner().unwrap().into_map()
    }
}

impl<V: Clone + Send + Sync + Unpin, A: Allocator> ZipperCreationPriv<'static, V, A>
    for ZipperHeadOwned<V, A>
{
    fn with_inner_core_z<Out, F>(&self, func: F) -> Out
    where
        F: FnOnce(&mut WriteZipperCore<'static, 'static, V, A>) -> Out,
        V: 'static,
    {
        let mut z = self.z.lock().unwrap();
        let core_z = z.core();
        func(core_z)
    }
    fn tracker_paths(&self) -> &SharedTrackerPaths {
        &self.tracker_paths
    }
}

super::impl_name_only_debug!(
    impl<V: Clone + Send + Sync + Unpin, A: Allocator> core::fmt::Debug for ZipperHeadOwned<V, A>
);

impl<'trie, Z, V: 'trie + Clone + Send + Sync + Unpin, A: Allocator + 'trie>
    ZipperCreation<'trie, V, A> for Z
where
    Z: ZipperCreationPriv<'trie, V, A>,
{
    fn read_zipper_at_borrowed_path<'a, 'path>(
        &'a self,
        path: &'path [u8],
    ) -> Result<ReadZipperTracked<'a, 'path, V, A>, Conflict>
    where
        'trie: 'a,
    {
        let zipper_tracker =
            Some(ZipperTracker::<TrackingRead>::new(self.tracker_paths().clone(), path)?);
        self.with_inner_core_z(|z| {
            let (root_node, root_val) = z.splitting_borrow_focus();

            //SAFETY: I am effectively taking items bound by `z`'s lifetime and lifting them up to the `'trie`
            // lifetime.  We need to do this because the ZipperHead internally uses a WriteZipper, which must
            // remain &mut accessible.  Safety is upheld by the fact that the ZipperHead exclusivity runtime
            // logic makes sure conflicting paths aren't permitted, so we should not get aliased &mut borrows
            let root_node: &'trie TrieNodeODRc<V, A> = unsafe { &*root_node };
            let root_val: Option<&'trie V> = root_val.map(|v| unsafe { &*v.as_ptr() });
            let new_zipper = ReadZipperTracked::new_with_node_and_path_in(
                root_node,
                true,
                path,
                path.len(),
                0,
                root_val,
                z.alloc.clone(),
                zipper_tracker,
            );
            Ok(new_zipper)
        })
    }
    unsafe fn read_zipper_at_borrowed_path_unchecked<'a, 'path>(
        &'a self,
        path: &'path [u8],
    ) -> ReadZipperTracked<'a, 'path, V, A>
    where
        'trie: 'a,
    {
        self.with_inner_core_z(|z| {
            let (root_node, root_val) = z.splitting_borrow_focus();

            //SAFETY: The user is asserting that the paths won't conflict.
            // See identical code in `read_zipper_at_borrowed_path` for more discussion
            let root_node: &'trie TrieNodeODRc<V, A> = unsafe { &*root_node };
            let root_val: Option<&'trie V> = root_val.map(|v| unsafe { &*v.as_ptr() });

            #[cfg(debug_assertions)]
            let zipper_tracker = Some(
                ZipperTracker::<TrackingRead>::new(self.tracker_paths().clone(), path)
                    .unwrap_or_else(|conflict| {
                        panic!("Fatal error. ReadZipper at {path:?} {conflict}")
                    }),
            );

            #[cfg(not(debug_assertions))]
            let zipper_tracker = None;

            ReadZipperTracked::new_with_node_and_path_in(
                root_node,
                true,
                path,
                path.len(),
                0,
                root_val,
                z.alloc.clone(),
                zipper_tracker,
            )
        })
    }
    fn read_zipper_at_path<'a, K: AsRef<[u8]>>(
        &'a self,
        path: K,
    ) -> Result<ReadZipperTracked<'a, 'static, V, A>, Conflict>
    where
        'trie: 'a,
    {
        let path = path.as_ref();
        let zipper_tracker =
            ZipperTracker::<TrackingRead>::new(self.tracker_paths().clone(), path)?;
        self.with_inner_core_z(|z| {
            let (root_node, root_val) = z.splitting_borrow_focus();

            //SAFETY: See identical code in `read_zipper_at_borrowed_path` for more discussion
            let root_node: &'trie TrieNodeODRc<V, A> = unsafe { &*root_node };
            let root_val: Option<&'trie V> = root_val.map(|v| unsafe { &*v.as_ptr() });

            let new_zipper = ReadZipperTracked::new_with_node_and_cloned_path_in(
                root_node,
                true,
                path,
                path.len(),
                0,
                root_val,
                z.alloc.clone(),
                Some(zipper_tracker),
            );
            Ok(new_zipper)
        })
    }
    unsafe fn read_zipper_at_path_unchecked<'a, K: AsRef<[u8]>>(
        &'a self,
        path: K,
    ) -> ReadZipperTracked<'a, 'static, V, A>
    where
        'trie: 'a,
    {
        let path = path.as_ref();
        self.with_inner_core_z(|z| {
            let (root_node, root_val) = z.splitting_borrow_focus();

            //SAFETY: The user is asserting that the paths won't conflict.
            // See identical code in `read_zipper_at_borrowed_path` for more discussion
            let root_node: &'trie TrieNodeODRc<V, A> = unsafe { &*root_node };
            let root_val: Option<&'trie V> = root_val.map(|v| unsafe { &*v.as_ptr() });

            #[cfg(debug_assertions)]
            let zipper_tracker = Some(
                ZipperTracker::<TrackingRead>::new(self.tracker_paths().clone(), path)
                    .unwrap_or_else(|conflict| {
                        panic!("Fatal error. ReadZipper at {path:?} {conflict}")
                    }),
            );

            #[cfg(not(debug_assertions))]
            let zipper_tracker = None;

            ReadZipperTracked::new_with_node_and_cloned_path_in(
                root_node,
                true,
                path,
                path.len(),
                0,
                root_val,
                z.alloc.clone(),
                zipper_tracker,
            )
        })
    }
    fn write_zipper_at_exclusive_path<'a, K: AsRef<[u8]>>(
        &'a self,
        path: K,
    ) -> Result<WriteZipperTracked<'a, 'static, V, A>, Conflict>
    where
        'trie: 'a,
    {
        let path = path.as_ref();
        let zipper_tracker =
            ZipperTracker::<TrackingWrite>::new(self.tracker_paths().clone(), path)?;
        self.with_inner_core_z(|z| {
            let (zipper_root_node, zipper_root_val) = prepare_exclusive_write_path(z, path);
            //SAFETY: See similar code in `read_zipper_at_borrowed_path` for more discussion
            let zipper_root_node: &'trie mut TrieNodeODRc<V, A> =
                unsafe { &mut *(zipper_root_node as *mut _) };
            let zipper_root_val: &'trie mut Option<V> =
                unsafe { &mut *(zipper_root_val as *mut _) };

            Ok(WriteZipperTracked::new_with_node_and_cloned_path_internal_in(
                zipper_root_node,
                Some(zipper_root_val),
                path,
                path.len(),
                z.alloc.clone(),
                Some(zipper_tracker),
            ))
        })
    }
    unsafe fn write_zipper_at_exclusive_path_unchecked<'a, K: AsRef<[u8]>>(
        &'a self,
        path: K,
    ) -> WriteZipperTracked<'a, 'static, V, A>
    where
        'trie: 'a,
    {
        let path = path.as_ref();
        self.with_inner_core_z(|z| {
            let (zipper_root_node, zipper_root_val) = prepare_exclusive_write_path(z, path);
            //SAFETY: The user is asserting that the paths won't conflict.
            // See similar code in `read_zipper_at_borrowed_path` for more discussion
            let zipper_root_node: &'trie mut TrieNodeODRc<V, A> =
                unsafe { &mut *(zipper_root_node as *mut _) };
            let zipper_root_val: &'trie mut Option<V> =
                unsafe { &mut *(zipper_root_val as *mut _) };

            //We still keep the trackers in debug mode
            #[cfg(debug_assertions)]
            let tracker = Some(
                ZipperTracker::<TrackingWrite>::new(self.tracker_paths().clone(), path)
                    .unwrap_or_else(|conflict| {
                        panic!("Fatal error. WriteZipper at {path:?} {conflict}")
                    }),
            );
            #[cfg(not(debug_assertions))]
            let tracker = None;

            WriteZipperTracked::new_with_node_and_cloned_path_internal_in(
                zipper_root_node,
                Some(zipper_root_val),
                path,
                path.len(),
                z.alloc.clone(),
                tracker,
            )
        })
    }
    fn cleanup_write_zipper<ChildZ: ZipperWriting<V, A> + ZipperAbsolutePath>(
        &self,
        mut z: ChildZ,
    ) {
        let origin_path = z.take_root_prefix_path();
        drop(z);
        self.with_inner_core_z(|inner_z| {
            //Sometimes people call `cleanup_write_zipper` in a drop method on a WZ wrapper, and the ZipperHead
            // has already been dismantled... So we are checking here in order to handle that situation gracefully
            if inner_z.focus_stack.top().is_some() {
                inner_z.move_to_path(origin_path);
                if inner_z.try_borrow_focus().unwrap().as_tagged().node_is_empty() {
                    inner_z.prune_path();
                }
                inner_z.reset();
            }
        })
    }
}

/// Ensures that the node at the specified path exists, and is a [CellByteNode]
///
/// Discussion: This function is fairly complicated because we are only able to safely access the top
///  of the focus stack.
/// There are 4 paths through this function:
/// 1. Both the WriteZipper and target path are at the root.  This is the only case where the reference
///  to the root_val is returned.
/// 2. The target path is below the focus root, in which case we traverse to the node
/// 3. The zipper focus doesn't exist, in which case we need to create it, and then follow one of the
///  other paths.
/// 4. The target path is the zipper focus
pub(crate) fn prepare_exclusive_write_path<
    'a,
    'trie: 'a,
    'path: 'a,
    V: Clone + Send + Sync + Unpin,
    A: Allocator + 'trie,
>(
    z: &'a mut WriteZipperCore<'trie, 'path, V, A>,
    path: &[u8],
) -> (&'a mut TrieNodeODRc<V, A>, &'a mut Option<V>) {
    //We need to start by making sure that the node at the root of the ZipperHead's WriteZipper is
    // unique, because a ReadZipper may have cloned that node, so we need to re-unique it
    if let Some(node) = z.try_borrow_focus_mut() {
        node.make_unique();
    }

    let node_key_start = z.key.node_key_start();

    //CASE 1
    //If we have a zero-length node_key and a zero-length path, we just need to make sure the root is a
    // CellNode, and we can return
    if node_key_start == z.key.prefix_buf.len() && path.is_empty() {
        //The only situation where a WriteZipper's node_key is zero-length is when the WZ is positioned at its root
        debug_assert_eq!(z.focus_stack.depth(), 1);
        z.focus_stack.to_root();
        let stack_root = z.focus_stack.root_mut().unwrap();
        make_cell_node(stack_root);
        let root_val = z.root_val.as_mut().unwrap();
        return (stack_root, unsafe { &mut **root_val });
    }

    //Otherwise we need to walk to the end of the path
    let original_path_len = z.key.prefix_buf.len();
    z.key.prefix_buf.extend(path);
    let last_path_byte = z.key.prefix_buf.pop().unwrap();

    //See below...
    let z_ptr: *mut _ = z;

    //Walk along the path to get the parent node
    let mut remaining_key = &z.key.prefix_buf[node_key_start..];
    if !remaining_key.is_empty() {
        match z.focus_stack.top_mut().unwrap().node_into_child_mut(remaining_key) {
            Some((consumed_bytes, node)) => {
                //CASE 2
                remaining_key = &remaining_key[consumed_bytes..];

                let end_node = prepare_node_at_path_end(node, remaining_key, z.alloc.clone());
                let cell_node = end_node.make_mut().into_cell_node().unwrap();
                let (exclusive_node, val) = cell_node.prepare_cf(last_path_byte);

                z.key.prefix_buf.truncate(original_path_len);

                (exclusive_node, val)
            }
            None => {
                //CASE 3

                //SAFETY: This is another "We need Polonius" case.  We're finished with the borrow if we get here.
                let alloc = z.alloc.clone();
                let z = unsafe { &mut *z_ptr };
                z.in_zipper_mut_static_result(
                    |node, key| {
                        let new_node = if !key.is_empty() {
                            if let Some(mut remaining) = node.take_node_at_key(key, false) {
                                make_cell_node(&mut remaining);
                                remaining
                            } else {
                                TrieNodeODRc::new_in(CellByteNode::new_in(alloc.clone()), alloc)
                            }
                        } else {
                            TrieNodeODRc::new_in(CellByteNode::new_in(alloc.clone()), alloc)
                        };
                        node.node_set_branch(key, new_node)
                    },
                    |_, _| true,
                );

                //Restore and fix the zipper if we added an intermediate node
                z.key.prefix_buf.truncate(original_path_len);
                if z.key.prefix_buf.len() < original_path_len {
                    z.key.prefix_buf.push(last_path_byte);
                }
                z.mend_root();
                z.descend_to_internal();

                //Try again, now that a CellNode at the zipper root has been created
                prepare_exclusive_write_path(z, path)
            }
        }
    } else {
        //CASE 4
        z.key.prefix_buf.truncate(original_path_len);

        //If the node on top of the stack is not a cell node, we need to upgrade it
        if !z.focus_stack.top().unwrap().is_cell_node() {
            swap_top_node(&mut z.focus_stack, &z.key, |mut existing_node| {
                make_cell_node(&mut existing_node);
                existing_node
            });
        }
        let cell_node = z.focus_stack.top_mut().unwrap().into_cell_node().unwrap();
        let (exclusive_node, val) = cell_node.prepare_cf(last_path_byte);
        (exclusive_node, val)
    }
}

/// Internal function.  Upgrades the node at the end of the `key` path to a CellByteNode
fn prepare_node_at_path_end<'a, V: Clone + Send + Sync, A: Allocator>(
    start_node: &'a mut TrieNodeODRc<V, A>,
    key: &[u8],
    alloc: A,
) -> &'a mut TrieNodeODRc<V, A> {
    let (remaining_key, mut node) = node_along_path_mut(start_node, key, false);

    //If remaining_key is non-zero length, split and upgrade the intervening node
    if !remaining_key.is_empty() {
        let mut node_ref = node.make_mut();
        let mut new_parent = match node_ref.take_node_at_key(remaining_key, false) {
            Some(downward_node) => downward_node,
            None => TrieNodeODRc::new_in(CellByteNode::new_in(alloc.clone()), alloc),
        };
        make_cell_node(&mut new_parent);
        let result = node_ref.node_set_branch(remaining_key, new_parent);
        match result {
            Ok(_) => {}
            Err(replacement_node) => {
                *node = replacement_node;
            }
        }
        let (new_remaining_key, child_node) = node_along_path_mut(node, remaining_key, false);
        debug_assert_eq!(new_remaining_key.len(), 0);
        node = child_node;
    } else {
        //Otherwise just upgrade node
        make_cell_node(node);
    }
    node
}

#[cfg(all(test, feature = "pathmap-internal-tests"))]
mod tests {
    use super::tests::prefix_key;
    use super::utils::ByteMask;
    use super::zipper::*;
    use super::{PathMap, utils::BitMask};
    use std::{thread, thread::ScopedJoinHandle};

    #[test]
    fn parallel_insert_test() {
        #[cfg(miri)]
        let elements = 32;
        #[cfg(all(feature = "pathmap-internal-tests", not(miri)))]
        let elements = 1024;

        let thread_cnt = 8;
        let elements_per_thread = elements / thread_cnt;

        let mut map = PathMap::<usize>::new();
        let zipper_head = map.zipper_head();

        thread::scope(|scope| {
            let elements_per_thread = elements / thread_cnt;

            //Preallocate all zippers
            let mut zippers = Vec::with_capacity(thread_cnt);
            for n in (0..thread_cnt).into_iter().rev() {
                let path = &[n as u8];
                let zipper = zipper_head.write_zipper_at_exclusive_path(path).unwrap();
                zippers.push(zipper);
            }

            let mut threads: Vec<ScopedJoinHandle<()>> = Vec::with_capacity(thread_cnt);

            //Spawn all the threads
            for n in 0..thread_cnt {
                let mut zipper = zippers.pop().unwrap();
                let thread = scope.spawn(move || {
                    for i in (n * elements_per_thread)..((n + 1) * elements_per_thread) {
                        zipper.descend_to(prefix_key(&(i as u64)));
                        assert!(zipper.set_val(i).is_none());
                        zipper.reset();
                    }
                });
                threads.push(thread);
            }

            //Wait for the threads to finish
            for thread in threads {
                thread.join().unwrap();
            }
        });
        drop(zipper_head);

        //Test that the values set by the threads are correct
        for n in 0..thread_cnt {
            for i in (n * elements_per_thread)..((n + 1) * elements_per_thread) {
                let mut path = vec![n as u8];
                path.extend(prefix_key(&(i as u64)));
                assert_eq!(map.get_val_at(path), Some(&i));
            }
        }
    }

    #[test]
    fn parallel_copy_values_test() {
        #[cfg(miri)]
        let elements = 32;
        #[cfg(all(feature = "pathmap-internal-tests", not(miri)))]
        let elements = 512;

        let thread_cnt: usize = 4;
        let elements_per_thread = elements / thread_cnt;

        let mut map = PathMap::<usize>::new();
        let mut zipper = map.write_zipper_at_path(b"in");
        for n in 0..thread_cnt {
            for i in (n * elements_per_thread)..((n + 1) * elements_per_thread) {
                zipper.descend_to_byte(n as u8);
                zipper.descend_to(i.to_be_bytes());
                zipper.set_val(i);
                zipper.reset();
            }
        }
        drop(zipper);

        let zipper_head = map.zipper_head();

        std::thread::scope(|scope| {
            let mut zipper_senders: Vec<
                std::sync::mpsc::Sender<(
                    ReadZipperTracked<'_, '_, usize>,
                    WriteZipperTracked<'_, '_, usize>,
                )>,
            > = Vec::with_capacity(thread_cnt);
            let mut signal_receivers: Vec<std::sync::mpsc::Receiver<bool>> =
                Vec::with_capacity(thread_cnt);

            //Spawn all the threads
            for _thread_idx in 0..thread_cnt {
                let (zipper_tx, zipper_rx) = std::sync::mpsc::channel();
                zipper_senders.push(zipper_tx);
                let (signal_tx, signal_rx) = std::sync::mpsc::channel::<bool>();
                signal_receivers.push(signal_rx);

                scope.spawn(move || {
                    loop {
                        let mut sanity_counter = 0;

                        //The thread will block here waiting for the zippers to be sent
                        match zipper_rx.recv() {
                            Ok((mut reader_z, mut writer_z)) => {
                                //We got the zippers, do the stuff
                                let witness = reader_z.witness();
                                while let Some(val) =
                                    reader_z.to_next_get_val_with_witness(&witness)
                                {
                                    writer_z.descend_to(reader_z.path());
                                    writer_z.set_val(*val);
                                    writer_z.reset();

                                    sanity_counter += 1;
                                }

                                assert_eq!(sanity_counter, elements_per_thread);

                                //Tell the main thread we're done
                                signal_tx.send(true).unwrap();
                            }
                            Err(_) => {
                                //The zipper_sender channel is closed, meaning it's time to shut down
                                break;
                            }
                        }
                    }
                });
            }

            let mut writer_z = zipper_head.write_zipper_at_exclusive_path(b"out").unwrap();
            writer_z.remove_branches(true);
            drop(writer_z);

            //Dispatch a zipper to each thread
            for n in 0..thread_cnt {
                let path = vec![b'o', b'u', b't', n as u8];
                let writer_z =
                    unsafe { zipper_head.write_zipper_at_exclusive_path_unchecked(path) };
                let path = vec![b'i', b'n', n as u8];
                let reader_z = unsafe { zipper_head.read_zipper_at_path_unchecked(path) };

                zipper_senders[n].send((reader_z, writer_z)).unwrap();
            }

            //Wait for the threads to all be done
            for n in 0..thread_cnt {
                assert_eq!(signal_receivers[n].recv().unwrap(), true);
            }
        });
        drop(zipper_head);
    }

    #[test]
    fn zipper_head1() {
        let mut map = PathMap::<isize>::new();

        //Make a ZipperHead for the whole map, and make a WriteZipper for a branch within the map
        let map_head = map.zipper_head();
        let mut zipper = map_head.write_zipper_at_exclusive_path(&[0]).unwrap();
        zipper.set_val(0);
        drop(zipper);
        drop(map_head);
        assert_eq!(map.get_val_at(&[0]), Some(&0));
    }

    #[test]
    fn zipper_head2() {
        let mut map = PathMap::<isize>::new();

        //Make a ZipperHead for the whole map, and then a zipper at the root
        //This degenerate case should be identical to making a WriteZipper from the map root
        let map_head = map.zipper_head();
        let mut zipper = map_head.write_zipper_at_exclusive_path(&[]).unwrap();
        zipper.descend_to(b"test");
        zipper.set_val(0);
        drop(zipper);
        drop(map_head);
        assert_eq!(map.get_val_at("test"), Some(&0));
    }

    #[test]
    fn zipper_head3() {
        let mut map = PathMap::<isize>::new();

        //Make a WriteZipper in a plece that will require creating multiple nodes
        let map_head = map.zipper_head();
        let mut zipper = map_head.write_zipper_at_exclusive_path(b"test").unwrap();
        zipper.descend_to(b":2");
        zipper.set_val(2);
        drop(zipper);
        drop(map_head);
        assert_eq!(map.get_val_at("test:2"), Some(&2));
    }

    #[test]
    fn zipper_head4() {
        let mut map = PathMap::<isize>::new();

        //Make a WriteZipper in a place that will require splitting an existing path
        map.set_val_at(b"test:3", 3);
        let map_head = map.zipper_head();
        let mut zipper = map_head.write_zipper_at_exclusive_path(b"test").unwrap();
        zipper.descend_to(b":3");
        assert!(zipper.path_exists());
        assert_eq!(zipper.val(), Some(&3));
        zipper.ascend_byte();
        zipper.descend_to_byte(b'2');
        zipper.set_val(2);
        drop(zipper);
        drop(map_head);

        assert_eq!(map.get_val_at("test:2"), Some(&2));
        assert_eq!(map.get_val_at("test:3"), Some(&3));
    }

    #[test]
    fn zipper_head5() {
        let mut map = PathMap::<isize>::new();

        //Work around a "stump" (aka a zipper root, aka a CellByteNodes that belonged to a zipper that was dropped)
        let map_head = map.zipper_head();
        let zipper = map_head.write_zipper_at_exclusive_path([3]).unwrap();
        drop(zipper);
        let mut zipper = map_head.write_zipper_at_exclusive_path([3, 193, 49]).unwrap();
        zipper.descend_to_byte(42);
        zipper.set_val(42);
        drop(zipper);
        drop(map_head);
        assert_eq!(map.get_val_at([3, 193, 49, 42]), Some(&42));
    }

    #[test]
    fn zipper_head6() {
        let mut map = PathMap::<isize>::new();

        //Make sure that inserting a WriteZipper doesn't chop off any downstream parts of the trie
        map.set_val_at(b"test:1", 1);
        map.set_val_at(b"test:2", 2);
        map.set_val_at(b"test:3", 3);
        map.set_val_at(b"test:4", 4);
        let map_head = map.zipper_head();
        let mut zipper = map_head.write_zipper_at_exclusive_path(b"test").unwrap();
        zipper.descend_to(b":3");
        assert!(zipper.path_exists());
        assert_eq!(zipper.val(), Some(&3));
        zipper.ascend_byte();
        zipper.descend_to_byte(b'5');
        zipper.set_val(5);
        drop(zipper);
        drop(map_head);

        assert_eq!(map.get_val_at("test:1"), Some(&1));
        assert_eq!(map.get_val_at("test:2"), Some(&2));
        assert_eq!(map.get_val_at("test:3"), Some(&3));
        assert_eq!(map.get_val_at("test:4"), Some(&4));
        assert_eq!(map.get_val_at("test:5"), Some(&5));
    }

    #[test]
    fn zipper_head7() {
        let mut map = PathMap::<isize>::new();

        //Make sure I can upgrade an ordinary ByteNode into a CellNode without losing anything
        map.set_val_at(b"test:1", 1);
        map.set_val_at(b"test:2", 2);
        map.set_val_at(b"test:3", 3);
        map.set_val_at(b"test:4", 4);
        let map_head = map.zipper_head();
        let mut zipper = map_head.write_zipper_at_exclusive_path(b"test:").unwrap();
        zipper.descend_to(b"3");
        assert!(zipper.path_exists());
        assert_eq!(zipper.val(), Some(&3));
        zipper.ascend_byte();
        zipper.descend_to_byte(b'5');
        zipper.set_val(5);
        drop(zipper);
        drop(map_head);

        assert_eq!(map.get_val_at("test:1"), Some(&1));
        assert_eq!(map.get_val_at("test:2"), Some(&2));
        assert_eq!(map.get_val_at("test:3"), Some(&3));
        assert_eq!(map.get_val_at("test:4"), Some(&4));
        assert_eq!(map.get_val_at("test:5"), Some(&5));
    }
    /// Tests a zipper head that starts from a path other than the map root
    #[test]
    fn zipper_head8() {
        let mut map = PathMap::<isize>::new();
        map.set_val_at(b"start:0000:hello", 0);

        let mut wz = map.write_zipper();
        wz.descend_to(b"start:");
        let zh = wz.zipper_head();

        let mut z0 = zh.write_zipper_at_exclusive_path(b"0000").unwrap();
        z0.descend_to(b":goodbye");
        z0.set_val(0);

        drop(z0);
        drop(zh);
        drop(wz);
        assert_eq!(map.val_count(), 2);
        assert_eq!(map.get_val_at(b"start:0000:hello"), Some(&0));
        assert_eq!(map.get_val_at(b"start:0000:goodbye"), Some(&0));
    }
    /// A test for the tracker logic, testing many parallel [WriteZipper]s at once
    #[test]
    fn zipper_head9() {
        let mut map = PathMap::<isize>::new();
        map.set_val_at(b"start:0000:hello", 0);
        map.set_val_at(b"start:0001:hello", 1);
        map.set_val_at(b"start:0002:hello", 2);
        map.set_val_at(b"start:0003:hello", 3);

        let mut wz = map.write_zipper();
        wz.descend_to(b"start:");
        let zh = wz.zipper_head();

        let mut z0 = zh.write_zipper_at_exclusive_path(b"0000").unwrap();
        let mut z1 = zh.write_zipper_at_exclusive_path(b"0001").unwrap();
        let mut z2 = zh.write_zipper_at_exclusive_path(b"0002").unwrap();
        let mut z3 = zh.write_zipper_at_exclusive_path(b"0003").unwrap();

        z0.descend_to(b":goodbye");
        z0.set_val(0);
        z1.descend_to(b":goodbye");
        z1.set_val(1);
        z2.descend_to(b":goodbye");
        z2.set_val(2);
        z3.descend_to(b":goodbye");
        z3.set_val(3);

        drop(z0);
        drop(z1);
        drop(z2);
        drop(z3);
        drop(zh);
        drop(wz);

        assert_eq!(map.val_count(), 8);
        assert_eq!(map.get_val_at("start:0000:hello"), Some(&0));
        assert_eq!(map.get_val_at("start:0000:goodbye"), Some(&0));
        assert_eq!(map.get_val_at("start:0003:hello"), Some(&3));
        assert_eq!(map.get_val_at("start:0003:goodbye"), Some(&3));
    }

    /// Test more cases in the logic to upgrade nodes before creating zippers
    #[test]
    fn zipper_heada() {
        //This hits the case where we attempt to make a WriteZipper rooted in the middle of an existing ListNode
        // This tests the `prepare_node_at_path_end` code path in the WriteZipper creation
        let mut map = PathMap::<()>::new();
        map.set_val_at([1], ());
        map.set_val_at([2], ());
        map.set_val_at([3, 193, 4], ());
        map.set_val_at([3, 194, 21, 134], ());
        map.set_val_at([3, 194, 21, 133], ());

        let zh = map.zipper_head();
        let wz = zh.write_zipper_at_exclusive_path([3, 194, 21]).unwrap();
        drop(wz);
        drop(zh);
        drop(map);

        //This hits the case where we need to upgrade a node that was used to make the root of a ReadZipper
        // This tests the `splitting_borrow_focus` code path in ReadZipper creation
        let mut map = PathMap::<()>::new();
        map.set_val_at([1], ());
        map.set_val_at([2], ());
        map.set_val_at([3, 193, 4], ());
        map.set_val_at([3, 194, 21, 134], ());
        map.set_val_at([3, 194, 21, 133], ());

        let zh = map.zipper_head();
        let mut rz = zh.read_zipper_at_borrowed_path(&[3, 194, 21]).unwrap();
        let mut wz = zh.write_zipper_at_exclusive_path(&[3, 194, 22]).unwrap();

        assert_eq!(rz.val(), None);
        assert!(rz.descend_first_byte());
        assert_eq!(rz.val(), Some(&()));

        assert_eq!(wz.val(), None);
        assert!(wz.set_val(()).is_none());

        rz.reset();
        assert_eq!(rz.val(), None);
        assert!(rz.descend_first_byte());
        assert_eq!(rz.val(), Some(&()));

        drop(wz);
        drop(rz);
        drop(zh);
        assert_eq!(map.get_val_at([3, 194, 22]), Some(&()));

        //Similar to above case, but for the ReadZipper that uses a cloned path
        let mut map = PathMap::<usize>::new();
        map.set_val_at([1], 1000);
        map.set_val_at([2], 1001);
        map.set_val_at([3, 193, 4], 1002);
        map.set_val_at([3, 194, 21, 134], 1003);
        map.set_val_at([3, 194, 21, 133], 1004);

        let zh = map.zipper_head();
        let mut rz = zh.read_zipper_at_path(&[3, 194, 21]).unwrap();
        let rz2 = zh.read_zipper_at_path(&[3, 194, 21, 134]).unwrap();
        let mut wz = zh.write_zipper_at_exclusive_path(&[3, 194, 22]).unwrap();

        assert_eq!(rz.val(), None);
        assert!(rz.descend_first_byte());
        assert_eq!(rz.val(), Some(&1004));

        assert_eq!(rz2.val(), Some(&1003));

        assert_eq!(wz.val(), None);
        assert!(wz.set_val(1005).is_none());

        assert_eq!(rz2.val(), Some(&1003));

        drop(wz);
        drop(rz);
        drop(rz2);
        drop(zh);
        assert_eq!(map.get_val_at([3, 194, 22]), Some(&1005));
    }

    /// Dance a bunch of readers and writers inside the same zipper head
    #[test]
    fn zipper_headb() {
        let space = PathMap::<()>::new().into_zipper_head(&[]);

        let mut wz = unsafe {
            space.write_zipper_at_exclusive_path_unchecked(&[2, 200, 0, 0, 0, 0, 0, 0, 0, 4])
        };
        wz.descend_to(&[200, 0, 0, 0, 0, 0, 0, 0, 5]);
        wz.set_val(());
        drop(wz);

        let mut wz = unsafe {
            space.write_zipper_at_exclusive_path_unchecked(&[4, 200, 0, 0, 0, 0, 0, 0, 0, 6])
        };
        wz.descend_to(&[
            2, 200, 0, 0, 0, 0, 0, 0, 0, 4, 200, 0, 0, 0, 0, 0, 0, 0, 7, 2, 200, 0, 0, 0, 0, 0, 0,
            0, 3, 2, 200, 0, 0, 0, 0, 0, 0, 0, 4, 2, 200, 0, 0, 0, 0, 0, 0, 0, 8, 192, 2, 200, 0,
            0, 0, 0, 0, 0, 0, 3, 2, 200, 0, 0, 0, 0, 0, 0, 0, 4, 2, 200, 0, 0, 0, 0, 0, 0, 0, 9,
            128,
        ]);
        wz.set_val(());
        wz.reset();
        wz.descend_to(&[
            2, 200, 0, 0, 0, 0, 0, 0, 0, 4, 200, 0, 0, 0, 0, 0, 0, 0, 7, 2, 200, 0, 0, 0, 0, 0, 0,
            0, 3, 2, 200, 0, 0, 0, 0, 0, 0, 0, 4, 200, 0, 0, 0, 0, 0, 0, 0, 5, 2, 200, 0, 0, 0, 0,
            0, 0, 0, 3, 2, 200, 0, 0, 0, 0, 0, 0, 0, 4, 200, 0, 0, 0, 0, 0, 0, 0, 10,
        ]);
        wz.set_val(());
        drop(wz);

        let mut wz = unsafe {
            space.write_zipper_at_exclusive_path_unchecked(&[
                4, 200, 0, 0, 0, 0, 0, 0, 0, 6, 2, 200, 0, 0, 0, 0, 0, 0, 0, 4,
            ])
        };
        wz.descend_to(&[
            200, 0, 0, 0, 0, 0, 0, 0, 7, 2, 200, 0, 0, 0, 0, 0, 0, 0, 3, 2, 200, 0, 0, 0, 0, 0, 0,
            0, 4, 2, 200, 0, 0, 0, 0, 0, 0, 0, 8, 192, 2, 200, 0, 0, 0, 0, 0, 0, 0, 3, 2, 200, 0,
            0, 0, 0, 0, 0, 0, 4, 2, 200, 0, 0, 0, 0, 0, 0, 0, 9, 128,
        ]);
        wz.remove_val(true);
        drop(wz);

        let wz = unsafe {
            space.write_zipper_at_exclusive_path_unchecked(&[
                2, 200, 0, 0, 0, 0, 0, 0, 0, 4, 2, 200, 0, 0, 0, 0, 0, 0, 0, 9,
            ])
        };
        drop(wz);

        let rz = unsafe {
            space.read_zipper_at_borrowed_path_unchecked(&[
                2, 200, 0, 0, 0, 0, 0, 0, 0, 4, 2, 200, 0, 0, 0, 0, 0, 0, 0, 8,
            ])
        };
        drop(rz);

        let mut wz = unsafe {
            space.write_zipper_at_exclusive_path_unchecked(&[
                4, 200, 0, 0, 0, 0, 0, 0, 0, 6, 2, 200, 0, 0, 0, 0, 0, 0, 0, 4,
            ])
        };
        wz.descend_to(&[
            200, 0, 0, 0, 0, 0, 0, 0, 7, 2, 200, 0, 0, 0, 0, 0, 0, 0, 3, 2, 200, 0, 0, 0, 0, 0, 0,
            0, 4, 200, 0, 0, 0, 0, 0, 0, 0, 5, 2, 200, 0, 0, 0, 0, 0, 0, 0, 3, 2, 200, 0, 0, 0, 0,
            0, 0, 0, 4, 200, 0, 0, 0, 0, 0, 0, 0, 10,
        ]);
        wz.remove_val(true);
        drop(wz);

        let wz = unsafe {
            space.write_zipper_at_exclusive_path_unchecked(&[
                2, 200, 0, 0, 0, 0, 0, 0, 0, 4, 200, 0, 0, 0, 0, 0, 0, 0, 10,
            ])
        };
        drop(wz);

        let rz = unsafe {
            space.read_zipper_at_borrowed_path_unchecked(&[
                2, 200, 0, 0, 0, 0, 0, 0, 0, 4, 200, 0, 0, 0, 0, 0, 0, 0, 5,
            ])
        };
        drop(rz)
    }

    /// Test if causing a node to upgrade within a ZipperHead breaks anything
    #[test]
    fn zipper_headc() {
        let mut space = PathMap::<()>::new();
        space.set_val_at(b"A:rd1", ());
        space.set_val_at(b"A:rd2", ());
        let zh = space.zipper_head();

        //Sanity check.  Validate that we see everything the items via a reader
        let rz1 = zh.read_zipper_at_borrowed_path(b"A:").unwrap();
        assert_eq!(rz1.val_count(), 2);

        //Cause the node that supports the reader to be upgraded from a PairNode to a ByteNode
        let _wz1 = zh.write_zipper_at_exclusive_path(b"B:wt").unwrap();
        let _wz2 = zh.write_zipper_at_exclusive_path(b"C:wt").unwrap();
        let _wz3 = zh.write_zipper_at_exclusive_path(b"D:wt").unwrap();

        //Check we can re-create a reader, and see all the right stuff
        let rz2 = zh.read_zipper_at_borrowed_path(b"A:").unwrap();
        assert_eq!(rz2.val_count(), 2);

        //Check that our original reader is still valid
        assert_eq!(rz1.val_count(), 2);
    }

    /// Test stability of ReadZipper root values when parent nodes in a ZipperHead are upgraded
    #[test]
    fn zipper_headd() {
        let mut space = PathMap::<usize>::new();
        space.set_val_at(b"A", 42);
        space.set_val_at(b"A:rd1", 1);
        space.set_val_at(b"A:rd2", 2);
        let zh = space.zipper_head();

        //Sanity check.  Validate that we see everything the items via a reader
        let mut rz1 = zh.read_zipper_at_borrowed_path(b"A").unwrap();
        let rz1_witness = rz1.witness();
        assert_eq!(rz1.get_val_with_witness(&rz1_witness), Some(&42));
        rz1.descend_to(":rd1");
        assert_eq!(rz1.path_exists(), true);
        assert_eq!(rz1.get_val_with_witness(&rz1_witness), Some(&1));
        assert_eq!(rz1.move_to_path(":rd2"), 3);
        assert_eq!(rz1.get_val_with_witness(&rz1_witness), Some(&2));

        //Cause the node that supports the reader to be upgraded from a PairNode to a ByteNode
        let _wz1 = zh.write_zipper_at_exclusive_path(b"B:wt").unwrap();
        let _wz2 = zh.write_zipper_at_exclusive_path(b"C:wt").unwrap();
        let _wz3 = zh.write_zipper_at_exclusive_path(b"D:wt").unwrap();

        //Check we can re-create a reader, and see all the right stuff
        let mut rz2 = zh.read_zipper_at_borrowed_path(b"A").unwrap();
        let rz2_witness = rz2.witness();
        assert_eq!(rz2.get_val_with_witness(&rz2_witness), Some(&42));
        rz2.descend_to(":rd1");
        assert_eq!(rz2.path_exists(), true);
        assert_eq!(rz2.get_val_with_witness(&rz2_witness), Some(&1));
        assert_eq!(rz2.move_to_path(":rd2"), 3);
        assert_eq!(rz2.get_val_with_witness(&rz2_witness), Some(&2));

        //Check that our original reader is still valid
        rz1.reset();
        assert_eq!(rz1.get_val_with_witness(&rz1_witness), Some(&42));
        rz1.descend_to(":rd1");
        assert_eq!(rz1.path_exists(), true);
        assert_eq!(rz1.get_val_with_witness(&rz1_witness), Some(&1));
        assert_eq!(rz1.move_to_path(":rd2"), 3);
        assert_eq!(rz1.get_val_with_witness(&rz1_witness), Some(&2));
    }

    #[test]
    fn zipper_heade() {
        let mut map = PathMap::new();
        map.set_val_at(b"path", 42);
        let zh = map.zipper_head();
        let rz = zh.read_zipper_at_path(b"path").unwrap();
        let rz_witness = rz.witness();
        let val_ref = rz.get_val_with_witness(&rz_witness).unwrap();
        assert_eq!(*val_ref, 42);
        drop(rz);

        assert_eq!(*val_ref, 42);
        drop(rz_witness);

        //Now that the witness is gone, validate I can create the WZ, and remove the value
        let mut wz = zh.write_zipper_at_exclusive_path(b"path").unwrap();
        wz.remove_val(true);
        drop(wz);
        drop(zh);
        assert_eq!(map.get_val_at(b"path"), None);
    }

    /// Test that a ReadZipper pointing at empty root behaves as it should
    #[test]
    fn zipper_headf() {
        let mut map = PathMap::new();
        map.set_val_at(b"ABCDEFG", 42);
        let zh = map.zipper_head();

        // Test *at* the end of a path that exists
        let mut z = zh.read_zipper_at_path(b"ABCDEFG").unwrap();
        assert_eq!(z.val(), Some(&42));
        assert_eq!(z.child_count(), 0);
        assert_eq!(z.child_mask(), ByteMask::EMPTY);
        assert_eq!(z.path_exists(), true);
        assert_eq!(z.to_next_sibling_byte(), false);
        assert_eq!(z.ascend_byte(), false);

        // Test creating a zipper at a path that doesn't exist
        let mut z2 = zh.read_zipper_at_path(b"ABCDEFGH").unwrap();
        assert_eq!(z2.val(), None);
        assert_eq!(z2.child_count(), 0);
        assert_eq!(z2.child_mask(), ByteMask::EMPTY);
        assert_eq!(z2.to_next_sibling_byte(), false);
        assert_eq!(z2.ascend_byte(), false);

        //Conceptually this should be `false`, but the act of creating the ReadZipper currently creates
        // the path - which is incorrect behavior and should be fixed!
        assert_eq!(z2.path_exists(), false);
    }

    /// This test ensures that a ReadZipper isn't invalidated by creating another ReadZipper at an
    /// overlapping path
    #[test]
    fn zipper_headg() {
        let mut map = PathMap::new();
        map.set_val_at(b"ABCDEFG", 42);
        map.set_val_at(b"A", 24);
        let zh = map.zipper_head();

        // Create a zipper and test to make sure it behaves properly
        let mut z = zh.read_zipper_at_path(b"A").unwrap();
        assert_eq!(z.val(), Some(&24));
        assert_eq!(z.to_next_sibling_byte(), false);
        z.descend_until();
        assert_eq!(z.path(), b"BCDEFG");
        assert_eq!(z.origin_path(), b"ABCDEFG");
        assert_eq!(z.val(), Some(&42));
        assert_eq!(z.to_next_sibling_byte(), false);

        // Create a second zipper and ensure it's valid
        let mut z2 = zh.read_zipper_at_path(z.origin_path()).unwrap();
        assert_eq!(z2.path(), b"");
        assert_eq!(z2.origin_path(), b"ABCDEFG");
        assert_eq!(z2.val(), Some(&42));
        assert_eq!(z2.to_next_sibling_byte(), false);

        // Test the original zipper
        assert_eq!(z.val(), Some(&42));
        assert_eq!(z.to_next_sibling_byte(), false);
        assert_eq!(z.ascend_until(), true);
        assert_eq!(z.path(), b"");
        assert_eq!(z.origin_path(), b"A");
        assert_eq!(z.val(), Some(&24));
        assert_eq!(z.to_next_sibling_byte(), false);
    }

    /// Similar test to zipper_headc, but here we are testing to make sure there are no issues when
    /// the read zipper clones (by reference) a descended node, rather than cloning the node at the
    /// ZipperHead's root
    #[test]
    fn zipper_headh() {
        let mut space = PathMap::<()>::new();
        space.set_val_at(b"A:read_path_that_is_long_enough_to_spill_past_a_single_node1", ());
        space.set_val_at(b"A:read_path_that_is_long_enough_to_spill_past_a_single_node2", ());
        let zh = space.zipper_head();

        //Sanity check.  Validate that we see everything the items via a reader
        let rz1 = zh
            .read_zipper_at_borrowed_path(
                b"A:read_path_that_is_long_enough_to_spill_past_a_single_node",
            )
            .unwrap();
        assert_eq!(rz1.val_count(), 2);

        //Cause the node that supports the reader to be upgraded from a PairNode to a ByteNode
        let _wz1 = zh.write_zipper_at_exclusive_path(b"B:wt").unwrap();
        let _wz2 = zh.write_zipper_at_exclusive_path(b"C:wt").unwrap();
        let _wz3 = zh.write_zipper_at_exclusive_path(b"D:wt").unwrap();

        //Check we can re-create a reader, and see all the right stuff
        let rz2 = zh
            .read_zipper_at_borrowed_path(
                b"A:read_path_that_is_long_enough_to_spill_past_a_single_node",
            )
            .unwrap();
        assert_eq!(rz2.val_count(), 2);

        //Check that our original reader is still valid
        assert_eq!(rz1.val_count(), 2);
    }

    /// Similar test to zipper_headc, but we create the zipper head far from the root of the WriteZipper
    #[test]
    fn zipper_headi() {
        let mut space = PathMap::<()>::new();
        space.set_val_at(b"path_that_is_long_enough_to_spill_past_a_single_node:A:rd1", ());
        space.set_val_at(b"path_that_is_long_enough_to_spill_past_a_single_node:A:rd2", ());
        let mut space_wz = space.write_zipper();
        space_wz.descend_to(b"path_that_is_long_enough_to_spill_past_a_single_node:");
        let zh = space_wz.zipper_head();

        //Sanity check.  Validate that we see everything the items via a reader
        let rz1 = zh.read_zipper_at_borrowed_path(b"A:").unwrap();
        assert_eq!(rz1.val_count(), 2);

        //Cause the node that supports the reader to be upgraded from a PairNode to a ByteNode
        let _wz1 = zh.write_zipper_at_exclusive_path(b"B:wt").unwrap();
        let _wz2 = zh.write_zipper_at_exclusive_path(b"C:wt").unwrap();
        let _wz3 = zh.write_zipper_at_exclusive_path(b"D:wt").unwrap();

        //Check we can re-create a reader, and see all the right stuff
        let rz2 = zh.read_zipper_at_borrowed_path(b"A:").unwrap();
        assert_eq!(rz2.val_count(), 2);

        //Check that our original reader is still valid
        assert_eq!(rz1.val_count(), 2);
    }

    /// A test that mimicks the usage of ZipperHead inside MORK's MM2 evaluator
    #[test]
    fn zipper_headj() {
        let paths: [&[u8]; _] = [
            &[4, 193, 95, 1, 193, 95, 193, 95, 193, 95],
            &[4, 193, 95, 2, 193, 95, 193, 95, 193, 95, 193, 95],
            &[
                4, 196, 101, 120, 101, 99, 193, 95, 2, 193, 44, 196, 116, 114, 117, 101, 2, 193,
                44, 4, 196, 95, 95, 95, 95, 193, 95, 1, 193, 95, 1, 193, 95,
            ],
            &[196, 116, 114, 117, 101],
        ];
        let mut space = PathMap::<()>::from_iter(paths.into_iter());

        space.remove([
            4, 196, 101, 120, 101, 99, 193, 95, 2, 193, 44, 196, 116, 114, 117, 101, 2, 193, 44, 4,
            196, 95, 95, 95, 95, 193, 95, 1, 193, 95, 1, 193, 95,
        ]);

        let mut read_copy = space.clone();
        let zh = space.zipper_head();
        read_copy.insert(
            [
                4, 196, 101, 120, 101, 99, 193, 95, 2, 193, 44, 196, 116, 114, 117, 101, 2, 193,
                44, 4, 196, 95, 95, 95, 95, 193, 95, 1, 193, 95, 1, 193, 95,
            ],
            (),
        );

        let _wz = zh
            .write_zipper_at_exclusive_path([
                4, 196, 95, 95, 95, 95, 193, 95, 1, 193, 95, 1, 193, 95,
            ])
            .unwrap();
    }

    /// Tests using a ZipperHead to create a zipper at a dangling path that already exists
    #[test]
    fn zipper_headk() {
        let mut map: PathMap<()> = PathMap::new();
        map.create_path(&[1, 255, 0]);
        let zh = map.zipper_head();
        let _rz = zh.read_zipper_at_path(&[1, 255, 0]).unwrap();
    }

    #[test]
    fn hierarchical_zipper_heads1() {
        let mut map = PathMap::<isize>::new();

        //Make a ZipperHead for the whole map, and two child zippers
        let map_head = map.zipper_head();
        let mut a_zipper = map_head.write_zipper_at_exclusive_path(b"a").unwrap();
        let mut b_zipper = map_head.write_zipper_at_exclusive_path(b"b").unwrap();

        //Do some interleaved work with the two zippers
        a_zipper.descend_to(b"+value");
        a_zipper.set_val(0);
        a_zipper.reset();
        b_zipper.descend_to(b"+value");
        b_zipper.set_val(1);
        b_zipper.reset();

        //Try pre-creating trie in the parent that will be visited by the child zipper
        b_zipper.descend_to(b"-children-0+metadata");
        b_zipper.set_val(-3);
        b_zipper.ascend(10);

        //Make a ZipperHead on the WriteZipper, and make two more parallel zippers
        let b_head = b_zipper.zipper_head();
        let mut b0_zipper = b_head.write_zipper_at_exclusive_path(b"0").unwrap();
        let mut b1_zipper = b_head.write_zipper_at_exclusive_path(b"1").unwrap();

        //Do some interleaved work with them
        b0_zipper.descend_to(b"+value");
        b0_zipper.set_val(4);
        b1_zipper.descend_to(b"+value");
        b1_zipper.set_val(-5);

        //Drop the child zippers, so we can move the parent again
        drop(b0_zipper);
        drop(b1_zipper);
        drop(b_head);

        //Visit some of the nodes the child zippers poked at, and fix their values with the parent
        b_zipper.descend_to(b"0+metadata");
        b_zipper.set_val(3);
        b_zipper.reset();
        b_zipper.descend_to(b"-children-1+value");
        b_zipper.set_val(5);

        //Test chopping an existing non-forking path, and inserting a new ZipperHead in there
        b_zipper.reset();
        b_zipper.descend_to(b"-children-0+meta");
        let b_head = b_zipper.zipper_head();
        let mut b0_zipper = b_head.write_zipper_at_exclusive_path([]).unwrap();
        b0_zipper.descend_to(b"bolic");
        b0_zipper.set_val(6);
        drop(b0_zipper);

        //Test making a ZipperHead when the parent WriteZipper is at a location that does not exist yet
        a_zipper.reset();
        a_zipper.descend_to(b"-children-");
        let a_head = a_zipper.zipper_head();
        let mut a0_zipper = a_head.write_zipper_at_exclusive_path("0").unwrap();
        a0_zipper.descend_to(b"+value");
        a0_zipper.set_val(7);
        drop(a0_zipper);

        //We're done.
        drop(a_head);
        drop(b_head);
        drop(a_zipper);
        drop(b_zipper);
        drop(map_head);

        // for (k, v) in map.iter() {
        //     println!("{} {v}", String::from_utf8_lossy(&k));
        // }
        assert_eq!(map.val_count(), 7);
        assert_eq!(map.get_val_at(b"a+value").unwrap(), &0);
        assert_eq!(map.get_val_at(b"a-children-0+value").unwrap(), &7);
        assert_eq!(map.get_val_at(b"b+value").unwrap(), &1);
        assert_eq!(map.get_val_at(b"b-children-0+metabolic").unwrap(), &6);
        assert_eq!(map.get_val_at(b"b-children-0+metadata").unwrap(), &3);
        assert_eq!(map.get_val_at(b"b-children-0+value").unwrap(), &4);
        assert_eq!(map.get_val_at(b"b-children-1+value").unwrap(), &5);
    }

    #[test]
    fn hierarchical_zipper_heads2() {
        let mut map = PathMap::<isize>::new();

        //Make a ZipperHead for the whole map, and two child zippers
        let map_head = map.zipper_head();
        let mut a_zipper = map_head.write_zipper_at_exclusive_path(b"a").unwrap();
        let mut b_zipper = map_head.write_zipper_at_exclusive_path(b"b").unwrap();

        //Make a separate ZipperHead on each WriteZipper
        let a_head = a_zipper.zipper_head();
        let b_head = b_zipper.zipper_head();

        //Make some WriteZippers on each head
        let mut a0_zipper = a_head.write_zipper_at_exclusive_path(b"0").unwrap();
        let mut a1_zipper = a_head.write_zipper_at_exclusive_path(b"1").unwrap();
        let mut b0_zipper = b_head.write_zipper_at_exclusive_path(b"0").unwrap();
        let mut b1_zipper = b_head.write_zipper_at_exclusive_path(b"1").unwrap();

        //Do some interleaved work with them
        a0_zipper.descend_to(b"+value");
        a0_zipper.set_val(0);
        a1_zipper.descend_to(b"+value");
        a1_zipper.set_val(1);
        b0_zipper.descend_to(b"+value");
        b0_zipper.set_val(2);
        b1_zipper.descend_to(b"+value");
        b1_zipper.set_val(3);

        //We're done
        drop(a0_zipper);
        drop(a1_zipper);
        drop(b0_zipper);
        drop(b1_zipper);
        drop(a_head);
        drop(b_head);
        drop(a_zipper);
        drop(b_zipper);
        drop(map_head);

        // for (k, v) in map.iter() {
        //     println!("{} {v}", String::from_utf8_lossy(&k));
        // }
        assert_eq!(map.val_count(), 4);
        assert_eq!(map.get_val_at(b"a0+value").unwrap(), &0);
        assert_eq!(map.get_val_at(b"a1+value").unwrap(), &1);
        assert_eq!(map.get_val_at(b"b0+value").unwrap(), &2);
        assert_eq!(map.get_val_at(b"b1+value").unwrap(), &3);
    }

    #[test]
    fn hierarchical_zipper_heads3() {
        let mut map = PathMap::<isize>::new();

        //Make a ZipperHead for the whole map, and then a zipper for a branch within the map
        let map_head = map.zipper_head();
        let mut top_zipper = map_head.write_zipper_at_exclusive_path(b"0").unwrap();
        top_zipper.descend_to(b":test:");

        //Make a sub-head at a path that doesn't exist yet
        let sub_head = top_zipper.zipper_head();
        let mut sub_zipper = sub_head.write_zipper_at_exclusive_path(b"5").unwrap();

        //Set the value at the zipper's root
        sub_zipper.set_val(5);

        //Set a value below the zipper's root
        sub_zipper.descend_to(b":next:1");
        sub_zipper.set_val(1);

        drop(sub_zipper);
        drop(sub_head);
        drop(top_zipper);
        drop(map_head);

        assert_eq!(map.get_val_at("0:test:5"), Some(&5));
        assert_eq!(map.get_val_at("0:test:5:next:1"), Some(&1));
    }
    /// Use a [ZipperHeadOwned] to write a bunch of paths into the map, single-threaded
    #[test]
    fn owned_zipper_head_test1() {
        let mut map = PathMap::<isize>::new();
        map.set_val_at(b"start:0000:hello", 0);
        map.set_val_at(b"start:0001:hello", 1);
        map.set_val_at(b"start:0002:hello", 2);
        map.set_val_at(b"start:0003:hello", 3);

        let zh = map.into_zipper_head(b"start:");

        let mut z0 = zh.write_zipper_at_exclusive_path(b"0000").unwrap();
        let mut z1 = zh.write_zipper_at_exclusive_path(b"0001").unwrap();
        let mut z2 = zh.write_zipper_at_exclusive_path(b"0002").unwrap();
        let mut z3 = zh.write_zipper_at_exclusive_path(b"0003").unwrap();

        z0.descend_to(b":goodbye");
        z0.set_val(0);
        z1.descend_to(b":goodbye");
        z1.set_val(1);
        z2.descend_to(b":goodbye");
        z2.set_val(2);
        z3.descend_to(b":goodbye");
        z3.set_val(3);

        drop(z0);
        drop(z1);
        drop(z2);
        drop(z3);

        let map = zh.into_map();
        assert_eq!(map.val_count(), 8);
        assert_eq!(map.get_val_at("start:0000:hello"), Some(&0));
        assert_eq!(map.get_val_at("start:0000:goodbye"), Some(&0));
        assert_eq!(map.get_val_at("start:0002:hello"), Some(&2));
        assert_eq!(map.get_val_at("start:0002:goodbye"), Some(&2));
    }
    /// Parallel version of `owned_zipper_head_test1`, but with a lot more elements, pounding on the
    /// ZipperHead from each thread
    #[test]
    fn parallel_owned_zipper_head_test2() {
        #[cfg(miri)]
        let elements = 32;
        #[cfg(all(feature = "pathmap-internal-tests", not(miri)))]
        let elements = 1024;

        let thread_cnt: usize = 8;
        let elements_per_thread = elements / thread_cnt;

        let mut map = PathMap::<u32>::new();
        for i in 0u32..(elements as u32) {
            let mut path = b"start:".to_vec();
            path.extend(i.to_be_bytes());
            path.extend(b":hello");
            map.set_val_at(path, i);
        }

        let zh = map.into_zipper_head(b"start:");
        thread::scope(|scope| {
            let mut threads: Vec<ScopedJoinHandle<()>> = Vec::with_capacity(thread_cnt);
            let zh_ref = &zh;

            //Spawn all the threads
            for thread_idx in 0..thread_cnt {
                let thread = scope.spawn(move || {
                    for i in 0..elements_per_thread {
                        let idx = (i * thread_cnt + thread_idx) as u32;
                        let mut z =
                            zh_ref.write_zipper_at_exclusive_path(idx.to_be_bytes()).unwrap();
                        z.descend_to(b":goodbye");
                        z.set_val(idx);
                    }
                });
                threads.push(thread);
            }

            //Wait for the threads to finish
            for thread in threads {
                thread.join().unwrap();
            }
        });
        let map = zh.into_map();

        assert_eq!(map.val_count(), elements * 2);
        for i in 0u32..(elements as u32) {
            let mut path_base = b"start:".to_vec();
            path_base.extend(i.to_be_bytes());
            let mut hello_path = path_base.clone();
            hello_path.extend(b":hello");
            let mut goodbye_path = path_base.clone();
            goodbye_path.extend(b":goodbye");
            assert_eq!(map.get_val_at(hello_path), Some(&i));
            assert_eq!(map.get_val_at(goodbye_path), Some(&i));
        }
    }
    /// Tests the [ZipperHead::cleanup_write_zipper] method
    #[test]
    fn cleanup_write_zipper_test1() {
        let mut map = PathMap::<()>::new();
        map.set_val_at("the_path_to_somewhere", ());

        //First ensure we *do* have a dangling path from these operations
        let zh = map.zipper_head();
        let wz = zh.write_zipper_at_exclusive_path("a_path_to_nowhere").unwrap();
        drop(wz);
        drop(zh);
        assert!(map.read_zipper().child_mask().test_bit(b'a'));
        assert!(map.read_zipper().child_mask().test_bit(b't'));

        //Now do it again, and clean up the zipper
        let zh = map.zipper_head();
        let wz = zh.write_zipper_at_exclusive_path("a_path_to_nowhere").unwrap();
        zh.cleanup_write_zipper(wz);
        drop(zh);
        assert!(!map.read_zipper().child_mask().test_bit(b'a'));
        assert!(map.read_zipper().child_mask().test_bit(b't'));
    }
    /// Tests the [ZipperHead::cleanup_write_zipper] method
    #[test]
    fn cleanup_write_zipper_test2() {
        let mut map = PathMap::<()>::new();
        map.set_val_at("a_path_to_somewhere", ());
        let zh = map.zipper_head();

        //Make a ZipperHead, which will create a dangling path, but then clean it up
        let wz = zh.write_zipper_at_exclusive_path("a_path_to_nowhere").unwrap();
        zh.cleanup_write_zipper(wz);

        //Make sure we cleaned up the dangling path, but nothing else
        let mut rz = zh.read_zipper_at_borrowed_path(b"a_path_").unwrap();
        assert!(rz.descend_until());
        assert_eq!(rz.path(), b"to_somewhere");
        drop(rz);

        //Now make sure we don't accidentally clean up a path with the wz on top of an existing path
        let wz = zh.write_zipper_at_exclusive_path("a_path_to_").unwrap();
        zh.cleanup_write_zipper(wz);
        let mut rz = zh.read_zipper_at_borrowed_path(b"a_path_").unwrap();
        assert_eq!(rz.path(), b"");
        assert!(rz.descend_until());
        assert_eq!(rz.path(), b"to_somewhere");
        drop(rz);

        drop(zh);
    }

    #[test]
    fn cleanup_write_zipper_test3() {
        let mut btm: PathMap<()> = PathMap::new();
        btm.insert([2, 197, 115, 116, 97, 116, 101, 197, 114, 101, 97, 100, 121], ());
        btm.insert(
            [
                4, 196, 101, 120, 101, 99, 193, 50, 2, 193, 44, 2, 199, 116, 114, 105, 103, 103,
                101, 114, 193, 120, 2, 193, 79, 2, 193, 43, 2, 195, 97, 100, 100, 193, 120,
            ],
            (),
        );
        let zh = btm.zipper_head();

        //Make a single value, at the root of a shared path
        let mut wz = zh
            .write_zipper_at_exclusive_path(&[2, 199, 116, 114, 105, 103, 103, 101, 114, 193, 120])
            .unwrap();
        wz.set_val(());
        zh.cleanup_write_zipper(wz);

        //Validate that the value is where we think it is
        let rz = zh
            .read_zipper_at_borrowed_path(&[2, 199, 116, 114, 105, 103, 103, 101, 114, 193, 120])
            .unwrap();
        assert_eq!(rz.path_exists(), true);
        assert_eq!(rz.is_val(), true);
        assert_eq!(rz.child_count(), 0);
        assert_eq!(rz.child_mask(), ByteMask::EMPTY);
        drop(rz);

        //Now clean up the value
        let mut wz = zh
            .write_zipper_at_exclusive_path(&[2, 199, 116, 114, 105, 103, 103, 101, 114, 193, 120])
            .unwrap();
        wz.remove_val(true);
        zh.cleanup_write_zipper(wz);

        //Validate that the value is gone
        let rz = zh
            .read_zipper_at_borrowed_path(&[2, 199, 116, 114, 105, 103, 103, 101, 114, 193, 120])
            .unwrap();
        assert_eq!(rz.path_exists(), false);
        assert_eq!(rz.is_val(), false);
        drop(rz);
    }
}
