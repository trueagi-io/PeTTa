use std::marker::PhantomData;
use std::num::NonZero;
use std::num::NonZeroU32;
use std::sync::Arc;
use std::sync::RwLock;

use super::PathMap;
use super::zipper::{
    ReadZipperUntracked, Zipper, ZipperAbsolutePath, ZipperForking, ZipperIteration, ZipperMoving,
    ZipperReadOnlyIteration, ZipperReadOnlyValues, ZipperWriting,
};

/// Marker to track an outstanding read zipper
pub struct TrackingRead;

/// Marker to track an outstanding write zipper
pub struct TrackingWrite;

/// Private mod to seal public trait
mod tracking_internal {
    use super::*;

    pub trait TrackingMode {
        fn as_str() -> &'static str;
        fn tracks_writes() -> bool;
        fn tracks_reads() -> bool {
            !Self::tracks_writes()
        }
    }
    impl TrackingMode for TrackingRead {
        fn as_str() -> &'static str {
            "read"
        }
        fn tracks_writes() -> bool {
            false
        }
    }
    impl TrackingMode for TrackingWrite {
        fn as_str() -> &'static str {
            "write"
        }
        fn tracks_writes() -> bool {
            true
        }
    }
}
use tracking_internal::TrackingMode;

/// Object that accompanies a zipper and tracks its path, to check for violations against all other
/// outstanding zipper paths.  See [ZipperCreation::write_zipper_at_exclusive_path](super::zipper::ZipperCreation::write_zipper_at_exclusive_path).
pub struct ZipperTracker<M: TrackingMode> {
    all_paths: SharedTrackerPaths,
    this_path: Vec<u8>,
    _is_tracking: PhantomData<M>,
}

impl Clone for ZipperTracker<TrackingRead> {
    fn clone(&self) -> Self {
        self.all_paths.add_reader_unchecked(self.this_path.as_slice());
        Self {
            all_paths: self.all_paths.clone(),
            this_path: self.this_path.clone(),
            _is_tracking: PhantomData,
        }
    }
}

#[cfg(feature = "zipper_tracking")]
impl<M: TrackingMode> ZipperTracker<M> {
    /// Returns the path that the tracker is guarding
    pub fn path(&self) -> &[u8] {
        &self.this_path
    }
}

/// An error type that may be produced when attempting to create a zipper or otherwise acquire
/// permission to access a path
#[derive(Debug)]
pub struct Conflict {
    with: IsTracking,
    at: Vec<u8>,
}

impl std::fmt::Display for Conflict {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let _ = write!(f, "conflicts with existing zipper: ");
        let _ = match self.with {
            IsTracking::WriteZipper => write!(f, "write zipper"),
            IsTracking::ReadZipper(cnt) => write!(f, "read zipper (arity: {cnt:?})"),
        };
        let path = &self.at;
        writeln!(f, " @ {path:?}")
    }
}

impl std::error::Error for Conflict {}

impl Conflict {
    fn write_conflict(path: &[u8]) -> Conflict {
        Conflict { with: IsTracking::WriteZipper, at: path.to_vec() }
    }

    fn read_conflict(cnt: NonZeroU32, path: &[u8]) -> Conflict {
        Conflict { with: IsTracking::ReadZipper(cnt), at: path.to_vec() }
    }

    fn check_for_lock_along_path<'a, A: Clone + Send + Sync + Unpin>(
        path: &[u8],
        zipper: &'a mut ReadZipperUntracked<A>,
    ) -> Option<&'a A> {
        let mut current_path = path;
        loop {
            if zipper.is_val() {
                return zipper.get_val();
            } else if current_path.is_empty() {
                return None;
            } else {
                let steps = zipper.descend_to_val(current_path);
                if steps == 0 {
                    return None;
                }
                current_path = &current_path[steps..];
            }
        }
    }

    fn check_for_write_conflict<C, ConflictF: FnOnce(&[u8]) -> C>(
        path: &[u8],
        all_paths: &PathMap<()>,
        conflict_f: ConflictF,
    ) -> Result<(), C> {
        let mut zipper = all_paths.read_zipper();
        match Conflict::check_for_lock_along_path(path, &mut zipper) {
            None =>
            /* at this point zipper is either focued on the given path (when it exists)
            , or the procedure broke out early, because it was determined that the path does not exist */
            {
                if zipper.path().len() == path.len() {
                    let mut subtree = zipper.fork_read_zipper();
                    match subtree.to_next_val() {
                        false => Ok(()),
                        true => Err(conflict_f(subtree.origin_path())),
                    }
                } else {
                    Ok(())
                }
            }
            Some(_) => Err(conflict_f(zipper.path())),
        }
    }

    fn check_for_read_conflict<C, ConflictF: FnOnce(NonZeroU32, &[u8]) -> C>(
        path: &[u8],
        all_paths: &PathMap<NonZeroU32>,
        conflict_f: ConflictF,
    ) -> Result<(), C> {
        let mut zipper = all_paths.read_zipper();
        match Conflict::check_for_lock_along_path(path, &mut zipper) {
            None => {
                if zipper.path().len() == path.len() {
                    let mut subtree = zipper.fork_read_zipper();
                    match subtree.to_next_get_val() {
                        None => Ok(()),
                        Some(lock) => Err(conflict_f(*lock, subtree.origin_path())),
                    }
                } else {
                    Ok(())
                }
            }
            Some(lock) => Err(conflict_f(*lock, zipper.path())),
        }
    }

    pub fn path(&self) -> &[u8] {
        &self.at[..]
    }
}

/// A shared registry of outstanding zippers
///
/// Use [SharedTrackerPaths::default] to make a new registry
//
// NOTE for the future: We were considering using a lockless queue in place of the `RwLock` to guard
// this object, with the understanding that all new zipper creation would be serialized.  The idea is
// that the zipper `Drop` implementations would enquque paths, and then the queue would be drained prior
// to creating any new zippers.  So a queue with lockless enqueue means there is no critical section
// even if the zippers move to different threads.  This is still a valid approach.  If, however,
// `SharedTrackerPaths` is changed to be `!Sync` by replacing the `RwLock`, we should move the
// `SharedTrackerPaths` inside the `Mutex` inside [ZipperHeadOwned](super::zipper::ZipperHeadOwned) so
// `ZipperHeadOwned` remains `Sync`
#[derive(Clone, Default)]
pub struct SharedTrackerPaths(Arc<RwLock<TrackerPaths>>);

#[derive(Clone, Default)]
struct TrackerPaths {
    read_paths: PathMap<NonZeroU32>,
    written_paths: PathMap<()>,
}

/// Represents the status of a specific path, returned by [SharedTrackerPaths::path_status]
#[cfg(feature = "zipper_tracking")]
pub enum PathStatus {
    /// The path is available for both reading and writing
    Available,
    /// Only read zippers may be created at the path
    AvailableForReading,
    /// No zippers may be created at the path
    Unavailable,
}

impl SharedTrackerPaths {
    fn with_paths<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&mut TrackerPaths) -> R,
    {
        let mut guard = self.0.write().unwrap();
        let r = f(&mut guard);
        drop(guard);
        r
    }

    /// Returns the status of a specific path, which corresponds to whether a a request for
    /// a zipper at precisely the same instant would have succeeded or failed
    ///
    /// As with any asynchronous operation, it is not reliable to assume the outcome of a
    /// future operation based on the return value of this method.
    #[cfg(feature = "zipper_tracking")]
    pub fn path_status<P: AsRef<[u8]>>(&self, path: P) -> PathStatus {
        let path = path.as_ref();
        self.with_paths(|all_paths: &mut TrackerPaths| {
            match Conflict::check_for_write_conflict(path, &all_paths.written_paths, |_| ()) {
                Ok(()) => {}
                Err(()) => return PathStatus::Unavailable,
            }
            match Conflict::check_for_read_conflict(path, &all_paths.read_paths, |_, _| ()) {
                Ok(()) => {}
                Err(()) => return PathStatus::AvailableForReading,
            }
            PathStatus::Available
        })
    }

    fn try_add_writer(&self, path: &[u8]) -> Result<(), Conflict> {
        let try_add_writer_internal = |all_paths: &mut TrackerPaths| {
            Conflict::check_for_write_conflict(
                path,
                &all_paths.written_paths,
                Conflict::write_conflict,
            )?;
            Conflict::check_for_read_conflict(
                path,
                &all_paths.read_paths,
                Conflict::read_conflict,
            )?;
            let mut writer = all_paths.written_paths.write_zipper_at_path(path);
            writer.set_val(());
            Ok(())
        };

        self.with_paths(try_add_writer_internal)
    }

    fn try_add_reader(&self, path: &[u8]) -> Result<(), Conflict> {
        let try_add_reader_internal = |all_paths: &mut TrackerPaths| {
            Conflict::check_for_write_conflict(
                path,
                &all_paths.written_paths,
                Conflict::write_conflict,
            )?;
            let mut writer = all_paths.read_paths.write_zipper_at_path(path);
            let value = writer.get_val_mut();
            match value {
                Some(cnt) => match cnt.checked_add(1) {
                    Some(new_cnt) => {
                        *cnt = new_cnt;
                        Ok(())
                    }
                    None => Err(Conflict::read_conflict(NonZero::<u32>::MAX, path)),
                },
                None => {
                    writer.set_val(NonZero::<u32>::MIN);
                    Ok(())
                }
            }
        };

        self.with_paths(try_add_reader_internal)
    }

    /// Adds a new reader without checking to see whether it conflicts with existing writers
    fn add_reader_unchecked(&self, path: &[u8]) {
        let add_reader = |paths: &mut TrackerPaths| {
            let mut writer = paths.read_paths.write_zipper_at_path(path);
            match writer.get_val_mut() {
                Some(cnt) => {
                    *cnt = unsafe { NonZero::new_unchecked(cnt.get() + 1) };
                }
                None => {
                    writer.set_val(unsafe { NonZero::new_unchecked(1) });
                }
            }
        };

        self.with_paths(add_reader)
    }
}

#[derive(Clone, Debug, PartialEq)]
enum IsTracking {
    WriteZipper,
    ReadZipper(NonZeroU32),
}

impl<M: TrackingMode> core::fmt::Debug for ZipperTracker<M> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let all_paths = self.all_paths.0.read().unwrap();
        let _ =
            writeln!(f, "ZipperTracker {{ type = {:?}, path = {:?}", M::as_str(), self.this_path);
        let _ = writeln!(f, "\tRead Zippers:");
        for (rz, cnt) in all_paths.read_paths.iter() {
            let _ = writeln!(f, "\t\t{rz:?} ({cnt:?})");
        }
        let _ = writeln!(f, "\tWrite Zippers:");
        for (wz, _) in all_paths.written_paths.iter() {
            let _ = writeln!(f, "\t\t{wz:?}");
        }
        write!(f, "}}")
    }
}

impl<M: TrackingMode> ZipperTracker<M> {
    /// Destroy a `ZipperTracker` without invoking Drop code to release the lock
    fn dismantle(self) -> (SharedTrackerPaths, Vec<u8>) {
        let tracker_shell = core::mem::ManuallyDrop::new(self);
        let all_paths = unsafe { core::ptr::read(&tracker_shell.all_paths) };
        let this_path = unsafe { core::ptr::read(&tracker_shell.this_path) };
        (all_paths, this_path)
    }
}

impl ZipperTracker<TrackingRead> {
    /// Create a new `ZipperTracker` to track a read zipper
    pub fn new(shared_paths: SharedTrackerPaths, path: &[u8]) -> Result<Self, Conflict> {
        shared_paths.try_add_reader(path)?;
        Ok(Self { all_paths: shared_paths, this_path: path.to_vec(), _is_tracking: PhantomData })
    }
}

impl ZipperTracker<TrackingWrite> {
    /// Create a new `ZipperTracker` to track a write zipper
    pub fn new(shared_paths: SharedTrackerPaths, path: &[u8]) -> Result<Self, Conflict> {
        shared_paths.try_add_writer(path)?;
        Ok(Self { all_paths: shared_paths, this_path: path.to_vec(), _is_tracking: PhantomData })
    }
    /// Consumes the writer tracker, and returns a new reader tracker with the same path
    pub fn into_reader(self) -> ZipperTracker<TrackingRead> {
        let (all_paths, this_path) = self.dismantle();
        //We add the reader lock first before removing the writer, so there is no chance another thread will
        // grab a writer in between.
        all_paths.add_reader_unchecked(&this_path);
        Self::remove_lock(&all_paths, &this_path);
        ZipperTracker::<TrackingRead> { all_paths, this_path, _is_tracking: PhantomData }
    }
}

impl<M: TrackingMode> ZipperTracker<M> {
    /// Internal method to remove a lock, called after it has been confirmed to be the correct thing to do
    fn remove_lock(all_paths: &SharedTrackerPaths, this_path: &[u8]) {
        let is_removed = all_paths.with_paths(|paths| {
            if M::tracks_reads() {
                let mut write_zipper = paths.read_paths.write_zipper_at_path(this_path);
                match write_zipper.get_val_mut() {
                    Some(cnt) => {
                        if *cnt == NonZero::<u32>::MIN {
                            write_zipper.remove_val(true);
                        } else {
                            *cnt = unsafe { NonZero::new_unchecked(cnt.get() - 1) };
                        };
                        true
                    }
                    None => false,
                }
            } else {
                let removed = paths.written_paths.write_zipper_at_path(this_path).remove_val(true);
                removed.is_some()
            }
        });
        if !is_removed {
            panic!("Lock is missing.\nContents {this_path:#?}");
        }
    }
}

impl<M: TrackingMode> Drop for ZipperTracker<M> {
    fn drop(&mut self) {
        Self::remove_lock(&self.all_paths, &self.this_path);
    }
}
