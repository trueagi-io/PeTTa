//! Compact Arena Trie Representation
//!
//! This format represents tries (or prefix trees) in a compact, efficient manner.
//!
//! ## Variable-Length Encoding: `varint64`
//!
//! The format uses variable-length encoding (VLE) for 64-bit integers. In this scheme:
//! - Numbers are encoded using 7 bits per byte.
//! - The most significant bit (MSB) indicates if more bytes follow (1) or not (0).
//! - Up to 9 bytes are used, preserving the last MSB in the resulting `u64`.
//! - This allows representation of the full range of `u64` values.
//!
//! See: [read_varint_u64] and [push_varint_u64] for the implementation and examples.
//!
//! # File Format Description
//!
//! ### File Header
//! The file begins with an 8-byte magic signature ([COMPACT_TREE_MAGIC]`=ACTree01`).
//! This signature helps quickly reject files not intended as tries.
//!
//! ### Root Offset
//! At offset `8`, a `u64` in little-endian byte order specifies the root node's offset.
//! This value can be updated to create a modified trie by setting a new root.
//!
//! ### Arena Layout
//! Starting at offset `16`, the arena stores dynamically sized objects (nodes and line data).
//! Each object is uniquely addressable by its starting offset.
//!
//! **Key Feature:** Siblings of any parent are encoded sequentially. This design:
//! - Allows storing only the offset to the first child.
//! - All children can be accessed by reading the nodes sequentially.
//!
//! ### Object Types in the Arena
//! The arena contains three object types:
//!
//! - **Line Data:**  
//!   - Format: `[length: varint64][data: u8; length]`  
//!   - A byte slice (`&[u8]`) with a variable-length size.
//!
//! - **Branch Node:** (Top bit of the first byte = 0)  
//!   - Byte 1: `[header: u8]`  
//!     - Bit #6: Has value? (1 = yes, 0 = no)  
//!     - Bits #0–5: Number of children (max 32).  
//!   - If value exists: `[node_value: varint64]`  
//!   - If children  > 0: `[first_child: varint64]`  
//!   - If children < 32: `[child_bytes: u8; num_children]` (ascending order)  
//!   - If children >=32: `[child_mask: u64; 4]`            (little-endian).
//!
//! - **Line Node:** (Top bit of the first byte = 1)  
//!   - Byte 1: `[header: u8]`  
//!     - Bit #6: Has value? (1 = yes, 0 = no)  
//!     - Bits #0–5: Number of children (max 1).  
//!   - If value exists: `[node_value: varint64]`  
//!   - If child exists: `[first_child: varint64]`  
//!   - Always: `[line_offset: varint64]` (points to line data).
//!
//! ## Diagram of File Format
//!
//! ```ignoretext
//! File format: [MAGIC=ACTree01][root_id: u64][arena of nodes]...
//! Where [arena of nodes] stores dynamically sized nodes and line data,
//! written densely one after another.
//!
//! Object types:
//! line data:   [length: varint64][u8; length]
//!    function: 
//!
//! branch node: [header: u8] (header & 0x80 == 0)
//!              [if (header&0x40 != 0) node_value : varint64         ]
//!              [if (header&0x3f != 0) first_child: varint64         ]
//!              [if (header&0x3f < 32) child_bytes: [u8; header&0x3f]]
//!              [if (header&0x3f >=32) child_mask : [u64; 4]         ]
//!
//! line node:   [header: u8] (header & 0x80 != 0)
//!              [if (header&0x40 != 0) node_value : varint64]
//!              [if (header&0x3f != 0) first_child: varint64]
//!              [                      line_offset: varint64]
//! ```ignore
use std::{io::Write, hash::Hasher};
use std::cell::Cell;
use std::marker::PhantomData;
use fast_slice_utils::starts_with;

use super::alloc::{GlobalAlloc, global_alloc};
use super::{
    PathMap,
    morphisms::Catamorphism,
    utils::{BitMask, ByteMask, find_prefix_overlap},
    zipper::{
        Zipper, ZipperValues, ZipperForking, ZipperAbsolutePath, ZipperIteration,
        ZipperMoving, ZipperPathBuffer, ZipperReadOnlyValues, ZipperSubtries,
        ZipperConcrete, ZipperReadOnlyConditionalValues
    },
};

use super::trie_core::r#ref::{TrieRef, TrieRefBorrowed, TrieRefOwned};
use super::trie_core::node::PayloadRef;

use super::gxhash::{GxHasher, HashMap, HashMapExt};

/// The identifier of a node (branch node or line node)
///
/// NOTE: this identifier can be (wrongly) reused between different tries,
/// which can catastrophically break the implementation.
///
/// However, in order to fix this issue, we would have to either introduce
/// - a runtime cost (include `Trie ID` or `&Trie` into the `NodeId`),
/// - ... or introduce a *very* inconvenient API, using invariant lifetimes.
///
/// This tradeoff is in favor of interface simplicity and lower runtime cost.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NodeId(u64);

/// The identifier of line data (essentially, `&[u8]`)
///
/// See documentation of [NodeId] for the note about safety
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LineId(u64);
const INVALID_LINE: LineId = LineId(!0);

/// Maximum node size:
/// 1 byte header + 9 byte first child + 9 byte value + 32 child mask
const MAX_BRANCH_NODE_SIZE: usize = 1 + 9 + 9 + 32;
/// Maximum node size:
/// 1 byte header + 9 byte first child + 9 byte value + 9 byte path id
const MAX_LINE_NODE_SIZE: usize = 1 + 9 + 9 + 9;

/// Top bit indicates that the node is a line node
const LINE_FLAG: u8 = 0x80;
/// Bit #6 indicates that this node contains a value
const VALUE_FLAG: u8 = 0x40;

/// Maximum number of bytes in `varint64`
const MAX_VARINT_SIZE: usize = 9;

const U64_SIZE: usize = core::mem::size_of::<u64>();

/// File magic signature
pub const MAGIC_LENGTH: usize = 8;
// Changes:
// ACTree01 -> ACTree02: Relative offsets
// ACTree02 -> ACTree03: Branchless varint
pub const COMPACT_TREE_MAGIC: [u8; MAGIC_LENGTH] = *b"ACTree03";

const VARINT_LEN_BIAS: u8 = u8::MAX - 8;
/// Decodes a variable-length encoded `u64` integer from a byte slice.
///
/// If the first byte is up to `VARINT_LEN_BIAS` (247), it represents the value directly.
/// Otherwise, the first byte (`VARINT_LEN_BIAS + nbytes`) indicates the number of following
/// bytes (`nbytes`) that contain the integer in little-endian order.
///
/// # Arguments
/// * `data` - A byte slice containing the encoded varint.
///
/// # Returns
/// A tuple containing:
/// * The decoded `u64` value.
/// * The number of bytes consumed from the input slice.
///
/// # Examples
/// ```ignore
/// use petta::pathmap::arena_compact::read_varint_u64;
///
/// // Single byte encoding (100)
/// let data = [100];
/// let (value, len) = read_varint_u64(&data);
/// assert_eq!(value, 100);
/// assert_eq!(len, 1);
///
/// // Multi-byte encoding (1000)
/// let data = [249, 232, 3];
/// let (value, len) = read_varint_u64(&data);
/// assert_eq!(value, 1000);
/// assert_eq!(len, 3);
///
/// // Maximum u64 value
/// let data = [255, 255, 255, 255, 255, 255, 255, 255, 255];
/// let (value, len) = read_varint_u64(&data);
/// assert_eq!(value, u64::MAX);
/// assert_eq!(len, 9);
/// ```ignore
pub fn read_varint_u64(data: &[u8]) -> (u64, usize) {
    let first = data[0];
    if first <= VARINT_LEN_BIAS {
        return (first as u64, 1);
    }
    let len = (first - VARINT_LEN_BIAS) as usize;
    let rest = unsafe {
        data.as_ptr().add(1)
            .cast::<u64>().read_unaligned()
    };
    let zeros = (64 - len * 8) as u32;
    let value = (rest << zeros) >> zeros;
    (value, len + 1)
}

/// Encodes a `u64` integer into a variable-length format and writes it to a `Writer`.
///
/// The encoding uses a single byte for values up to `VARINT_LEN_BIAS` (247). For larger values,
/// it writes a header byte (`VARINT_LEN_BIAS + nbytes`) followed by the `nbytes` least significant
/// bytes of the integer in little-endian order. The maximum encoding size is 9 bytes.
///
/// # Arguments
/// * `dst` - A mutable reference to a type implementing `Write`, such as `Vec<u8>` or `BufWriter`.
/// * `int` - The unsigned 64-bit integer to encode.
///
/// # Examples
/// ```ignore
/// use std::io::Write;
/// use petta::pathmap::arena_compact::push_varint_u64;
///
/// // Single byte encoding for small value (100)
/// let mut buf = Vec::new();
/// push_varint_u64(&mut buf, 100).unwrap();
/// assert_eq!(buf, [100]);
///
/// // Multi-byte encoding for larger value (1000)
/// let mut buf = Vec::new();
/// push_varint_u64(&mut buf, 1000).unwrap();
/// assert_eq!(buf, [249, 232, 3]);
///
/// // Maximum u64 value (2^64 - 1)
/// let mut buf = Vec::new();
/// push_varint_u64(&mut buf, u64::MAX).unwrap();
/// assert_eq!(buf, [255, 255, 255, 255, 255, 255, 255, 255, 255]);
/// ```ignore
pub fn push_varint_u64(dst: &mut impl Write, int: u64)
    -> Result<usize, std::io::Error>
{
    if int <= VARINT_LEN_BIAS as u64 {
        dst.write_all(&[int as u8])?;
        return Ok(1)
    }
    let nbytes = (8 - int.leading_zeros() / 8) as usize;
    let arr = int.to_le_bytes();
    dst.write_all(&[VARINT_LEN_BIAS + nbytes as u8])?;
    dst.write_all(&arr[..nbytes])?;
    Ok(nbytes + 1)
}

/*
// older varints
/// Read `u64` in variable-length encoding (VLE) from a slice.
///
/// This function implements varint decoding, where numbers are encoded using
/// 7 bits per byte, with the most significant bit (MSB) indicating whether
/// more bytes follow (1) or not (0). It can read up to 9 bytes to represent
/// a full 64-bit value.
///
/// # Returns
/// A tuple containing:
/// * `u64` - The decoded unsigned 64-bit integer.
/// * `usize` - The number of bytes consumed from the input slice.
///
/// # Examples
///
/// ```ignore
/// use petta::pathmap::arena_compact::read_varint_u64;
///
/// // Single byte encoding (127)
/// let bytes = [0x7F];
/// let (value, bytes_read) = read_varint_u64(&bytes);
/// assert_eq!(value, 127);
/// assert_eq!(bytes_read, 1);
///
/// // Two byte encoding (130) - shows little-endian style shift
/// let bytes = [0x82, 0x01];
/// let (value, bytes_read) = read_varint_u64(&bytes);
/// assert_eq!(value, 130);
/// assert_eq!(bytes_read, 2);
///
/// // Maximum u64 value (2^64 - 1) using 9 bytes
/// let bytes = [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF];
/// let (value, bytes_read) = read_varint_u64(&bytes);
/// assert_eq!(value, u64::MAX);
/// assert_eq!(bytes_read, 9);
/// ```ignore
///
/// Panics when the VLE encoding is longer than the slice.
/// Will never panic if slice length is at least 9.
pub fn read_varint_u64(data: &[u8]) -> (u64, usize) {
    let mut value = 0;
    let mut shift = 0;
    let mut bread = 0;
    for ii in 0..8 {
        let byte = data[ii];
        value = value | (((byte & 0x7f) as u64) << shift);
        bread += 1;
        if (byte >> 7) == 0 {
            return (value, bread);
        }
        shift += 7;
    }
    // Read last byte without clearing the top bit
    let byte = data[8];
    value = value | ((byte as u64) << shift);
    bread += 1;
    (value, bread)
}

/// Writes a variable-length unsigned 64-bit integer to a Writer.
///
/// This function encodes a `u64` value into a varint format, using 7 bits
/// per byte, with the most significant bit (MSB) set to 1 if more bytes follow
/// and 0 if it's the last byte. Uses up to 9 bytes for encoding.
///
/// # Arguments
/// * `dst` - Reference to Writer e.g. `Vec<u8>`, `File`, `BufWriter`.
/// * `int` - The unsigned 64-bit integer to encode.
///
/// # Examples
/// ```ignore
/// use std::io::Write;
/// use petta::pathmap::arena_compact::push_varint_u64;
///
/// // Single byte encoding (127)
/// let mut buf = Vec::new();
/// push_varint_u64(&mut buf, 127);
/// assert_eq!(buf, [0x7F]);
///
/// // Two byte encoding (130) - shows little-endian style shift
/// let mut buf = Vec::new();
/// push_varint_u64(&mut buf, 130);
/// assert_eq!(buf, [0x82, 0x01]);
///
/// // Maximum u64 value (2^64 - 1) using 8 bytes
/// let mut buf = Vec::new();
/// push_varint_u64(&mut buf, u64::MAX);
/// assert_eq!(buf, [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]);
/// ```ignore
pub fn push_varint_u64(dst: &mut impl Write, mut int: u64)
    -> Result<usize, std::io::Error>
{
    let mut buf = [0_u8; MAX_VARINT_SIZE];
    for ii in 0..MAX_VARINT_SIZE - 1 {
        let mut byte = (int & 0x7f) as u8;
        int = int >> 7;
        byte = byte | ((int > 0) as u8) << 7;
        buf[ii] = byte;
        if int == 0 {
            dst.write_all(&buf[..ii + 1])?;
            return Ok(ii + 1);
        }
    }
    // write the last byte;
    buf[MAX_VARINT_SIZE - 1] = int as u8;
    dst.write_all(&buf)?;
    Ok(MAX_VARINT_SIZE)
}
*/

/// Read a node from the start of a given slice
///
/// # Usage
/// ```ignoreignore
/// use petta::pathmap::arena_compact::read_node;
/// use petta::pathmap::arena_compact::Node;
/// let (node, length) = read_node(&[0x00]);
/// assert!(matches!(node, Node::Branch(_)));
/// assert_eq!(node.child_count(), 0);
/// assert_eq!(length, 1);
/// ```ignore
fn read_node(data: &[u8], node_id: NodeId) -> (Node, usize) {
    let head = data[0];
    let mut pos = 1;
    if head & LINE_FLAG == 0 {
        let mut node = NodeBranch::empty();
        let has_value = (head & VALUE_FLAG) != 0;
        node.value = if has_value {
            let (value, off) = read_varint_u64(&data[pos..]);
            pos += off;
            Some(value)
        } else {
            None
        };
        let nchildren = (head & 0x3f) as usize;
        assert!(nchildren <= 32, "invalid children count");
        if nchildren > 0 {
            let (first_child, off) = read_varint_u64(&data[pos..]);
            pos += off;
            node.first_child = Some(NodeId(node_id.0 - first_child));
        }
        let children_bytes = &data[pos..pos + nchildren];
        pos += nchildren;
        node.bytemask = if nchildren == 32 {
            #[cfg(not(target_endian = "little"))]
            compile_error!("big endian not supported");
            let ptr = children_bytes.as_ptr().cast::<[u64; 4]>();
            // Safety: we're not reading past the end,
            // since children_bytes is exact size
            ByteMask::from(unsafe { ptr.read_unaligned() })
        } else {
            ByteMask::from_iter(children_bytes.iter().copied())
        };
        (Node::Branch(node), pos)
    } else {
        let mut line = NodeLine::empty();
        let has_value = (head & VALUE_FLAG) != 0;
        if has_value {
            let (value, off) = read_varint_u64(&data[pos..]);
            pos += off;
            line.value = Some(value);
        }
        let has_child = (head & 0x1) != 0;
        if has_child {
            let (child, off) = read_varint_u64(&data[pos..]);
            pos += off;
            line.child = Some(NodeId(node_id.0 - child));
        }
        let (line_id, off) = read_varint_u64(&data[pos..]);
        pos += off;
        line.path = LineId(node_id.0 - line_id);
        (Node::Line(line), pos)
    }
}

const USE_COUNTERS: bool = cfg!(feature="act_counters");

#[derive(Default, Clone)]
pub struct Counters {
    nodes: usize,
    nodes_size: usize,
    children: usize,
    child_mask_size: usize,
    lines: usize,
    lines_size: usize,
    values: usize,
    values_size: usize,
    offsets: usize,
    offsets_size: usize,
    line_data: usize,
    line_data_size: usize,
    line_data_reuse: usize,
    line_data_reuse_size: usize,
}

const SI_PREFIX: &[u8] = b"KMGTPE";

struct SiCount(usize);

impl std::fmt::Display for SiCount {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut value = self.0 as f64;
        if value < 1000.0 {
            return write!(fmt, "{value:3.0}");
        }
        let mut idx = 0;
        while value > 995.0 && idx < SI_PREFIX.len() {
            idx += 1;
            value = value / 1000.0;
        }
        write!(fmt, "{value:3.2}{}", SI_PREFIX[idx - 1] as char)
    }
}

impl std::fmt::Debug for Counters {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        let total_size = self.nodes_size + self.lines_size
            + self.line_data_size + 16 + 8;
        write!(fmt,
"Total file size: {total}B
Offsets: {offsets_size}B / {offsets} ({offsets_avg:1.3})
Contents:
    Line data: {line_data_size}B / {line_data} ({line_data_avg:1.3}) (saved by reuse={reuse})
    Line nodes: {lines_size}B / {lines} ({lines_avg:1.3})
    Branch nodes: {nodes_size}B / {nodes} ({nodes_avg:1.3})
        Children: average={children_avg:1.3}, mask size={mask_avg:1.3}",
            total=SiCount(total_size),
            offsets_size=SiCount(self.offsets_size),
            offsets=SiCount(self.offsets),
            offsets_avg=self.offsets_size as f64 / self.offsets as f64,
            line_data_size=SiCount(self.line_data_size),
            line_data=SiCount(self.line_data),
            line_data_avg=self.line_data_size as f64 / self.line_data as f64,
            reuse=SiCount(self.line_data_reuse_size),
            lines_size=SiCount(self.lines_size),
            lines=SiCount(self.lines),
            lines_avg=self.lines_size as f64 / self.lines as f64,
            nodes_size=SiCount(self.nodes_size),
            nodes=SiCount(self.nodes),
            nodes_avg=self.nodes_size as f64 / self.nodes as f64,
            children_avg=self.children as f64 / self.nodes as f64,
            mask_avg=self.child_mask_size as f64 / self.nodes as f64,
        )
    }
}

impl Counters {
    #[inline(always)]
    fn add_line(&mut self, size: usize) {
        if !USE_COUNTERS { return; }
        self.lines += 1;
        self.lines_size += size;
    }
    #[inline(always)]
    fn add_line_data(&mut self, size: usize) {
        if !USE_COUNTERS { return; }
        self.line_data += 1;
        self.line_data_size += size;
    }
    #[inline(always)]
    fn add_line_data_reuse(&mut self, size: usize) {
        if !USE_COUNTERS { return; }
        self.line_data_reuse += 1;
        self.line_data_reuse_size += size;
    }
    #[inline(always)]
    fn add_node(&mut self, size: usize) {
        if !USE_COUNTERS { return; }
        self.nodes += 1;
        self.nodes_size += size;
    }
    #[inline(always)]
    fn add_offset(&mut self, size: usize) {
        if !USE_COUNTERS { return; }
        self.offsets += 1;
        self.offsets_size += size;
    }
    #[inline(always)]
    fn add_value(&mut self, size: usize) {
        if !USE_COUNTERS { return; }
        self.values += 1;
        self.values_size += size;
    }
    #[inline(always)]
    fn add_children(&mut self, children: usize, size: usize) {
        if !USE_COUNTERS { return; }
        self.children += children;
        self.child_mask_size += size;
    }
}

/// Represents a trie stored in compact format.
///
/// See module-level documentation for the file format details.
///
/// `Storage` type is the backing mechanism for trie data
///
/// Currently supported `Storage`:
/// - [`Vec<u8>`], used for in-memory serialization.
/// - [`memmap2::Mmap`], used for from-disk reading.
/// - TODO(igorm): `Wrapper(File)` for direct to-disk serialization.
///
/// [ArenaCompactTree] can be constructed using [`ArenaCompactTree::<Vec<u8>>::from_zipper`]:
///
/// ... Or opened from disk using [`ArenaCompactTree::<Mmap>::open`]:
pub struct ArenaCompactTree<Storage> {
    /// Backing storage for the trie
    storage: Storage,
    /// Always points past the last byte of serialized data
    position: u64,
    /// Used for re-use of lines. Look up LineId by line hash.
    /// In case of collisions, line can't be cached.
    line_map: HashMap<u64, LineId>,
    /// Hasher for the lines.
    hasher: GxHasher,
    /// Number of stored lines
    lines: usize,
    /// Counters to debug storage usage
    counters: Counters,
    /// Currently read value, so that the reader can borrow it
    value: Cell<u64>,
}

pub type ACTVec = ArenaCompactTree<Vec<u8>>;
pub type ACTMmap = ArenaCompactTree<Mmap>;
pub type ACTVecZipper<'tree, Value> = ACTZipper<'tree, Vec<u8>, Value>;
pub type ACTMmapZipper<'tree, Value> = ACTZipper<'tree, Mmap, Value>;

impl<Storage> ArenaCompactTree<Storage> {
    fn write_line(
        dst: &mut impl Write, line: &NodeLine, node_id: NodeId,
        counters: &mut Counters,
    ) -> Result<(), std::io::Error> {
        const ARC_HEAD: u8 = 0x80;
        let value_flag = if line.value.is_some() { VALUE_FLAG } else { 0 };
        let child_flag = if line.child.is_some() { 1 } else { 0 };
        let head = ARC_HEAD | value_flag | child_flag;
        dst.write_all(&[head]).unwrap();
        if let Some(value) = line.value {
            let size = push_varint_u64(dst, value)?;
            counters.add_value(size);
        }
        if let Some(child) = line.child {
            let offset = node_id.0.checked_sub(child.0)
                .expect("Children are expected to be written first");
            let size = push_varint_u64(dst, offset as u64)?;
            counters.add_offset(size);
        }
        let offset = node_id.0.checked_sub(line.path.0)
            .expect("Children are expected to be written first");
        let size = push_varint_u64(dst, offset as u64)?;
        counters.add_offset(size);
        Ok(())
    }

    fn write_node(
        dst: &mut impl Write, node: &NodeBranch, node_id: NodeId,
        counters: &mut Counters,
    ) -> Result<(), std::io::Error> {
        let nchildren = node.bytemask.count_bits();
        let value_flag = if node.value.is_some() { VALUE_FLAG } else { 0 };
        let head = nchildren.min(32) as u8 | value_flag;
        dst.write_all(&[head]).unwrap();
        if let Some(value) = node.value {
            let size = push_varint_u64(dst, value)?;
            counters.add_value(size);
        }
        if let Some(first_child) = node.first_child {
            let offset = node_id.0.checked_sub(first_child.0)
                .expect("Children are expected to be written first");
            assert!(nchildren > 0, "child count == 0 and first_child is Some");
            let size = push_varint_u64(dst, offset as u64)?;
            counters.add_offset(size);
        }
        if nchildren >= 32 {
            counters.add_children(nchildren as usize, 32);
            for word in node.bytemask.0 {
                dst.write_all(&word.to_le_bytes())?;
            }
        } else {
            counters.add_children(nchildren as usize, nchildren as usize);
            for byte in node.bytemask.iter() {
                dst.write_all(&[byte])?;
            }
        }
        Ok(())
    }

    pub fn counters(&self) -> &Counters {
        &self.counters
    }
}

impl<Storage> ArenaCompactTree<Storage>
where Storage: AsRef<[u8]>
{
    /// Get the reference to the serialized bytes
    ///
    /// # Examples
    /// ```ignore
    /// use petta::pathmap::{PathMap, arena_compact::ArenaCompactTree};
    /// let items = ["ace", "acf", "adg", "adh", "bjk"];
    /// let btm = PathMap::from_iter(items.iter().map(|i| (i, ())));
    /// let tree1 = ArenaCompactTree::from_zipper(btm.read_zipper(), |_v| 0);
    /// let the_serialized_bytes = tree1.get_data();
    /// println!("serialized data: {the_serialized_bytes:?}");
    /// ```ignore
    pub fn get_data(&self) -> &[u8] {
        self.storage.as_ref()
    }

    /// Read node provided [NodeId]
    ///
    /// Returns a tuple of the read [Node] and next child [NodeId]
    /// The next child id is potentially invalid.
    fn get_node(&self, node_id: NodeId) -> (Node, NodeId) {
        let data = &self.storage.as_ref()[node_id.0 as usize..];
        let (node, off) = read_node(data, node_id);
        let next = NodeId(node_id.0 + off as u64);
        (node, next)
    }

    /// Read line data provided [LineId]
    ///
    /// Returns a byte slice of line data.
    fn get_line(&self, line_id: LineId) -> &[u8] {
        let start = &self.storage.as_ref()[line_id.0 as usize..];
        let (len, off) = read_varint_u64(start);
        assert!(len != 0);
        &start[off..off + len as usize]
    }

    /// Read root [Node]
    ///
    /// Returns root [Node], together with root's [NodeId].
    fn get_root(&self) -> (Node, NodeId) {
        let root_slice = &self.storage.as_ref()[MAGIC_LENGTH..][..U64_SIZE];
        let root_buf: &[u8; U64_SIZE] = root_slice.try_into()
            .expect("buffer size must be U64_SIZE, we just made it this way");
        let root_id = NodeId(u64::from_le_bytes(*root_buf));
        (self.get_node(root_id).0, root_id)
    }

    /// Find existing [LineId] that contains provided line `data`
    ///
    /// This is done by calculating the hash of the data, and storing it in a map.
    /// Because of this, this function can give a false negative in case of collision
    /// This happens in `~1/5e9` cases, so we probably don't care about that.
    ///
    /// NOTE: the hash function is chosen to be deterministic, for consistency,
    /// Which means it's possible to contruct a malicious set of paths which
    /// will not able to reuse lines.
    fn find_line_reuse(&self, data: impl AsRef<[u8]>) -> Option<LineId> {
        let data = data.as_ref();
        let mut hasher = self.hasher.clone();
        hasher.write(data);
        let hash = hasher.finish();
        let line_id = *self.line_map.get(&hash)?;
        (self.get_line(line_id) == data).then_some(line_id)
    }

    /// Read `index`'th sibling starting from `node_id`
    ///
    /// Returns [Node] data together with it's [NodeId] and next siblings's [NodeId]
    fn nth_node(&self, mut node_id: NodeId, index: usize) -> (Node, NodeId, NodeId) {
        let (mut node, mut next) = self.get_node(node_id);
        for _ii in 0..index {
            let (nnode, nnext) = self.get_node(next);
            node_id = next;
            next = nnext;
            node = nnode;
        }
        (node, node_id, next)
    }

    /// Returns the value at the specified `path`, or `None` if no value exists
    pub fn get_val_at<K: AsRef<[u8]>>(&self, path: K) -> Option<u64> {
        let mut path = path.as_ref();
        let mut cur_node = self.get_root().0;
        loop {
            match cur_node {
                Node::Line(line) => {
                    let lpath = self.get_line(line.path);
                    if !starts_with(path, lpath) {
                        return None;
                    }
                    path = &path[lpath.len()..];
                    if path.is_empty() && line.value.is_some() {
                        return line.value;
                    }
                    cur_node = self.get_node(line.child?).0;
                }
                Node::Branch(node) => {
                    if path.is_empty() {
                        return node.value;
                    }
                    let first_child = node.first_child?;
                    let idx = node.bytemask.index_of(path[0]) as usize;
                    cur_node = self.nth_node(first_child, idx).0;
                    path = &path[1..];
                }
            }
        }
    }

    /// Deprecated alias for [Self::get_val_at]
    #[deprecated] //GOAT-old-names
    pub fn get<K: AsRef<[u8]>>(&self, path: K) -> Option<u64> {
        self.get_val_at(path)
    }
}

impl<Storage> ArenaCompactTree<Storage>
where Storage: Write
{
    fn push_node(&mut self, node: &NodeBranch)
        -> Result<NodeId, std::io::Error>
    {
        let node_id = NodeId(self.position);
        let mut cursor = std::io::Cursor::new([0; MAX_BRANCH_NODE_SIZE]);
        Self::write_node(&mut cursor, node, node_id, &mut self.counters)?;
        let len = cursor.position();
        self.counters.add_node(len as usize);
        self.storage.write_all(&cursor.get_ref()[..len as usize])?;
        self.position += len;
        Ok(node_id)
    }

    fn push_line(&mut self, line: &NodeLine)
        -> Result<NodeId, std::io::Error>
    {
        let node_id = NodeId(self.position);
        let mut cursor = std::io::Cursor::new([0; MAX_LINE_NODE_SIZE]);
        Self::write_line(&mut cursor, line, node_id, &mut self.counters)?;
        let len = cursor.position();
        self.counters.add_line(len as usize);
        self.storage.write_all(&cursor.get_ref()[..len as usize])?;
        self.position += len;
        Ok(node_id)
    }

    fn push(&mut self, node: &Node) -> Result<NodeId, std::io::Error> {
        let (node_id, _kind) = match node {
            Node::Line(line) => (self.push_line(line), "line"),
            Node::Branch(branch) => (self.push_node(branch), "bra"),
        };
        if DO_TRACE { eprintln!("push {node_id:?} node={node:?}"); }
        // debug_assert_eq!(self.position, self.storage.len() as u64, "failed push {_kind}");
        node_id
    }

    fn finalize(&mut self) -> Result<(), std::io::Error> {
        // Invariant: There must always be a 9-byte slice at the end
        // This allows [ValueSlice] to always point at correct data,
        // And readers to always be able to read a varint.
        self.storage.write_all(&[0; MAX_VARINT_SIZE - 1])?;
        self.storage.flush()
    }
}
/*
impl ArenaCompactTree<File> {
    fn find_line_reuse(&mut self, line: impl AsRef<[u8]>) -> Option<LineId> {
        use std::io::SeekFrom;
        use std::io::Seek;
        let line = line.as_ref();
        let mut hasher = self.hasher.clone();
        hasher.write(line);
        let hash = hasher.finish();
        let line_id = *self.line_map.get(&hash)?;
        self.storage.seek(SeekFrom::Start(line_id.0 as u64)).unwrap();
        (self.get_line(line_id) == line).then_some(line_id)
    }
}
*/

impl ArenaCompactTree<Vec<u8>> {
    fn new() -> Self {
        // Allocate the space for the header + root node
        let mut storage = COMPACT_TREE_MAGIC.to_vec();
        storage.extend_from_slice(&[0; U64_SIZE]);
        Self {
            position: storage.len() as u64,
            storage,
            line_map: HashMap::new(),
            hasher: Default::default(),
            lines: 0,
            counters: Counters::default(),
            value: Cell::new(0),
        }
    }

    /// Construct [ArenaCompactTree] from a read zipper.
    /// # Examples
    /// ```ignore
    /// use petta::pathmap::{PathMap, arena_compact::ArenaCompactTree};
    /// let items = ["ace", "acf", "adg", "adh", "bjk"];
    /// let btm = PathMap::from_iter(items.iter().map(|i| (i, ())));
    /// let tree1 = ArenaCompactTree::from_zipper(btm.read_zipper(), |_v| 0);
    /// let mut zipper = tree1.read_zipper();
    /// for path in items {
    ///     use petta::pathmap::zipper::ZipperMoving;
    ///     zipper.reset();
    ///     assert!(zipper.descend_to_existing(path) == path.len());
    ///     assert_eq!(zipper.path(), path.as_bytes());
    /// }
    /// let tree2 = ArenaCompactTree::from_zipper(tree1.read_zipper(), |_v| 0);
    /// assert_eq!(tree1.get_data(), tree2.get_data())
    /// ```ignore
    #[inline]
    pub fn from_zipper<V, Z, M>(zipper: Z, map: M) -> Self
    where
        V: Clone + Send + Sync + Unpin,
        Z: Catamorphism<V>,
        M: Fn(&V) -> u64,
    {
        build_arena_tree(zipper, map)
    }

    fn push_v(&mut self, node: &Node) -> NodeId {
        self.push(node).expect("push to vec doesn't fail")
    }

    fn set_root(&mut self, node: &Node) -> NodeId {
        let node_id = self.push_v(node);
        let root_buf = &mut self.storage[MAGIC_LENGTH..][..U64_SIZE];
        root_buf.copy_from_slice(&node_id.0.to_le_bytes());
        node_id
    }

    fn add_path(&mut self, line: impl AsRef<[u8]>) -> LineId {
        let line = line.as_ref();
        let line_id = LineId(self.position);

        const REUSE_ARCS: bool = true;
        if REUSE_ARCS {
            // caching lines
            if let Some(prev) = self.find_line_reuse(line) {
                self.counters.add_line_data_reuse(line.len());
                return prev;
            }
            let mut hasher = self.hasher.clone();
            hasher.write(line);
            self.line_map.insert(hasher.finish(), line_id);
        }
        let lenlen = push_varint_u64(&mut self.storage, line.len() as u64)
            .expect("writing to vec should never fail.");
        self.storage.extend_from_slice(line);
        self.counters.add_line_data(lenlen + line.len());
        self.position = self.storage.len() as u64;
        self.lines += 1;
        line_id
    }
}

use memmap2::Mmap;
use std::path::Path;

impl ArenaCompactTree<Mmap> {
    /// Memmap a file and use it as backing storage for the trie
    ///
    /// # Examples
    /// ```ignore
    /// use petta::pathmap::{PathMap, arena_compact::ArenaCompactTree};
    /// use tempfile::NamedTempFile;
    /// use std::io::Write;
    /// # fn main() -> std::io::Result<()> {
    /// let mut file = NamedTempFile::new()?;
    /// let items = ["ace", "acf", "adg", "adh", "bjk"];
    /// let btm = PathMap::from_iter(items.iter().map(|i| (i, ())));
    /// let tree1 = ArenaCompactTree::from_zipper(btm.read_zipper(), |_v| 0);
    /// file.write_all(tree1.get_data())?;
    /// let tree_path = file.path();
    /// let tree2 = ArenaCompactTree::open_mmap(tree_path)?;
    /// assert_eq!(tree1.get_data(), tree2.get_data());
    /// # Ok(())
    /// # }
    /// ```ignore
    pub fn open_mmap(path: impl AsRef<Path>) -> std::io::Result<Self> {
        let file = std::fs::File::open(&path)?;
        let memmap = unsafe { Mmap::map(&file) }?;
        if &memmap[..MAGIC_LENGTH] != &COMPACT_TREE_MAGIC {
            return Err(std::io::Error::other("Invalid file magic"));
        }
        Ok(Self {
            position: memmap.as_ref().len() as u64,
            storage: memmap,
            line_map: Default::default(),
            lines: Default::default(),
            hasher: Default::default(),
            value: Cell::new(0),
            counters: Counters::default(),
        })
    }


    /// ```ignore
    /// use petta::pathmap::{PathMap, arena_compact::ArenaCompactTree};
    /// use tempfile::NamedTempFile;
    /// use std::io::Write;
    /// # fn main() -> std::io::Result<()> {
    /// let mut file = NamedTempFile::new()?;
    /// let tree_path = "test_tree.tree";
    /// let items = ["ace", "acf", "adg", "adh", "bjk"];
    /// let btm = PathMap::from_iter(items.iter().map(|i| (i, ())));
    /// let tree1 = ArenaCompactTree::dump_from_zipper(
    ///     btm.read_zipper(), |_v| 0, tree_path)?;
    /// let tree2 = ArenaCompactTree::from_zipper(
    ///     btm.read_zipper(), |_v| 0);
    /// assert_eq!(tree1.get_data(), tree2.get_data());
    /// # Ok(())
    /// # }
    /// ```ignore
    pub fn dump_from_zipper<V, Z, F, P>(
        zipper: Z, map_val: F, path: P
    ) -> Result<Self, std::io::Error>
        where
            V: Clone + Send + Sync + Unpin,
            Z: Catamorphism<V>,
            F: Fn(&V) -> u64,
            P: AsRef<Path>
    {
        let arena = dump_arena_tree(zipper, map_val, path)?;
        let file = arena.storage.buf_writer.into_inner()?;
        let memmap = unsafe { Mmap::map(&file) }?;
        if &memmap[..MAGIC_LENGTH] != &COMPACT_TREE_MAGIC {
            return Err(std::io::Error::other("Invalid file magic"));
        }
        Ok(Self {
            position: memmap.as_ref().len() as u64,
            storage: memmap,
            line_map: Default::default(),
            lines: Default::default(),
            hasher: Default::default(),
            value: Cell::new(0),
            counters: arena.counters,
        })
    }
}

#[derive(Clone, Debug)]
pub enum Node {
    Line(NodeLine),
    Branch(NodeBranch),
}

impl Node {
    pub fn child_count(&self) -> usize {
        match self {
            Node::Line(line) => if line.child.is_some() { 1 } else { 0 },
            Node::Branch(node) => node.bytemask.count_bits(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct NodeLine {
    pub path: LineId,
    // pub footprint: u64,
    pub value: Option<u64>,
    pub child: Option<NodeId>,
}

impl NodeLine {
    pub fn empty() -> Self {
        Self {
            path: INVALID_LINE,
            // footprint: 0,
            value: None,
            child: None,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct NodeBranch {
    pub bytemask: ByteMask,
    pub first_child: Option<NodeId>,
    // pub footprint: u64,
    pub value: Option<u64>,
}

impl NodeBranch {
    pub fn empty() -> Self {
        Self {
            bytemask: ByteMask::EMPTY,
            first_child: None,
            // footprint: 0,
            value: None,
        }
    }
}

// benchmark for morphisms (caching/side, jumping/plain)
// val_count ?
fn build_arena_tree<V, Z, F>(zipper: Z, map_val: F) -> ArenaCompactTree<Vec<u8>>
    where
        V: Clone + Send + Sync + Unpin,
        Z: Catamorphism<V>,
        F: Fn(&V) -> u64,
{
    let mut arena = ArenaCompactTree::new();
    let map_val = &map_val;
    let root = zipper.into_cata_jumping_side_effect::<Node, _>(|bm, children, jump, v, path| {
        let mut first_child: Option<NodeId> = None;
        for child in children.iter() {
            let id = arena.push_v(child);
            first_child = first_child.or(Some(id));
        }
        let node = NodeBranch {
            bytemask: ByteMask::from(*bm),
            first_child,
            value: v.map(map_val),
        };
        if jump == 0 {
            return Node::Branch(node);
        }

        let mut line = NodeLine::empty();
        line.path = arena.add_path(&path[path.len() - jump..]);

        if !children.is_empty() {
            first_child = Some(arena.push_v(&Node::Branch(node)));
        } else {
            line.value = v.map(map_val);
        }

        line.child = first_child;
        Node::Line(line)
    });
    let _root_id = arena.set_root(&root);
    arena.finalize().unwrap();
    arena
}

use std::io::{BufWriter, Seek, SeekFrom};
use std::fs::{File, OpenOptions};

pub struct FileDumper {
    buf_writer: BufWriter<File>,
    line_buf: Vec<u8>,
    line_map: HashMap::<u64, (usize, usize, LineId)>,
}

impl Write for FileDumper {
    fn write(&mut self, data: &[u8]) -> Result<usize, std::io::Error> {
        self.buf_writer.write(data)
    }

    fn flush(&mut self) -> Result<(), std::io::Error> {
        self.buf_writer.flush()
    }
}

/// BufWriter buffer size. The default of 8KiB is too small.
const DUMPER_BUFFER_SIZE: usize = 4*1024*1024;
impl ArenaCompactTree<FileDumper> {
    fn open(path: impl AsRef<Path>) -> Result<Self, std::io::Error> {
        let mut file = OpenOptions::new()
            .read(true).write(true)
            .create(true).truncate(true)
            .open(path)?;
        file.seek(SeekFrom::Start(0))?;
        file.write_all(&COMPACT_TREE_MAGIC)?;
        file.write_all(&[0; 8])?;
        let position = file.stream_position()?;
        let buf_writer = BufWriter::with_capacity(DUMPER_BUFFER_SIZE, file);
        let storage = FileDumper {
            buf_writer,
            line_buf: Default::default(),
            line_map: Default::default(),
        };
        let act = ArenaCompactTree {
            storage,
            position,
            line_map: HashMap::new(),
            hasher: GxHasher::default(),
            lines: 0,
            counters: Counters::default(),
            value: Cell::new(0),
        };
        Ok(act)
    }

    fn set_root(&mut self, node: &Node) -> Result<NodeId, std::io::Error> {
        let node_id = self.push(node)?;
        self.storage.buf_writer.seek(SeekFrom::Start(8))?;
        self.storage.write_all(&node_id.0.to_le_bytes())?;
        self.storage.buf_writer.seek(SeekFrom::Start(self.position))?;
        Ok(node_id)
    }

    fn add_path(
        &mut self, path: impl AsRef<[u8]>
    ) -> Result<LineId, std::io::Error> {
        let path = path.as_ref();
        let mut hasher = self.hasher.clone();
        hasher.write(path);
        let hash = hasher.finish();
        if let Some(&(start, len, prev)) = self.storage.line_map.get(&hash) {
            let buf = &self.storage.line_buf[start..start+len];
            if buf == path {
                self.counters.add_line_data_reuse(path.len());
                return Ok(prev);
            }
        }
        let line_id = LineId(self.position);
        let line_start = self.storage.line_buf.len();
        self.storage.line_buf.extend_from_slice(path);
        let lenlen = push_varint_u64(
            &mut self.storage, path.len() as u64
        )? as u64;
        self.position += lenlen;
        self.storage.write_all(path)?;
        self.position += path.len() as u64;
        self.counters.add_line_data(lenlen as usize + path.len());
        self.storage.line_map.insert(hash, (line_start, path.len(), line_id));
        Ok(line_id)
    }
}

fn dump_arena_tree<V, Z, F, P>(
    zipper: Z, map_val: F, path: P
) -> Result<ArenaCompactTree<FileDumper>, std::io::Error>
    where
        V: Clone + Send + Sync + Unpin,
        Z: Catamorphism<V>,
        F: Fn(&V) -> u64,
        P: AsRef<Path>,
{
    // A bit of code duplication compared to build_arena_tree
    let mut arena = ArenaCompactTree::<FileDumper>::open(path)?;
    let map_val = &map_val;
    let root = zipper.into_cata_jumping_side_effect_fallible::<Node, std::io::Error, _>(|bm, children, jump, v, path| {
        let mut first_child: Option<NodeId> = None;
        for child in children.iter() {
            let id = arena.push(child)?;
            first_child = first_child.or(Some(id));
        }
        let node = NodeBranch {
            bytemask: ByteMask::from(*bm),
            first_child,
            value: v.map(map_val),
        };
        if jump == 0 {
            return Ok(Node::Branch(node));
        }

        let mut line = NodeLine::empty();
        line.path = arena.add_path(&path[path.len() - jump..])?;

        if !children.is_empty() {
            first_child = Some(arena.push(&Node::Branch(node))?);
        } else {
            line.value = v.map(map_val);
        }

        line.child = first_child;
        Ok(Node::Line(line))
    })?;

    let _root_id = arena.set_root(&root)?;
    arena.finalize().unwrap();
    Ok(arena)
}

/*
fn tree_to_btm(tree: &ArenaCompactTree) -> PathMap<()> {
    struct PathIdx(NodeId, usize)
    let (_root, root_id) = tree.get_root();
    PathMap::<()>::new_from_ana(PathIdx(root_id, 0), |PathIdx(node_id, depth), val, children, _path| {
        match tree.get_node(node_id) {
            Node::Line(line) => {
                *val = line.value.map(|_| ());
            }
            Node::Branch(node) => {
                *val = node.value.map(|_| ());
            }
        }
    })
}
*/
#[derive(Clone, Debug)]
struct StackFrame {
    node_id: NodeId,
    child_count: usize,
    child_index: usize,
    next_id: Option<NodeId>,
    node_depth: usize,
}
impl StackFrame {
    fn from(node: &Node, node_id: NodeId) -> Self {
        StackFrame {
            node_id,
            child_count: node.child_count(),
            child_index: 0,
            next_id: None,
            node_depth: 0,
        }
    }
}

pub struct ACTZipper<'tree, Storage, Value>
where Storage: AsRef<[u8]>
{
    tree: &'tree ArenaCompactTree<Storage>,
    cur_node: Node,
    stack: Vec<StackFrame>,
    path: Vec<u8>,
    origin_depth: usize,
    origin_node_depth: usize,
    pub invalid: usize,
    _marker: PhantomData<Value>,
}

impl<'tree, Storage, Value> Clone for ACTZipper<'tree, Storage, Value>
where Storage: AsRef<[u8]>
{
    fn clone(&self) -> Self {
        let Self {
            tree, cur_node, stack, path,
            origin_depth, origin_node_depth, invalid, ..
        } = self;
        Self {
            tree,
            cur_node: cur_node.clone(),
            stack: stack.clone(),
            path: path.clone(),
            origin_depth: *origin_depth,
            origin_node_depth: *origin_node_depth,
            invalid: *invalid,
            _marker: PhantomData,
        }
    }
}

impl<Storage> ArenaCompactTree<Storage>
where Storage: AsRef<[u8]>
{
    #[inline]
    pub fn read_zipper_u64<'tree>(&'tree self) -> ACTZipper<'tree, Storage, u64> {
        ACTZipper::from_tree(self)
    }

    #[inline]
    pub fn read_zipper_at_path_u64<'tree>(&'tree self, path: &[u8]) -> ACTZipper<'tree, Storage, u64> {
        let mut rz = ACTZipper::from_tree(self);
        rz.descend_to(path);
        rz.with_root_here()
    }

    #[inline]
    pub fn read_zipper_at_borrowed_path_u64<'tree>(&'tree self, path: &[u8]) -> ACTZipper<'tree, Storage, u64> {
        self.read_zipper_at_path_u64(path)
    }

    #[inline]
    pub fn read_zipper<'tree>(&'tree self) -> ACTZipper<'tree, Storage, ()> {
        ACTZipper::from_tree(self)
    }

    #[inline]
    pub fn read_zipper_at_path<'tree>(&'tree self, path: &[u8]) -> ACTZipper<'tree, Storage, ()> {
        let mut rz = ACTZipper::from_tree(self);
        rz.descend_to(path);
        rz.with_root_here()
    }

    #[inline]
    pub fn read_zipper_at_borrowed_path<'tree>(&'tree self, path: &[u8]) -> ACTZipper<'tree, Storage, ()> {
        self.read_zipper_at_path(path)
    }
}

impl<'tree, Storage, Value> ACTZipper<'tree, Storage, Value>
where Storage: AsRef<[u8]>
{
    fn from_tree(tree: &'tree ArenaCompactTree<Storage>) -> Self {
        let (cur_node, node_id) = tree.get_root();
        let stack_frame = StackFrame::from(&cur_node, node_id);
        ACTZipper {
            tree, cur_node,
            path: Vec::new(),
            invalid: 0,
            origin_depth: 0,
            origin_node_depth: 0,
            stack: Vec::from([stack_frame]),
            _marker: PhantomData,
        }
    }

    fn with_root_here(mut self) -> Self {
        self.origin_depth = self.path.len();
        if self.stack.len() > 1 {
            let last = self.stack.len() - 1;
            self.stack.swap(0, last);
            self.stack.truncate(1);
        }
        self.origin_node_depth = self.stack[0].node_depth;
        self
    }
}

impl<'tree, Storage> ZipperReadOnlyConditionalValues<'tree, ()> for ACTZipper<'tree, Storage, ()>
where Storage: AsRef<[u8]>
{
    type WitnessT = ();
    fn witness<'w>(&self) -> Self::WitnessT {}
    fn get_val_with_witness<'w>(&self, _witness: &'w Self::WitnessT) -> Option<&'w ()> where 'tree: 'w {
        self.get_val()
    }
}

impl<'tree, Storage> ZipperReadOnlyConditionalValues<'tree, u64> for ACTZipper<'tree, Storage, u64>
where Storage: AsRef<[u8]>
{
    type WitnessT = ();
    fn witness<'w>(&self) -> Self::WitnessT {}
    fn get_val_with_witness<'w>(&self, _witness: &'w Self::WitnessT) -> Option<&'w u64> where 'tree: 'w {
        self.get_val()
    }
}

impl<'tree, Storage, Value> Zipper for ACTZipper<'tree, Storage, Value>
where Storage: AsRef<[u8]>
{
    /// Returns `true` if the zipper's focus is on a path within the trie, otherwise `false`
    fn path_exists(&self) -> bool {
        self.invalid == 0
    }

    /// Returns `true` if there is a value at the zipper's focus, otherwise `false`
    fn is_val(&self) -> bool {
        if self.invalid > 0 {
            return false;
        }
        match &self.cur_node {
            Node::Branch(node) => {
                node.value.is_some()
            }
            Node::Line(line) => {
                if line.value.is_none() {
                    false
                } else {
                    let last = self.stack.last().unwrap();
                    let line = self.tree.get_line(line.path);
                    line.len() == last.node_depth
                }
            }
        }
    }

    /// Returns the number of child branches from the focus node
    ///
    /// Returns 0 if the focus is on a leaf
    fn child_count(&self) -> usize {
        if self.invalid > 0 {
            return 0;
        }
        match &self.cur_node {
            Node::Branch(node) => {
                node.bytemask.count_bits()
            }
            Node::Line(path) => {
                let last = self.stack.last().unwrap();
                let path = self.tree.get_line(path.path);
                if last.node_depth < path.len() {
                    1
                } else {
                    0
                }
            }
        }
    }

    /// Returns 256-bit mask indicating which children exist from the branch at the zipper's focus
    ///
    /// Returns an empty mask if the focus is on a leaf or non-existent path
    fn child_mask(&self) -> ByteMask {
        if self.invalid > 0 {
            return ByteMask::EMPTY;
        }
        match &self.cur_node {
            Node::Branch(node) => {
                node.bytemask
            }
            Node::Line(path) => {
                let top_frame = self.stack.last().unwrap();
                let path = self.tree.get_line(path.path);
                if top_frame.node_depth == path.len() {
                    ByteMask::EMPTY
                } else {
                    ByteMask::from(path[top_frame.node_depth])
                }
            }
        }
    }
}

impl<'tree, Storage, Value> ZipperAbsolutePath for ACTZipper<'tree, Storage, Value>
where Storage: AsRef<[u8]>
{
    fn origin_path(&self) -> &[u8] {
        &self.path[..]
    }

    fn root_prefix_path(&self) -> &[u8] {
        &self.path[..self.origin_depth]
    }
}

impl<'tree, Storage, Value> ZipperPathBuffer for ACTZipper<'tree, Storage, Value>
where Storage: AsRef<[u8]>
{
    unsafe fn origin_path_assert_len(&self, len: usize) -> &[u8] {
        // Safety: we're not creating a slice larger than capacity
        assert!(self.path.capacity() >= len);
        unsafe{ core::slice::from_raw_parts(self.path.as_ptr(), len) }
    }

    fn reserve_buffers(&mut self, path_len: usize, stack_depth: usize) {
        self.path.reserve(path_len.saturating_sub(self.path.len()));
        self.stack.reserve(stack_depth.saturating_sub(self.stack.len()));
    }

    fn prepare_buffers(&mut self) {
    }
}

impl<'tree, Storage> ZipperSubtries<(), GlobalAlloc> for ACTZipper<'tree, Storage, ()>
where Storage: AsRef<[u8]>
{
    fn native_subtries(&self) -> bool { false }
    fn try_make_map(&self) -> Option<PathMap<(), GlobalAlloc>> { None }
    fn trie_ref(&self) -> Option<TrieRef<'_, (), GlobalAlloc>> { None }
    fn alloc(&self) -> GlobalAlloc { global_alloc() }
}

const DO_TRACE: bool = false;
impl<'tree, Storage, Value> ACTZipper<'tree, Storage, Value>
where Storage: AsRef<[u8]>
{
    fn trace_pos(&self) {
        if !DO_TRACE { return; }
        let last_frame = self.stack.last().unwrap();
        eprintln!("node={:?}, path={:?}, depth={}",
            last_frame.node_id, self.path, last_frame.node_depth);
    }
    fn get_value(&self) -> Option<u64> {
        if !self.is_val() {
            return None;
        }
        let top_frame = self.stack.last()?;
        let node_id = top_frame.node_id.0;
        let data = &self.tree.storage.as_ref()[node_id as usize..];
        let head = data[0];
        if head & VALUE_FLAG == 0 {
            return None;
        }
        let value = read_varint_u64(&data[1..]).0;
        Some(value)
    }

    fn ascend_invalid(&mut self, limit: Option<&mut usize>) -> bool {
        if self.invalid == 0 {
            return true;
        }
        let len = self.path.len();
        let mut invalid_cut = self.invalid.min(len - self.origin_depth);
        if let Some(limit) = limit {
            invalid_cut = invalid_cut.min(*limit);
            *limit -= invalid_cut;
        }
        self.path.truncate(len - invalid_cut);
        self.invalid = self.invalid - invalid_cut;
        self.invalid == 0
    }

    fn ascend_to_branch(&mut self, need_value: bool) -> bool {
        self.trace_pos();
        let mut moved = false;
        if self.invalid > 0 {
            moved = true;
            if !self.ascend_invalid(None) {
                return false;
            }

            match &self.cur_node {
                Node::Line(line) => {
                    if need_value && line.value.is_some() {
                        return true;
                    }
                }
                Node::Branch(node) => {
                    if need_value && node.value.is_some() {
                        return true;
                    }
                }
            }
        }
        while let Some(top_frame) = self.stack.last_mut() {
            let mut nchildren = top_frame.child_count;
            let mut this_steps = top_frame.node_depth
                .min(self.path.len() - self.origin_depth);
            top_frame.node_depth = 0;
            moved |= this_steps > 0;
            if self.stack.len() > 1 {
                self.stack.pop();
                let prev = self.stack.last().unwrap();
                self.cur_node = self.tree.get_node(prev.node_id).0;
                nchildren = prev.child_count;
                moved = true;
                this_steps += 1;
            }
            self.path.truncate(self.path.len() - this_steps);
            // eprintln!("path={:?}", self.path);
            let brk = match &self.cur_node {
                Node::Branch(node) => {
                    (nchildren > 1) || (need_value && node.value.is_some())
                }
                _ => false,
            };
            if brk || self.at_root() {
                break;
            }
        }
        moved
    }

    fn descend_cond(&mut self, path: &[u8], on_value: bool) -> usize {
        self.trace_pos();
        if self.invalid > 0 {
            return 0;
        }
        let mut descended = 0;
        let mut path = path.as_ref();
        'descend: while !path.is_empty() {
            match &self.cur_node {
                Node::Line(line) => {
                    let frame = self.stack.last_mut().unwrap();
                    let node_path = &self.tree.get_line(line.path);
                    let rest_path = &node_path[frame.node_depth..];
                    let common = find_prefix_overlap(path, rest_path);
                    descended += common;
                    path = &path[common..];
                    let into_child = rest_path.len() == common && line.child.is_some();
                    let line_child_hack = if into_child { 1 } else { 0 };
                    frame.node_depth += common - line_child_hack;
                    self.path.extend_from_slice(&rest_path[..common]);
                    if on_value && descended > 0 && line.value.is_some() {
                        break 'descend;
                    }
                    if common < rest_path.len() {
                        break 'descend;
                    }
                    let Some(node_id) = line.child else { break 'descend };
                    let (node, _next_id) = self.tree.get_node(node_id);
                    // no need to update next_id
                    self.stack.push(StackFrame::from(&node, node_id));
                    self.cur_node = node;
                }
                Node::Branch(node) => {
                    if on_value && descended > 0 && node.value.is_some() {
                        break 'descend;
                    }
                    if !node.bytemask.test_bit(path[0]) {
                        break 'descend;
                    }
                    let idx = node.bytemask.index_of(path[0]) as usize;
                    let frame = self.stack.last_mut().unwrap();
                    let ((node, next_id), node_id) = if frame.next_id.is_some() && frame.child_index + 1 == idx {
                        // Optimization: if we know the exact next node, descend
                        (self.tree.get_node(frame.next_id.unwrap()), frame.next_id.unwrap())
                    } else {
                        let (node, node_id, next_id) = self.tree
                            .nth_node(node.first_child.unwrap(), idx);
                        ((node, next_id), node_id)
                    };
                    frame.child_index = idx;
                    frame.next_id = Some(next_id);
                    self.stack.push(StackFrame::from(&node, node_id));
                    self.cur_node = node;
                    self.path.push(path[0]);
                    path = &path[1..];
                    descended += 1;
                }
            }
        }
        descended
    }

    fn to_sibling(&mut self, next: bool) -> bool {
        let top_frame = self.stack.last().unwrap();
        if self.stack.len() <= 1 || top_frame.node_depth > 0 {
            // can't move to sibling at root, or along the path
            return false;
        }
        let top2_frame = &self.stack[self.stack.len() - 2];
        let sibling_idx = if next {
            let idx = top2_frame.child_index + 1;
            if idx >= top2_frame.child_count {
                return false;
            }
            idx
        } else {
            if top2_frame.child_index == 0 {
                return false;
            }
            top2_frame.child_index - 1
        };
        self.ascend(1) && self.descend_indexed_byte(sibling_idx)
    }
}

impl<'tree, Storage> ZipperValues<()> for ACTZipper<'tree, Storage, ()>
where Storage: AsRef<[u8]>
{
    fn val(&self) -> Option<&()> {
        self.get_value().map(|_x| &())
    }
}

impl<'tree, Storage> ZipperValues<u64> for ACTZipper<'tree, Storage, u64>
where Storage: AsRef<[u8]>
{
    fn val(&self) -> Option<&u64> {
        let value = self.get_value()?;
        if self.tree.value.get() != value {
            self.tree.value.set(value);            
        }
        let ptr = self.tree.value.as_ptr();
        // technically if someone borrows the value twice, they will hit UB
        // since we provided a read-only reference to the value, and we ALSO
        // can update it.
        // all of this is done so that the value can be borrowed with the same
        // lifetime as the tree.
        Some(unsafe { &*ptr })
    }
}

impl<'tree, Storage> ZipperForking<()> for ACTZipper<'tree, Storage, ()>
where Storage: AsRef<[u8]>
{
    type ReadZipperT<'t> = ACTZipper<'t, Storage, ()> where Self: 't;
    fn fork_read_zipper<'a>(&'a self) -> Self::ReadZipperT<'a> {
        self.clone().with_root_here()
    }
}

impl<'tree, Storage> ZipperForking<u64> for ACTZipper<'tree, Storage, u64>
where Storage: AsRef<[u8]>
{
    type ReadZipperT<'t> = ACTZipper<'t, Storage, u64> where Self: 't;
    fn fork_read_zipper<'a>(&'a self) -> Self::ReadZipperT<'a> {
        self.clone().with_root_here()
    }
}

impl<'tree, Storage> ZipperReadOnlyValues<'tree, ()> for ACTZipper<'tree, Storage, ()>
where Storage: AsRef<[u8]>
{
    fn get_val(&self) -> Option<&'tree ()> {
        self.get_value().map(|_x| &())
    }
}

impl<'tree, Storage> ZipperReadOnlyValues<'tree, u64> for ACTZipper<'tree, Storage, u64>
where Storage: AsRef<[u8]>
{
    fn get_val(&self) -> Option<&'tree u64> {
        let value = self.get_value()?;
        if self.tree.value.get() != value {
            self.tree.value.set(value);
        }
        let ptr = self.tree.value.as_ptr();
        Some(unsafe { &*ptr })
    }
}

impl<'tree, Storage, Value> ZipperConcrete for ACTZipper<'tree, Storage, Value>
where Storage: AsRef<[u8]>
{
    fn shared_node_id(&self) -> Option<u64> {
        // TODO: no way to detect now
        None
    }
    fn is_shared(&self) -> bool {
        // TODO: no way to detect now
        false
    }
}

/// An interface to enable moving a zipper around the trie and inspecting paths
impl<'tree, Storage, Value> ZipperMoving for ACTZipper<'tree, Storage, Value>
where Storage: AsRef<[u8]>
{
    /// Returns `true` if the zipper cannot ascend further, otherwise returns `false`
    fn at_root(&self) -> bool { self.path.len() <= self.origin_depth }

    /// Resets the zipper's focus back to the root
    fn reset(&mut self) {
        // self.ascend(self.path.len() - self.origin_depth);
        let (cur_node, _) = self.tree.get_node(self.stack[0].node_id);
        self.cur_node = cur_node;
        self.stack.truncate(1);
        self.stack[0].node_depth = self.origin_node_depth;
        self.path.truncate(self.origin_depth);
        self.invalid = 0;
    }

    /// Returns the path from the zipper's root to the current focus
    fn path(&self) -> &[u8] { &self.path[self.origin_depth..] }

    /// Returns the total number of values contained at and below the zipper's focus, including the focus itself
    ///
    /// WARNING: This is not a cheap method. It may have an order-N cost
    fn val_count(&self) -> usize {
        let mut zipper = self.clone();
        zipper.reset();
        let mut count = 0;
        if zipper.is_val() {
            count += 1;
        }
        while zipper.to_next_val() {
            count += 1;
        }
        count
    }

    /// Moves the zipper deeper into the trie, to the `key` specified relative to the current zipper focus
    ///
    /// Returns `true` if the zipper points to an existing path within the tree, otherwise `false`.  The
    /// zipper's location will be updated, regardless of whether or not the path exists within the tree.
    fn descend_to<P: AsRef<[u8]>>(&mut self, path: P) {
        let path = path.as_ref();
        let depth = path.len();
        let descended = self.descend_to_existing(path);
        if descended != depth {
            self.path.extend_from_slice(&path[descended..]);
            self.invalid += depth - descended;
        }
    }

    /// Moves the zipper deeper into the trie, following the path specified by `k`, relative to the current
    /// zipper focus.  Descent stops at the point where the path does not exist
    ///
    /// Returns the number of bytes descended along the path.  The zipper's focus will always be on an
    /// existing path after this method returns, unless the method was called with the focus on a
    /// non-existent path.
    fn descend_to_existing<P: AsRef<[u8]>>(&mut self, path: P) -> usize {
        self.descend_cond(path.as_ref(), false)
    }

    /// Moves the zipper deeper into the trie, following the path specified by `k`, relative to the current
    /// zipper focus.  Descent stops if a value is encountered or if the path ceases to exist.
    ///
    /// Returns the number of bytes descended along the path.
    ///
    /// If the focus is already on a value, this method will descend to the *next* value along
    /// the path.
    fn descend_to_value<K: AsRef<[u8]>>(&mut self, path: K) -> usize {
        self.descend_cond(path.as_ref(), true)
    }

    fn descend_to_val<K: AsRef<[u8]>>(&mut self, path: K) -> usize {
        self.descend_cond(path.as_ref(), true)
    }

    /// Moves the zipper one byte deeper into the trie.  Identical in effect to [descend_to](Self::descend_to)
    /// with a 1-byte key argument
    fn descend_to_byte(&mut self, k: u8) {
        self.descend_to(&[k])
    }

    /// Descends the zipper's focus one byte into a child branch uniquely identified by `child_idx`
    ///
    /// `child_idx` must within the range `0..child_count()` or this method will do nothing and return `false`
    ///
    /// WARNING: The branch represented by a given index is not guaranteed to be stable across modifications
    /// to the trie.  This method should only be used as part of a directed traversal operation, but
    /// index-based paths may not be stored as locations within the trie.
    fn descend_indexed_byte(&mut self, idx: usize) -> bool {
        if self.invalid > 0 {
            return false;
        }
        self.trace_pos();
        let mut child_id: Option<NodeId> = None;
        match &self.cur_node {
            Node::Line(line) => {
                let top_frame = self.stack.last_mut().unwrap();
                let path = self.tree.get_line(line.path);
                let rest_path = &path[top_frame.node_depth..];
                if idx != 0 || rest_path.is_empty() {
                    return false;
                }
                self.path.push(rest_path[0]);
                if let (true, Some(line_child)) = (rest_path.len() == 1, line.child) {
                    child_id = Some(line_child);
                } else {
                    top_frame.node_depth += 1;
                    return true;
                }
            }
            Node::Branch(node) => {
                let top_frame = self.stack.last_mut().unwrap();
                if idx > top_frame.child_count {
                    return false;
                }
                let byte = node.bytemask.indexed_bit::<true>(idx);
                if let Some(byte) = byte {
                    if top_frame.next_id.is_some() && top_frame.child_index + 1 == idx {
                        child_id = top_frame.next_id;
                    } else {
                        let first_child = node.first_child.unwrap();
                        child_id = Some(self.tree.nth_node(first_child, idx).1);
                    }
                    self.path.push(byte);
                }
            }
        }
        if let Some(child_id) = child_id {
            let top_frame = self.stack.last_mut().unwrap();
            let (node, next_id) = self.tree.get_node(child_id);
            top_frame.child_index = idx;
            top_frame.next_id = Some(next_id);
            self.stack.push(StackFrame::from(&node, child_id));
            self.cur_node = node;
        }
        child_id.is_some()
    }

    /// Descends the zipper's focus one step into the first child branch in a depth-first traversal
    ///
    /// NOTE: This method should have identical behavior to passing `0` to [descend_indexed_byte](ZipperMoving::descend_indexed_byte),
    /// although with less overhead
    fn descend_first_byte(&mut self) -> bool {
        self.descend_indexed_byte(0)
    }

    /// Descends the zipper's focus until a branch or a value is encountered.  Returns `true` if the focus
    /// moved otherwise returns `false`
    fn descend_until(&mut self) -> bool {
        self.trace_pos();
        let mut descended = false;
        'descend: while self.child_count() == 1 {
            let child_id;
            match &self.cur_node {
                Node::Line(line) => {
                    let top_frame = self.stack.last_mut().unwrap();
                    let path = self.tree.get_line(line.path);
                    let rest_path = &path[top_frame.node_depth..];
                    let line_child_hack = if line.child.is_some() { 1 } else { 0 };
                    top_frame.node_depth += rest_path.len() - line_child_hack;
                    self.path.extend_from_slice(rest_path);
                    child_id = line.child;
                    if line.value.is_some() {
                        descended = true;
                        break 'descend;
                    }
                }
                Node::Branch(node) => {
                    let Some(byte) = node.bytemask.iter().next()
                        else { break 'descend };
                    self.path.push(byte);
                    child_id = node.first_child;
                }
            }
            descended = true;
            if let Some(child_id) = child_id {
                let top_frame = self.stack.last_mut().unwrap();
                let (node, next_id) = self.tree.get_node(child_id);
                top_frame.child_index = 0;
                top_frame.next_id = Some(next_id);
                let frame = StackFrame::from(&node, child_id);
                let nchildren = frame.child_count;
                self.stack.push(frame);
                self.cur_node = node.clone();
                if let Node::Branch(node) = node {
                    if node.value.is_some() || nchildren > 1 {
                        break 'descend;
                    }
                }
            }
        }
        descended
    }

    /// Ascends the zipper `steps` steps.  Returns `true` if the zipper sucessfully moved `steps`
    ///
    /// If the root is fewer than `n` steps from the zipper's position, then this method will stop at
    /// the root and return `false`
    fn ascend(&mut self, mut steps: usize) -> bool {
        self.trace_pos();
        if !self.ascend_invalid(Some(&mut steps)) {
            return false;
        }
        while let Some(top_frame) = self.stack.last_mut() {
            let rest_path = &self.path[self.origin_depth..];
            let mut this_steps = steps.min(top_frame.node_depth).min(rest_path.len());
            top_frame.node_depth -= this_steps;
            steps -= this_steps;
            if top_frame.node_depth == 0 && self.stack.len() > 1 && steps > 0 {
                self.stack.pop();
                let prev = self.stack.last().unwrap();
                self.cur_node = self.tree.get_node(prev.node_id).0;
                this_steps += 1;
                steps -= 1;
            }
            self.path.truncate(self.path.len() - this_steps);
            if self.at_root() || steps == 0 {
                return steps == 0 && this_steps > 0;
            }
        }
        unreachable!();
    }

    /// Ascends the zipper up a single byte.  Equivalent to passing `1` to [ascend](Self::ascend)
    fn ascend_byte(&mut self) -> bool {
        self.ascend(1)
    }

    /// Ascends the zipper to the nearest upstream branch point or value.  Returns `true` if the zipper
    /// focus moved upwards, otherwise returns `false` if the zipper was already at the root
    fn ascend_until(&mut self) -> bool {
        self.ascend_to_branch(true)
    }

    /// Ascends the zipper to the nearest upstream branch point, skipping over values along the way.  Returns
    /// `true` if the zipper focus moved upwards, otherwise returns `false` if the zipper was already at the
    /// root
    fn ascend_until_branch(&mut self) -> bool {
        self.ascend_to_branch(false)
    }

    #[inline]
    fn to_next_sibling_byte(&mut self) -> bool {
        self.to_sibling(true)
    }

    #[inline]
    fn to_prev_sibling_byte(&mut self) -> bool {
        self.to_sibling(false)
    }

    // default
    // fn to_next_step(&mut self) -> bool;
}

impl<Storage, Value> ZipperIteration for ACTZipper<'_, Storage, Value>
where Storage: AsRef<[u8]>
{
    /// Systematically advances to the next value accessible from the zipper, traversing in a depth-first
    /// order
    ///
    /// Returns a reference to the value or `None` if the zipper has encountered the root.
    fn to_next_val(&mut self) -> bool {
        while self.to_next_step()  {
            if self.is_val() {
                return true;
            }
        }
        false
    }

    /// Descends the zipper's focus `k`` bytes, following the first child at each branch, and continuing
    /// with depth-first exploration until a path that is `k` bytes from the focus has been found
    ///
    /// Returns `true` if the zipper has sucessfully descended `k` steps, or `false` otherwise.  If this
    /// method returns `false` then the zipper will be in its original position.
    ///
    /// WARNING: This is not a constant-time operation, and may be as bad as `order n` with respect to the paths
    /// below the zipper's focus.  Although a typical cost is `order log n` or better.
    ///
    /// See: [to_next_k_path](ZipperIteration::to_next_k_path)
    fn descend_first_k_path(&mut self, k: usize) -> bool {
        for ii in 0..k {
            if !self.descend_first_byte() {
                self.ascend(ii);
                return false;
            }
        }
        return true;
    }

    /// Moves the zipper's focus to the next location with the same path length as the current focus,
    /// following a depth-first exploration from a common root `k` steps above the current focus
    ///
    /// Returns `true` if the zipper has sucessfully moved to a new location at the same level, or `false`
    /// if no further locations exist.  If this method returns `false` then the zipper will be ascended `k`
    /// steps to the common root.  (The focus position when [descend_first_k_path](ZipperIteration::descend_first_k_path) was called)
    ///
    /// WARNING: This is not a constant-time operation, and may be as bad as `order n` with respect to the paths
    /// below the zipper's focus.  Although a typical cost is `order log n` or better.
    ///
    /// See: [descend_first_k_path](ZipperIteration::descend_first_k_path)
    fn to_next_k_path(&mut self, k: usize) -> bool {
        let mut depth = k;
        'outer: loop {
            while depth > 0 && self.child_count() <= 1 {
                if !self.ascend(1) {
                    break 'outer;
                }
                depth -= 1;
            }
            let stack = self.stack.last_mut().unwrap();
            let idx = stack.child_index + 1;
            if idx >= stack.child_count {
                if depth == 0 || !self.ascend(1) {
                    break 'outer;
                }
                depth -= 1;
                continue 'outer;
            }
            assert!(self.descend_indexed_byte(idx));
            depth += 1;
            for _ii in 0..k - depth {
                if !self.descend_first_byte() {
                    continue 'outer;
                }
                depth += 1;
            }
            return true;
        }
        self.ascend(depth);
        false
    }
}

#[cfg(all(test, feature = "pathmap-internal-tests"))]
mod tests {
    use super::{ArenaCompactTree, ACTZipper};
    use super::{
        morphisms::Catamorphism, PathMap, zipper::{zipper_iteration_tests, zipper_moving_tests, ZipperIteration, ZipperMoving, ZipperValues}
    };

    zipper_moving_tests::zipper_moving_tests!(arena_compact_zipper,
        |keys: &[&[u8]]| {
            let btm = keys.into_iter().map(|k| (k, ())).collect::<PathMap<()>>();
            ArenaCompactTree::from_zipper(btm.read_zipper(), |&_v| 0)
        },
        |trie: &mut ArenaCompactTree<Vec<u8>>, path: &[u8]| -> ACTZipper<'_, Vec<u8>, ()> {
            trie.read_zipper_at_path(path)
        }
    );

    zipper_iteration_tests::zipper_iteration_tests!(arena_compact_zipper,
        |keys: &[&[u8]]| {
            let btm = keys.into_iter().map(|k| (k, ())).collect::<PathMap<()>>();
            ArenaCompactTree::from_zipper(btm.read_zipper(), |&_v| 0)
        },
        |trie: &mut ArenaCompactTree<Vec<u8>>, path: &[u8]| -> ACTZipper<'_, Vec<u8>, ()> {
            trie.read_zipper_at_path(path)
        }
    );

    const PATHS: &[&str] = &[
        "arrow", "bow", "cannon", "roman", "romane", "romanus", "romulus",
        "rubens", "ruber", "rubicon", "rubicundus", "rom'i",
        "aaaaaaaaaaaaaaaaaaaaaaaaaaaaab",
        "aaaaaaaaaaaaaaaaaaaaaaaaaaaaac",
        "bbbbbbbbbbbbbbbbbbbbbbbbbbaaaa",
        "bbbbbbbbbbbbbbbbbbbbbbbbbbcccc",
    ];

    #[test]
    fn test_act_from_zipper() {
        let path_vals = PATHS.iter().enumerate()
            .map(|(idx, path)| (path, idx as u64));

        let btm = PathMap::from_iter(path_vals);
        let act = ArenaCompactTree::from_zipper(btm.read_zipper(), |&v| v);

        let mut btm_zipper = btm.read_zipper();
        let mut act_zipper = act.read_zipper_u64();

        loop {
            btm_zipper.to_next_val();
            act_zipper.to_next_val();

            let btm_val = btm_zipper.val().copied();
            let act_val = act_zipper.val().copied();

            assert_eq!(btm_zipper.path(), act_zipper.path());
            assert_eq!(btm_val, act_val);

            if act_val.is_none() {
                break;
            }
        }
    }

    #[test]
    fn test_act_get() {
        let path_vals = PATHS.iter().enumerate()
            .map(|(idx, path)| (path, idx as u64));
        let btm = PathMap::from_iter(path_vals.clone());
        let act = ArenaCompactTree::from_zipper(btm.read_zipper(), |&v| v);
        for (path, idx) in path_vals {
            assert_eq!(Some(idx), act.get_val_at(path));
        }
    }

    #[test]
    fn test_act_round_trip() {
        let path_vals = PATHS.iter().enumerate()
            .map(|(idx, path)| (path, idx as u64));

        let btm = PathMap::from_iter(path_vals);
        let act1 = ArenaCompactTree::from_zipper(btm.read_zipper(), |&v| v);
        let act2 = ArenaCompactTree::from_zipper(act1.read_zipper_u64(), |&v: &u64| v);
        assert_eq!(act1.get_data(), act2.get_data());
    }

    #[test]
    fn test_act_cata() {
        let path_vals = PATHS.iter().enumerate()
            .map(|(idx, path)| (path, idx as u64));

        let btm = PathMap::from_iter(path_vals);
        let btm_value = btm.read_zipper().into_cata_side_effect(|bm, ch, val, path| {
            let path = std::str::from_utf8(path).unwrap();
            let children = ch.join(", ");
            format!("('{path}' {val:?} {bm:?}\n{children})")
        });
        let act = ArenaCompactTree::from_zipper(btm.read_zipper(), |&v| v);
        let act_value = act.read_zipper_u64().into_cata_side_effect(|bm, ch: &mut[String], val: Option<&u64>, path| {
            let path = std::str::from_utf8(path).unwrap();
            let children = ch.join(", ");
            format!("('{path}' {val:?} {bm:?}\n{children})")
        });
        assert_eq!(btm_value, act_value);
    }

    #[test]
    fn test_act_mmap() -> Result<(), std::io::Error> {
        use tempfile::NamedTempFile;
        use std::io::Write;
        let path_vals = PATHS.iter().enumerate()
            .map(|(idx, path)| (path, idx as u64));

        let btm = PathMap::from_iter(path_vals);
        let act = ArenaCompactTree::from_zipper(btm.read_zipper(), |&v| v);
        let mut tmp = NamedTempFile::new()?;
        tmp.write_all(act.get_data())?;
        let act_mmap = ArenaCompactTree::open_mmap(tmp.path())?;

        let btm_value = btm.read_zipper().into_cata_side_effect(|bm, ch, v, path| {
            let path = std::str::from_utf8(path).unwrap();
            let children = ch.join(", ");
            format!("('{path}' {v:?} {bm:?}\n{children})")
        });
        let act_value = act_mmap.read_zipper_u64().into_cata_side_effect(|bm, ch, val: Option<&u64>, path| {
            let path = std::str::from_utf8(path).unwrap();
            let children = ch.join(", ");
            format!("('{path}' {val:?} {bm:?}\n{children})")
        });
        assert_eq!(btm_value, act_value);
        Ok(())
    }
}
