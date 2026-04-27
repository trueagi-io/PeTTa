#![cfg_attr(rustfmt, rustfmt::skip)]

use std::{any::type_name, hash::Hasher, io::{BufRead, BufReader, BufWriter, Read, Seek, Write}, path::PathBuf};

use crate::pathmap::{morphisms::Catamorphism, PathMap, zipper::{ZipperMoving, ZipperWriting}, TrieValue};
extern crate alloc;
use alloc::collections::BTreeMap;

use crate::gxhash::GxHasher;

macro_rules! hex { () => { b'A'..=b'F' | b'0'..=b'9'}; }

//GOAT, Document this module and clean up this list of desiderata
//
// Serialization requirements:
// Should at least maintain the current sharing
// Serialization should not traverse all paths (i.e. use pointer caching)
// Needn't be finance/security correct
// Somewhat fast to (de)serialize
// Stable across machines
// Instant verification if serialized trees are the same (plus if this is true for subtrees, too)
// Version, val count, total path bytes count, and longest path in meta-data
// I added a few, let me know what you think @Luke Peterson @Remy_Clarke
// Big plus if we have skip-ahead (which allows for search, even better if it allows for partial deserialization) (modifié)

//GOAT TODO to make this a nice public-facing API:
// - We decided this format will be called the "topo_dag" format.  Change function names to reflect that
// - Separate the trie optimization functionality from the format encode functionality, and move trie optimization
//  (aka Merkle Tree optimization) to a separate module that can run independently or called from serialization
// - Figure out if / how we can get the overheads in the encoding in line with the path_serialization, and if
//  we can't (or don't want to), then document why.
// - Make a single file format that encapsulates both the metadata and the serialized data, using separate sections.
// - We should specify a private header that includes a file version.  LP: You have no idea how much time I've
//  lost from my life debugging code when it had loaded an incompatible version of a private file format.
// - Look at a single-pass approach to generate the file, so there is no need for a 2-pass algorithm and a temporary file
// - Eliminate sub-file-names from the publicly exposed API.  e.g. `pub const RAW_HEX_DATA_FILENAME`, etc.
// - Create separate entry points to encode it as either compressed or uncompressed.
// - Abstract away filesystem calls, and implement in terms of std::io traits.  i.e. `std::io::Read`, `std::io::Write`, `std::io::Seek`

#[repr(u8)]
#[derive(Debug, Clone, Copy)]
enum Tag {
  Path       = Tag::PATH,
  Value      = Tag::VALUE,
  ChildMask  = Tag::CHILD_MASK,
  Branches   = Tag::BRANCHES,
  PathNode   = Tag::PATH_NODE,
  ValueNode  = Tag::VALUE_NODE,
  BranchNode = Tag::BRANCH_NODE,
}
impl Tag {
  const PATH        : u8 = b'p';
  const VALUE       : u8 = b'v';
  const CHILD_MASK  : u8 = b'c';
  const BRANCHES    : u8 = b'b';
  const PATH_NODE   : u8 = b'P';
  const VALUE_NODE  : u8 = b'V';
  const BRANCH_NODE : u8 = b'B';
}

const U64_BYTES : usize = u64::BITS as usize /8;

const OFFSET_LEN : usize = U64_BYTES;
// a big endian representation of a line offset.
type Offset = [u8; OFFSET_LEN];

const HEX_OFFSET_LEN : usize = OFFSET_LEN*2+1;
/// an ascii represntation of a [`Offset`]
/// the leading byte of a hex offset is a b'x'
type HexOffset = [u8; HEX_OFFSET_LEN];

pub struct SeOutputs {
  pub raw_data_path               : PathBuf,
  pub zeroes_compressed_data_path : PathBuf,
  pub meta_path                   : PathBuf,
}

/// Filename of the raw hex data file at the `out_dir_path` formal parameter in [`write_trie`] 
pub const RAW_HEX_DATA_FILENAME             : &'static str = "raw_hex.data";
/// Filename of the meta data file at the `out_dir_path` formal parameter in [`write_trie`] 
pub const META_DATA_FILENAME                : &'static str = "meta.json";
/// Filename of the zero compressesed data file at the `out_dir_path` formal parameter in [`write_trie`] 
pub const ZERO_COMPRESSED_HEX_DATA_FILENAME : &'static str = "zero_compressed_hex.data";

pub fn write_trie<C :Catamorphism<V> ,V: TrieValue>(
  memo            : impl AsRef<str>, 
  cata            : C,
  serialize_value : impl for<'read, 'encode> Fn(&'read V, &'encode mut Vec<u8>)->ValueSlice<'read, 'encode>,
  out_dir_path    : impl AsRef<std::path::Path>
) -> Result<SeOutputs,std::io::Error>
{
  core::debug_assert_eq!(
    core::alloc::Layout::new::<Offset>().align(), 
    core::alloc::Layout::new::<u8>().align()
  );

  let mut data_path = out_dir_path.as_ref().to_path_buf();
  data_path.push(RAW_HEX_DATA_FILENAME);

  let mut meta_path = out_dir_path.as_ref().to_path_buf();
  meta_path.push(META_DATA_FILENAME);

  let _ = std::fs::remove_file(&data_path);

  let mut data_file = std::fs::File::create_new(&data_path)?;
  data_file.write_fmt(format_args!("{:?} :: {:?}\n", type_name::<PathMap<V>>(), memo.as_ref()))?;
  let mut meta_file = std::fs::File::create(&meta_path)?;

  let pos = data_file.stream_position()?;


  let mut context = Ctx {
    count               : 0,
    entry              : BTreeMap::new(),
    value_offsets       : Vec::from([pos]),
    data_file           : BufWriter::new(data_file),
    algf_scratch        : AlgFScratch::zeroed(),
    serialize_scratch   : Vec::with_capacity(4096),
    lazy_bytes_pool     : Vec::with_capacity(256), 
  };

  let ctx = &mut context;

  let Accumulator { max_len, val_count, paths_count, ..  } =
  cata.into_cata_jumping_side_effect::<Result<Accumulator, std::io::Error>,_>(
    |bytemask, accumulators, jump_length, maybe_val, origin_path| {
      core::debug_assert!(bytemask.iter().count() == accumulators.len());
      let acc0 = match accumulators { 
        // create nil or branching
        []| [ _, _, ..] => {
                            // reset the scratch buffer
                            ctx.algf_scratch.len = 0;

                            let jump_sub_slice = ctx.lazy_bytes_pool.pop().unwrap_or(Vec::new());

                            // with the exception of the hash, this is the basis of the "nil" node
                            let mut acc = Accumulator 
                            { hash_idx    : (GxHasher::with_seed(0).finish_u128(), ctx.count), // note we are not aumulating on this field, this is just a dummy
                              max_len     : origin_path.len(),
                              val_count   : 0,
                              paths_count : 0,
                              jump_sub_slice
                            };

                            let mut hasher = GxHasher::with_seed(0); // the accumulation for the hash happens here

                            for each in accumulators {
                              let Accumulator { mut hash_idx, max_len, val_count, paths_count, jump_sub_slice } = core::mem::replace(each, Ok(Accumulator::zeroed()))?;


                              '_deal_with_lost_bytes : {
                                hash_idx = maybe_make_path_node(ctx, &jump_sub_slice, hash_idx)?;
                                ctx.lazy_bytes_pool.push(jump_sub_slice);
                              };

                              hasher.write_u128(hash_idx.0);
                              let offset = hash_idx.1.to_be_bytes();
                              let _type_check : Offset = offset;

                              ctx.algf_scratch.buffer[ctx.algf_scratch.len] = offset_to_hex_be(offset);

                              acc.max_len      = acc.max_len.max(max_len);
                              acc.val_count   += val_count;
                              acc.paths_count += paths_count;

                              ctx.algf_scratch.len+=1;
                            }
                            let branches_hash = hasher.finish_u128();
                            // we now have all needed hashes

                            // find correct offset
                            let branches_idx =
                                if let Some(&offset) = ctx.entry.get(&branches_hash) {
                                   offset
                                } else {
                                  // it does not exist we write it to the file
                                  let Ctx { count, entry: values, value_offsets, data_file, algf_scratch, .. } = ctx;

                                  let cur_idx = *count;
                                  data_file.write_all(&[Tag::Branches as u8, b' '])?;
                                  data_file.write_all(algf_scratch.buffer[0..algf_scratch.len].as_flattened())?;
                                  data_file.write_all(&[b'\n'])?;

                                  let new_pos = data_file.stream_position()?;

                                  value_offsets.push(new_pos);

                                  values.insert(branches_hash, cur_idx);
                                  *count += INCR;

                                  cur_idx
                                };

                            let branches_hash_idx = (branches_hash, branches_idx);

                            let child_mask_hash_idx = serialize_with_rollback( ctx,
                                                                               Tag::ChildMask,
                                                                               &mut bytemask.0.map(|word| word.reverse_bits().to_be_bytes()).as_flattened().into_iter().copied(),
                                                                             )?;

                            let hash_idx = write_node(ctx, Tag::BranchNode, child_mask_hash_idx, branches_hash_idx)?;
                            Accumulator {
                              hash_idx,
                              .. acc
                            }
                          }
        // collapse without building a child mask
        [acc_]         => {
                            let byte = bytemask.iter().next().unwrap();
                            let mut acc = std::mem::replace(acc_, Ok(Accumulator::zeroed()))?;
                            acc.jump_sub_slice.insert(0, byte);
                            let hash_idx = maybe_make_path_node(ctx, &acc.jump_sub_slice, acc.hash_idx)?;
                            Accumulator {
                              hash_idx,
                              .. acc
                            }
                          }
      };

      let mut acc1 = match maybe_val {
        // attach value
        Some(value) => { let value =
                             {
                               let mut tmp = core::mem::replace(&mut ctx.serialize_scratch, Vec::new());

                               let slice = serialize_value(value, &mut tmp);
                               let slice = match slice { ValueSlice::Encode(items) => { &*items},
                                                         ValueSlice::Read(items)   => items,
                                                       };
                               let v        = serialize_with_rollback( ctx, Tag::Value,
                                                                        &mut slice.into_iter().copied(),
                                                                      )?;

                               tmp.clear();
                               let _ = core::mem::replace(&mut ctx.serialize_scratch, tmp);

                               v
                             };
                         let cont = acc0;

                         let hash_idx = write_node(ctx, Tag::ValueNode, value, cont.hash_idx)?;

                         Accumulator {
                           hash_idx,
                           val_count  : cont.val_count+1,
                           .. cont
                         }
                       }
        None        => { acc0 }
      };

      acc1.jump_sub_slice.clear();

      if origin_path.len() == jump_length {
        acc1.hash_idx = maybe_make_path_node(ctx, origin_path, acc1.hash_idx)?;
        Ok(acc1)
      } else {
        acc1.jump_sub_slice.extend_from_slice(&origin_path[origin_path.len()-jump_length..origin_path.len()]);
        Ok(acc1)
      }


    }
  )?;

  let Ctx { entry: values, value_offsets, mut data_file, .. } = context;

  data_file.flush()?;
  data_file.rewind()?;

  let compressed = offset_and_childmask_zero_compressor(&data_file.into_inner()?, &out_dir_path)?;

  let mut values_as_vec = values.into_iter().collect::<Vec<_>>();
  values_as_vec.sort_unstable_by(|(_,idx_l), (_,idx_r)| idx_l.cmp(idx_r));

  let values = values_as_vec
    .iter().copied()
    .map( |(h,_)| hash_to_hex_string(h) )
    .collect::<Vec<_>>();

  


  meta_file.write_fmt(format_args!("\
    {{\
    \n  \"PATHS_COUNT\"                    : {paths_count:?},\
    \n  \"MAX_PATH_LEN\"                   : {max_len:?},\
    \n  \"VAL_COUNT\"                      : {val_count:?},\
    \n  \"RAW_FILE_OFFSETS\"               : {value_offsets:?},\
    \n  \"ZEROES_COMPRESSED_FILE_OFFSETS\" : {zeroes_offsets:?},\
    \n  \"HASHES\"                         : {values:?}\
    \n}}
    \n",
    zeroes_offsets=compressed.offsets
  ))?;

  meta_file.flush()?;





  Ok(SeOutputs {
    raw_data_path : data_path,
    zeroes_compressed_data_path : compressed.path,
    meta_path,
  })
}



fn rollback_or_advance (
    context      : &mut Ctx,
    hash         : u128, 
    rollback_pos : FilePos
) -> Result<(u128, Index), std::io::Error> 
{
  Ok(
    if let Some(&offset) = context.entry.get(&hash) {
      // rollback and use lookup value

      let cur_pos = context.data_file.stream_position()?;
      context.data_file.seek_relative(rollback_pos as i64 - cur_pos as i64)?;
      (hash, offset)
    } else {
      // advance

      let idx = context.count;
      context.entry.insert(hash, idx);
      context.count += INCR;

      context.value_offsets.push(context.data_file.stream_position()?);
      (hash, idx)
    }
  )
}

fn serialize_with_rollback (
    context : &mut Ctx, 
    tag     : Tag, 
    i       : &mut dyn Iterator<Item = u8>
) -> Result<(u128, Index), std::io::Error>
{
  let rollback_pos = *context.value_offsets.last().unwrap();
  let mut hasher = GxHasher::with_seed(0);


  hasher.write_u8(tag as u8);
  context.data_file.write_all(&[tag as u8, b' '])?;
  for b in i {
    hasher.write_u8(b);
    context.data_file.write_all(&byte_to_hex_pair_be(b))?;
  }
  let hash = hasher.finish_u128();
  context.data_file.write_all(&[b'\n'])?;

  rollback_or_advance(context, hash, rollback_pos)
}

// new_hash === seed(0) -> write_u8(Tag) -> write_u128(Hash) -> write_u128(Hash)
fn write_node(
    context                 : &mut Ctx,
    tag                     : Tag,
    (value_hash, value_idx) : (u128, Index),
    (cont_hash, cont_idx)   : (u128, Index)
) -> Result<(u128, Index), std::io::Error>
{
  debug_assert_eq!( context.entry.get(&value_hash), Some(&value_idx) );
  debug_assert_eq!( context.entry.get(&cont_hash),  Some(&cont_idx)  );

  let mut hasher = GxHasher::with_seed(0);
  hasher.write_u8(tag as u8);
  hasher.write_u128(value_hash);
  hasher.write_u128(cont_hash);
  let hash = hasher.finish_u128();

  match context.entry.get(&hash) {
    Some(&offset) => Ok((hash, offset)),
    None          => {
                       let cur_idx = context.count;
                       let v_offset = value_idx.to_be_bytes();

                       let c_offset = cont_idx.to_be_bytes();
                       let _type_check : [Offset; 2] = [v_offset, c_offset];

context.data_file.write_all(&[tag as u8, b' '])?;
                        context.data_file.write_all(&offset_to_hex_be( v_offset ))?;
                        context.data_file.write_all(&offset_to_hex_be( c_offset ))?;
                        context.data_file.write_all(&[b'\n'])?;

                       context.entry.insert(hash, cur_idx);
                       context.count += INCR;

                       let new_pos = context.data_file.stream_position()?;
                       context.value_offsets.push(new_pos);
                       Ok((hash, cur_idx))
                     }
  }
}

/// makes a path node if the sub_path supplied is non-empty, otherwise returns the passed in `cont_hash_idx`
fn maybe_make_path_node(
    ctx      : &mut Ctx, 
    sub_path : &[u8], 
    cont_hash_idx : (u128, usize)
) -> Result<(u128, usize), std::io::Error>
{
  if sub_path.is_empty() {
    // don't make a new node
    return Ok(cont_hash_idx);
  }
  let p = serialize_with_rollback( ctx,
                                   Tag::Path,
                                   &mut sub_path.into_iter().copied(),
                                 )?;
  write_node(ctx, Tag::PathNode, p, cont_hash_idx)
}

/// helper function for metadata output
fn hash_to_hex_string(h : u128)->String 
{
  h.to_be_bytes()
    .map(byte_to_hex_pair_be)
    .into_iter()
    .fold(String::new(), |mut acc, [t,b]| { acc.push(t as char); acc.push(b as char); acc})
}

// this being true means we need to add the nil hash to the map
#[cfg(all(test, feature = "pathmap-internal-tests"))]#[test] fn gxhash_finish_zero_is_zero() { core::assert!(GxHasher::with_seed(0).finish_u128() != 0) }

type ChildMask = [u64;4];

/// the position of the cursor in a file
type FilePos = u64;
/// a new Index is generated when the count of Ctx increments
type Index = usize;
struct Ctx{
  /// the number of entries into the file
  count             : usize,
  /// entries are hashed as the are serialized, we do this to track sharing, shared entries need to rollbacked if they had been written
  entry             : BTreeMap<u128, Index>,
  /// this is used for "raw_hex_meta.data"
  value_offsets     : Vec<FilePos>,
  /// the writer to the "raw_hex.data"
  data_file         : BufWriter<std::fs::File>,
  algf_scratch      : AlgFScratch,
  serialize_scratch : Vec<u8>,

  /// we need to keep some bytes from sub paths on jumps, and lazily use them on the next step to avoid making single byte paths for singleton paths with values
  lazy_bytes_pool   : Vec<Vec<u8>>,
}

struct AlgFScratch {
  buffer : [ HexOffset; 256],
  len    : usize,
}
impl  AlgFScratch {
  const fn zeroed() -> Self { AlgFScratch { buffer: [[0; HEX_OFFSET_LEN];256], len: 0 } }
}

struct Accumulator {
  /// The core of the accumulator, the hash is the "semantically identity", the index the "arbitrary identity". 
  /// We use the arbitrary identity for the file entries, and the hashes for calculating sharing.
  hash_idx       : (u128, Index),
  /// to avoid making singleton branching entries, we thread the jump path to the next join point when needed, and make the path then
  jump_sub_slice : Vec<u8>,
  
  /// only accumulated for metadata
  max_len     : usize,
  /// only accumulated for metadata
  val_count   : usize,
  /// only accumulated for metadata
  paths_count : usize,
}

impl Accumulator {
  const fn zeroed() -> Self {
    Accumulator { 
      hash_idx       : (0,0),
      max_len        : 0,
      val_count      : 0,
      paths_count    : 0,
      jump_sub_slice : Vec::new(),
    }}
}

/// turns a big endian ordered pair of ascii hex bytes in to a byte
fn hex_pair_be_to_byte(pair : [u8;2])->u8{
  core::debug_assert!(matches!(pair, [hex!(),hex!()]));

  let [top,bot] = pair.map(|h|
    match h {
      b'0'..=b'9' => h - b'0',
      b'A'..=b'F' => h - b'A' + 10,
      _ => panic!("found {h}, as char '{}'", h as char)
    }
  );
  (top << 4) | bot
}

/// turns a byte into a big endian ordered pair of ascii hex bytes
fn byte_to_hex_pair_be(b : u8) -> [u8;2] {
  let top = b >> 4;
  let bot = b & 0x_f;
  let unchecked_to_hex = |b : u8| {
    match b {
      0..=9   => b + b'0',
      10..=16 => b + b'A' - 10 ,
      _ => panic!("found {b}, as char '{}'", b as char)
    }
  };
  [top, bot].map(unchecked_to_hex)
}

#[cfg(all(test, feature = "pathmap-internal-tests"))] #[test]
fn byte_to_hex_inverses(){ for each in 0..u8::MAX { core::assert_eq!(each, hex_pair_be_to_byte(byte_to_hex_pair_be(each))) } }

/// converts an in memory numberic offset into a ascii hex 
fn offset_to_hex_be(bytes : Offset) -> HexOffset {
  unsafe { 
    let mut hex = core::mem::transmute::<_,[u8;OFFSET_LEN*2]>( bytes.map( byte_to_hex_pair_be ) );
    let mut out = [0; HEX_OFFSET_LEN];
    out[0] = b'x';
    (&mut out[1..]).copy_from_slice(&mut hex);
    out
  }
}


// The datatype that describes what will be sent to the serializer
pub enum ValueSlice<'read, 'encode> {
  /// if a value can transparently reveal a slice of bytes that represents enough data to serialize the data this variant can be used
  Read(&'read [u8]),
  /// if the value cannot be trivially read as bytes, one can encode it into the mutable buffer
  Encode(&'encode mut Vec<u8>),
}

/// this constant exists purely for debugging compression of zeros
#[cfg(not(debug_assertions))]
const INCR : usize = 0x_1;
#[cfg(debug_assertions)]
const INCR : usize = 0x_1;
// const INCR : usize = 0x_100000;

struct ZeroCompressedFile {
  path : PathBuf,
  offsets : Vec<FilePos>
}


fn offset_and_childmask_zero_compressor(
    f : &std::fs::File, 
    out_dir_path: impl AsRef<std::path::Path>
) -> Result<ZeroCompressedFile, std::io::Error> {

  let mut path = out_dir_path.as_ref().to_path_buf();
  path.push(ZERO_COMPRESSED_HEX_DATA_FILENAME);
  let out_file = std::fs::File::create(&path)?;
  let mut out  = BufWriter::new(out_file);

  let mut offsets : Vec<FilePos> = Vec::new();


  let reader = BufReader::new(f);
  let mut bytes = reader.bytes();
  
  // get past header
  while let Some(byte) = bytes.next() {
    let x = byte?;
    out.write_all(&[x])?;
    if x == b'\n' {break}
  }
  offsets.push(out.stream_position()?);


  let write_zeroes = |z : &mut _, file : &mut BufWriter<_>, boundary : bool| -> Result<(), std::io::Error>{
    let dummy = [b'0'; 4];
    let mut n = *z;
    *z = 0;
    match n {
      0..=3 => {
        file.write_all(&dummy[0..n])?;
      }
      4..=64 => { 
                  if !boundary {
                    n -= 1;
                  }
                  let hex = byte_to_hex_pair_be((n) as u8);
                  file.write_all(&[b'/', hex[0], hex[1],])?;
                  if !boundary {
                    file.write_all(&[b'0'])?;
                  }
                },
      _       => core::unreachable!(),
    }
    Ok(())
  };
  
  let mut byte_boundary = true;
  let     zeroes        = &mut 0;
  let mut skip          = false;
  let mut hit_x         = false;

  while let Some(each) = bytes.next() {
    byte_boundary = !byte_boundary;
    let ascii = each?;
    core::debug_assert!(ascii.is_ascii_alphanumeric() || ascii.is_ascii_whitespace());

    match ascii {
      b'v'
    | b'p'  => { skip = true;
                 out.write_all(&[ascii])?;
               },
      b'\n' => {
                 skip = false;
                 if *zeroes > 0 && !hit_x {
                   write_zeroes(zeroes, &mut out, byte_boundary)?;
                 }
                 if hit_x {
                   // only happens once for "nil"
                   out.write_all(b"00")?;
                 }
                 hit_x = false;
                 byte_boundary = false;
                 out.write_all(b"\n")?;
                 offsets.push(out.stream_position()?);
               }
      b'0'  => {
                 if hit_x {
                   debug_assert!(*zeroes == 0);
                   continue;
                 }
                 if skip {
                   core::debug_assert!(*zeroes == 0);
                   out.write_all(b"0")?;
                 } else if !byte_boundary && *zeroes == 0 {
                   out.write_all(b"0")?;
                   hit_x = false;
                 } else {
                   *zeroes += 1;
                 }

               }
      b'x'  => {
                 if *zeroes > 0 {
                   core::debug_assert!(!hit_x);
                   write_zeroes(zeroes, &mut out, byte_boundary)?;
                 }
                if hit_x {panic!()}
                 out.write_all(b"x")?;
                 byte_boundary = false;
                 hit_x = true;
                 *zeroes = 0;
               }
      _     => {
                 if *zeroes > 0 {
                   core::debug_assert!(!hit_x);
                   write_zeroes(zeroes, &mut out, byte_boundary)?;
                 }
                 if hit_x && !byte_boundary {
                   out.write_all(b"0")?;
                 }
                 out.write_all(&[ascii])?;
                 hit_x = false
               }
    };
  }
  out.flush()?;

  Ok(
    ZeroCompressedFile { 
      path,
      offsets
    }
  )
}


// ///////////////////
// DESERIALIZATION //
// /////////////////

/// deserialize the serialized pathmap
pub fn deserialize_file<V: TrieValue>(file_path : impl AsRef<std::path::Path>, de : impl Fn(&[u8])->V)-> Result<PathMap<V>, std::io::Error> {
  let f = std::fs::File::open(file_path.as_ref())?;
  let mut reader = BufReader::new(f);

  let mut line = String::with_capacity(4096);

  // strip header
  reader.read_line(&mut line)?;
  line.clear();

  // ~ 1 gigabyte virtual allocation to start
  let mut paths_buffer = Vec::with_capacity(2_usize.pow(30));
  let mut branches_buffer = Vec::with_capacity(2_usize.pow(30)/U64_BYTES);
  
  // we pay the price of looking at a tag, but it should pay off as we get constant lookup
  enum Deserialized<V: Clone + Send + Sync> {
    Path(std::ops::Range<usize>),
    Value(V),
    ChildMask(ChildMask),
    Branches(std::ops::Range<usize>),
    Node(PathMap<V>),
  }

  #[cfg(debug_assertions)]
  impl<V: TrieValue> core::fmt::Debug for Deserialized<V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      match self {
        Deserialized::Path(range)           => write!(f,"Path({}..{})", range.start, range.end),
        Deserialized::Value(_)              => write!(f,"Value"),
        Deserialized::ChildMask(mask)       => {
                                                  let s = mask.into_iter().flat_map(|n|format!("{:.>64b} ",n.reverse_bits()).chars().collect::<Vec<_>>()).collect::<String>();
                                                  write!(f,"ChildMask({:?})",s)
                                               },
        Deserialized::Branches(range)       => write!(f,"Branches({}..{})", range.start, range.end),
        Deserialized::Node(bytes_trie_map)  => write!(f,"Node(empty?{})",bytes_trie_map.is_empty()),
      }
    }
  }

  let mut deserialized = Vec::with_capacity(4096);
  let mut val_scratch = Vec::with_capacity(4096);

  while let Ok(_) = reader.read_line(&mut line) {
    let [bytes @ .. , b'\n'] = line.as_bytes() else {break};
    let [  t @ ( Tag::PATH 
               | Tag::VALUE 
               | Tag::CHILD_MASK 
               | Tag::BRANCHES
               | Tag::PATH_NODE
               | Tag::VALUE_NODE
               | Tag::BRANCH_NODE
               ),
          b' ',data @ ..] = bytes else { return Err(std::io::Error::other("Malformed serialized ByteTrie, expected `<tag byte><space>`")); };

  // // these are for debugging
  // println!("0x{:_>16x} {} {:?}\n{:#?}", deserialized.len(), *t as char, std::str::from_utf8(data), deserialized);
  // println!("0x{:_>16x} {} {:?}", deserialized.len(), *t as char, std::str::from_utf8(data));



    match *t {
      // paths currently have no compression yet
      Tag::PATH         => { 
                             let start = paths_buffer.len();

                             let mut cur = data;
                             loop {
                               match cur {
                                []                   => break,
                                [top,bot, rest @ ..] => {
                                                          let b = hex_pair_be_to_byte([*top,*bot]);
                                                          paths_buffer.push(b);
                                                          cur = rest
                                                        }
                                _                    => return Err(std::io::Error::other("Malformed serialized ByteTrie, expected path as `(<hex_top><Hex_bot>)*`"))
                               }
                             }

                             let end = paths_buffer.len();


                             deserialized.push(Deserialized::Path(start..end));
                           }
      // values currently have no compression yet
      Tag::VALUE        => { 
                             val_scratch.clear();

                             let mut cur = data;

                             loop {
                               match cur {
                                []                   => break,
                                [top,bot, rest @ ..] => {
                                                          let b = hex_pair_be_to_byte([*top,*bot]);
                                                          val_scratch.push(b);
                                                          cur = rest
                                                        }
                                _                    => { 
                                                          return Err(std::io::Error::other("Malformed serialized ByteTrie, expected value as `(<hex_top><Hex_bot>)*`"))
                                                        }
                               }
                             }

                             let value = de(&val_scratch);

                             val_scratch.clear();

                             deserialized.push(Deserialized::Value(value));
                           }

      // child mask has compression but no b'x' bytes
      Tag::CHILD_MASK   => { 
                             let mask_buf = decompress_zeros_compression_child_mask(data)?;
                             let mask = mask_buf.map(u64::from_be_bytes).map(u64::reverse_bits);

                            deserialized.push(Deserialized::ChildMask(mask));
                           }

      // the rest have ofset compression with b'x' bytes
      Tag::BRANCHES     => { 
                             let mut children_buf = [0_u64 ; 256];
                             let x_count      = decompress_zeros_compression_offset(data, &mut children_buf)?;

                            let start = branches_buffer.len();
                            branches_buffer.extend_from_slice(&children_buf[0..x_count]);
                            let end   = branches_buffer.len();

                            deserialized.push(Deserialized::Branches(start..end));

                           }
      Tag::PATH_NODE    => { let mut node_buf     = [0_u64 ; 2];
                             decompress_zeros_compression_offset(data, &mut node_buf)?;

                             let [path_idx, node_idx] = node_buf.map(|x| x as usize);

                             let Deserialized::Path(path) = &deserialized[path_idx] else { return Err(std::io::Error::other("Malformed serialized ByteTrie, expected path")); };
                             let Deserialized::Node(node) = &deserialized[node_idx] else { return Err(std::io::Error::other("Malformed serialized ByteTrie, expected node")); };

                             let mut path_node = PathMap::new();

                             let mut wz        = path_node.write_zipper();
                             wz.descend_to(&paths_buffer[path.start..path.end]);
                             wz.graft(&node.read_zipper());
                             drop(wz);

                             core::debug_assert!(!path_node.is_empty());

                             deserialized.push(Deserialized::Node(path_node));

                           }
      Tag::VALUE_NODE   => { let mut node_buf     = [0_u64 ; 2];

                             decompress_zeros_compression_offset(data, &mut node_buf)?;

                             let [val_idx, node_idx] = node_buf.map(|x| x as usize);

                             let Deserialized::Value(value) = &deserialized[val_idx]  else { return Err(std::io::Error::other("Malformed serialized ByteTrie, expected value")); };
                             let Deserialized::Node(node)   = &deserialized[node_idx] else { return Err(std::io::Error::other("Malformed serialized ByteTrie, expected node")); };

                             let mut value_node = node.clone();
                             value_node.set_val_at(&[], value.clone());

                             deserialized.push(Deserialized::Node(value_node));
                           } 

      Tag::BRANCH_NODE  => { let mut node_buf     = [0_u64 ; 2];
                             decompress_zeros_compression_offset(data, &mut node_buf)?;

                             let [mask_idx, branches_idx] = node_buf.map(|x| x as usize);

                             let Deserialized::ChildMask(mask) = &deserialized[mask_idx] else { return Err(std::io::Error::other("Malformed serialized ByteTrie, expected childmask as `(/?<hex_top><Hex_bot>)*`")); };
                             let iter = crate::pathmap::utils::ByteMaskIter::new(*mask);

                             let Deserialized::Branches(r) = &deserialized[branches_idx] else { return Err(std::io::Error::other("Malformed serialized ByteTrie, expected branches")); };
                             let branches = &branches_buffer[r.start..r.end];

                             core::debug_assert_eq!(mask.into_iter().copied().map(u64::count_ones).sum::<u32>() as usize, branches.len());

                             let mut branch_node = PathMap::new();
                             let mut wz = branch_node.write_zipper();

                             for (byte, &idx) in iter.into_iter().zip(branches) {
                               let Deserialized::Node(node) = &deserialized[idx as usize] else { return Err(std::io::Error::other("Malformed serialized ByteTrie, expected node")); };

                               core::debug_assert!(!node.is_empty());

                               wz.descend_to_byte(byte);
                               wz.graft(&node.read_zipper());
                               wz.ascend_byte();
                             }

                             drop(wz);


                             deserialized.push(Deserialized::Node(branch_node));
                           }

      _ => core::unreachable!()
    }

    line.clear();
  }

  let Some(Deserialized::Node(n)) = deserialized.pop() else { return Err(std::io::Error::other("Malformed serialized ByteTrie, expected root node")); };

  Ok(n)
}


// zeroes the buffer before decompressing
fn decompress_zeros_compression_offset(mut encoded_hex : &[u8], buffer : &mut [u64])->Result<usize, std::io::Error> {

  for each in buffer.iter_mut() {
    *each = 0;
  }

  let mut count = 0;
  let mut x = false;

  loop {
    match encoded_hex {
      []                                             => { 
                                                          if x { count +=1; }
                                                          break
                                                        }
      [b'x',                             rest @ .. ] => {
                                                          if x { count += 1; }
                                                          x = true;
                                                          encoded_hex = rest;
                                                        }
      [b'/', top @ hex!(), bot @ hex!(), rest @ .. ] => { let hex_zeros = hex_pair_be_to_byte([*top,*bot]);

                                                          // whole bytes
                                                          let zeroes = hex_zeros/2;

                                                          debug_assert!(zeroes <= 6 );
                                                          buffer[count] <<= zeroes * u8::BITS as u8;

                                                          encoded_hex = rest;
                                                        }
      [      top @ hex!(), bot @ hex!(), rest @ .. ] => { let b = hex_pair_be_to_byte([*top,*bot]);

                                                          buffer[count] <<= u8::BITS;
                                                          buffer[count] |= b as u64;

                                                          encoded_hex = rest;
                                                        }
      _                                              => { return Err(std::io::Error::other("Malformed serialized ByteTrie")); }

    }
  }

  #[cfg(debug_assertions)]
  if INCR != 1 {
    for each in buffer.iter_mut() {
      *each >>= INCR.trailing_zeros();
    }
  }

  Ok(count)
}

fn decompress_zeros_compression_child_mask(mut encoded_hex : &[u8], )->Result<[[u8;U64_BYTES];4], std::io::Error> {
  let mut buffer = [[0_u8;U64_BYTES];4];

  let mut bytes = 0;
  let mut count = 0;

  loop {
    match encoded_hex {
      []                                             => { 
                                                          break
                                                        }
      [b'/', top @ hex!(), bot @ hex!(), rest @ .. ] => { let hex_zeros = hex_pair_be_to_byte([*top,*bot]);

                                                          // whole bytes
                                                          let zeroes = hex_zeros/2;

                                                          let total :usize = bytes + zeroes as usize + count * U64_BYTES;
                                                          (count,bytes) = (total / U64_BYTES, total % U64_BYTES);
                                                          encoded_hex = rest;
                                                        }
      [      top @ hex!(), bot @ hex!(), rest @ .. ] => { let b = hex_pair_be_to_byte([*top,*bot]);

                                                          buffer[count][bytes] = b;

                                                          bytes += 1;
                                                          count += bytes as usize / U64_BYTES;
                                                          bytes %= U64_BYTES;

                                                          encoded_hex = rest;
                                                        }
      _                                              => { return Err(std::io::Error::other("Malformed serialized ByteTrie, childmask")); }

    }
  }
  Ok(buffer)
}

#[cfg(all(test, feature = "pathmap-internal-tests"))]
mod test {
  use super::*;
  use std::sync::Arc;

  #[test]
  fn serialization_trivial_test() {
    const LEN : usize = 0x_80;
    // const LEN : usize = 0x_02;
    #[allow(long_running_const_eval)]
    const ARR : [u8; LEN]= {
      let mut arr = [0_u8 ; LEN];
      let mut i = 0;
      while i != LEN {
        arr[i] = i as u8;
        i += 1;
      }
      arr
    };

    let mut trie = PathMap::<Arc<[u8]>>::new();

    let as_arc = |bs : &[u8]| alloc::sync::Arc::<[u8]>::from(bs);
    trie.set_val_at(b"a", as_arc(b""));
    trie.set_val_at(b"abc", as_arc(b""));

    trie.set_val_at(b"ab", as_arc(b""));
    trie.set_val_at(b"abcd", as_arc(b""));


    trie.set_val_at(b"ab", as_arc(b""));
    trie.set_val_at(b"abc", as_arc(b""));
    trie.set_val_at(b"abd", as_arc(b""));



    trie.set_val_at(b"", as_arc(b""));
    trie.set_val_at(b"ab", as_arc(b""));

    trie.set_val_at(b"abce", as_arc(b""));
    trie.set_val_at(b"abcf", as_arc(b""));


    trie.set_val_at(b"", as_arc(b""));
    trie.set_val_at(b"a", as_arc(b""));
    trie.set_val_at(b"b", as_arc(b""));   
    trie.set_val_at(b"axb", as_arc(b""));  
    trie.set_val_at(b"axbxc", as_arc(b"")); 
    trie.set_val_at(b"axbxd", as_arc(b"")); 
    trie.set_val_at(b"axc", as_arc(b"")); 
    trie.set_val_at(b"axb", as_arc(b"")); 
    trie.set_val_at(b"axbf", as_arc(b"")); 
    trie.set_val_at(b"axbe", as_arc(b"")); 
    trie.set_val_at(b"axbexy", as_arc(b"")); 
    trie.set_val_at(b"axbexyb", as_arc(b"")); 
    trie.set_val_at(b"axbxxxxxxxxxxxxxxxxxxd", as_arc(b"")); 
    trie.set_val_at(b"axbxexz", as_arc(b"")); 
    trie.set_val_at(b"axbexyzccca", as_arc(b"")); 
    trie.set_val_at(b"axbexyz", as_arc(b"")); 

    trie.set_val_at(b"", as_arc(b"abc"));



    trie.set_val_at(b"a", as_arc(b""));
    trie.set_val_at(b"ab", as_arc(b""));
    trie.set_val_at(b"b", as_arc(b""));


    trie.set_val_at(b"desf", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"desF", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"desG", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"desH", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"desI", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"desJ", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"desK", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"desM", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"abesf", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"abesF", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"abesG", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"abesH", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"abesI", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"abesJ", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"abesK", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"abesM", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"abcd", as_arc(b"xyz"));
    trie.set_val_at(b"abce", as_arc(b"xyz"));
    trie.set_val_at(b"abcf", as_arc(b"xyz"));
    trie.set_val_at(b"abcg", as_arc(b"xyz"));
    trie.set_val_at(b"agbcd", as_arc(b"xyz"));
    trie.set_val_at(b"agbce", as_arc(b"xyz"));
    trie.set_val_at(b"agbcf", as_arc(b"xyz"));
    trie.set_val_at(b"agbcg", as_arc(b"xyz"));


    trie.set_val_at(b"dxxxesf", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"dxxxesF", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"dxxxesG", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"dxxxesH", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"dxxxesI", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"dxxxesJ", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"dxxxesK", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"dxxxesM", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"axxxbesf", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"axxxbesF", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"axxxbesG", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"axxxbesH", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"axxxbesI", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"axxxbesJ", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"axxxbesK", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"axxxbesM", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"axxxbcd", as_arc(b"xyz"));
    trie.set_val_at(b"axxxbce", as_arc(b"xyz"));
    trie.set_val_at(b"axxxbcf", as_arc(b"xyz"));
    trie.set_val_at(b"axxxbcg", as_arc(b"xyz"));
    trie.set_val_at(b"axxxgbcd", as_arc(b"xyz"));
    trie.set_val_at(b"axxxgbce", as_arc(b"xyz"));
    trie.set_val_at(b"axxxgbcf", as_arc(b"xyz"));
    trie.set_val_at(b"axxxgbcg", as_arc(b"xyz"));


    trie.set_val_at(b"123dzxesf", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"123dzxesF", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"123dzxesG", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"123dzxesH", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"123dzxesI", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"123dzxesJ", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"123dzxesK", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"123dzxesM", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"123azxbesf", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"123azxbesF", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"123azxbesG", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"123azxbesH", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"123azxbesI", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"123azxbesJ", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"123azxbesK", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"123azxbesM", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"123azxbcd", as_arc(b"xyz"));
    trie.set_val_at(b"123azxbce", as_arc(b"xyz"));
    trie.set_val_at(b"123azxbcf", as_arc(b"xyz"));
    trie.set_val_at(b"123azxbcg", as_arc(b"xyz"));
    trie.set_val_at(b"123azxgbcd", as_arc(b"xyz"));
    trie.set_val_at(b"123azxgbce", as_arc(b"xyz"));
    trie.set_val_at(b"123azxgbcf", as_arc(b"xyz"));
    trie.set_val_at(b"123azxgbcg", as_arc(b"xyz"));





    trie.set_val_at(b"", as_arc(b""));
    trie.set_val_at(b"ab", as_arc(b""));
    trie.set_val_at(b"abce", as_arc(b""));
    trie.set_val_at(b"abcf", as_arc(b""));


    trie.set_val_at(b"", as_arc(b""));
    trie.set_val_at(b"a", as_arc(b""));  
    trie.set_val_at(b"b", as_arc(b""));  
    trie.set_val_at(b"axb", as_arc(b"")); 
    trie.set_val_at(b"axbxc", as_arc(b""));
    trie.set_val_at(b"axbxd", as_arc(b""));
    trie.set_val_at(b"axc", as_arc(b""));
    trie.set_val_at(b"axb", as_arc(b""));
    trie.set_val_at(b"axbf", as_arc(b""));
    trie.set_val_at(b"axbe", as_arc(b""));
    trie.set_val_at(b"axbexy", as_arc(b""));
    trie.set_val_at(b"axbexyb", as_arc(b""));
    trie.set_val_at(b"axbxxxxxxxxxxxxxxxxxxd", as_arc(b""));
    trie.set_val_at(b"axbxexz", as_arc(b""));
    trie.set_val_at(b"axbexyzccca", as_arc(b""));
    trie.set_val_at(b"axbexyz", as_arc(b""));

    trie.set_val_at(b"a", as_arc(b""));
    trie.set_val_at(b"ab", as_arc(b""));
    trie.set_val_at(b"b", as_arc(b""));


    trie.set_val_at(b"57desf", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57desF", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57desG", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57desH", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57desI", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57desJ", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57desK", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57desM", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57abesf", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57abesF", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57abesG", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57abesH", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57abesI", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"57abesJ", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"57abesK", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"57abesM", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"57abcd", as_arc(b"xyz"));
    trie.set_val_at(b"57abce", as_arc(b"xyz"));
    trie.set_val_at(b"57abcf", as_arc(b"xyz"));
    trie.set_val_at(b"57abcg", as_arc(b"xyz"));
    trie.set_val_at(b"57agbcd", as_arc(b"xyz"));
    trie.set_val_at(b"57agbce", as_arc(b"xyz"));
    trie.set_val_at(b"57agbcf", as_arc(b"xyz"));
    trie.set_val_at(b"57agbcg", as_arc(b"xyz"));
    trie.set_val_at(b"57dxxxesf", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57dxxxesF", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57dxxxesG", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57dxxxesH", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57dxxxesI", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57dxxxesJ", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57dxxxesK", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57dxxxesM", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57axxxbesf", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57axxxbesF", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57axxxbesG", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57axxxbesH", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57axxxbesI", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"57axxxbesJ", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"57axxxbesK", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"57axxxbesM", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"57axxxbcd", as_arc(b"xyz"));
    trie.set_val_at(b"57axxxbce", as_arc(b"xyz"));
    trie.set_val_at(b"57axxxbcf", as_arc(b"xyz"));
    trie.set_val_at(b"57axxxbcg", as_arc(b"xyz"));
    trie.set_val_at(b"57axxxgbcd", as_arc(b"xyz"));
    trie.set_val_at(b"57axxxgbce", as_arc(b"xyz"));
    trie.set_val_at(b"57axxxgbcf", as_arc(b"xyz"));
    trie.set_val_at(b"57axxxgbcg", as_arc(b"xyz"));
    trie.set_val_at(b"57123dzxesf", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57123dzxesF", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57123dzxesG", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57123dzxesH", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57123dzxesI", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57123dzxesJ", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57123dzxesK", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57123dzxesM", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57123azxbesf", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57123azxbesF", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57123azxbesG", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57123azxbesH", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57123azxbesI", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"57123azxbesJ", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"57123azxbesK", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"57123azxbesM", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"57123azxbcd", as_arc(b"xyz"));
    trie.set_val_at(b"57123azxbce", as_arc(b"xyz"));
    trie.set_val_at(b"57123azxbcf", as_arc(b"xyz"));
    trie.set_val_at(b"57123azxbcg", as_arc(b"xyz"));
    trie.set_val_at(b"57123azxgbcd", as_arc(b"xyz"));
    trie.set_val_at(b"57123azxgbce", as_arc(b"xyz"));
    trie.set_val_at(b"57123azxgbcf", as_arc(b"xyz"));
    trie.set_val_at(b"57123azxgbcg", as_arc(b"xyz"));




    trie.set_val_at(b"fgsfdgsfdgfds57desf", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdgsfdgfds57desF", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdgsfdgfds57desG", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdgsfdgfds57desH", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdgsfdgfds57desI", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdgsfdgfds57desJ", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdgsfdgfds57desK", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdgsfdgfds57desM", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdgsfdgfds57abesf", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdgsfdgfds57abesF", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdgsfdgfds57abesG", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdgsfdgfds57abesH", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdgsfdgfds57abesI", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"fgsfdgsfdgfds57abesJ", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"fgsfdgsfdgfds57abesK", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"fgsfdgsfdgfds57abesM", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"fgsfdgsfdgfds57abcd", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdgsfdgfds57abce", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdgsfdgfds57abcf", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdgsfdgfds57abcg", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdgsfdgfds57agbcd", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdgsfdgfds57agbce", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdgsfdgfds57agbcf", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdgsfdgfds57agbcg", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdgsfdgfds57dxxxesf", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdgsfdgfds57dxxxesF", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdgsfdgfds57dxxxesG", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdgsfdgfds57dxxxesH", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdgsfdgfds57dxxxesI", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdgsfdgfds57dxxxesJ", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdgsfdgfds57dxxxesK", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdgsfdgfds57dxxxesM", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdgsfdgfds57axxxbesf", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdgsfdgfds57axxxbesF", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdgsfdgfds57axxxbesG", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdgsfdgfds57axxxbesH", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdgsfdgfds57axxxbesI", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"fgsfdgsfdgfds57axxxbesJ", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"fgsfdgsfdgfds57axxxbesK", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"fgsfdgsfdgfds57axxxbesM", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"fgsfdgsfdgfds57axxxbcd", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdgsfdgfds57axxxbce", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdgsfdgfds57axxxbcf", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdgsfdgfds57axxxbcg", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdgsfdgfds57axxxgbcd", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdgsfdgfds57axxxgbce", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdgsfdgfds57axxxgbcf", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdgsfdgfds57axxxgbcg", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdgsfdgfds57123dzxesf", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdgsfdgfds57123dzxesF", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdgsfdgfds57123dzxesG", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdgsfdgfds57123dzxesH", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdgsfdgfds57123dzxesI", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdgsfdgfds57123dzxesJ", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdgsfdgfds57123dzxesK", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdgsfdgfds57123dzxesM", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdgsfdgfds57123azxbesf", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdgsfdgfds57123azxbesF", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdgsfdgfds57123azxbesG", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdgsfdgfds57123azxbesH", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdgsfdgfds57123azxbesI", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"fgsfdgsfdgfds57123azxbesJ", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"fgsfdgsfdgfds57123azxbesK", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"fgsfdgsfdgfds57123azxbesM", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"fgsfdgsfdgfds57123azxbcd", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdgsfdgfds57123azxbce", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdgsfdgfds57123azxbcf", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdgsfdgfds57123azxbcg", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdgsfdgfds57123azxgbcd", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdgsfdgfds57123azxgbce", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdgsfdgfds57123azxgbcf", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdgsfdgfds57123azxgbcg", as_arc(b"xyz"));







    trie.set_val_at(b"57gfdgsfgfdgfsdggsdesf", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdesF", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdesG", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdesH", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdesI", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdesJ", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdesK", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdesM", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdbesf", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdbesF", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdbesG", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdbesH", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdbesI", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdbesJ", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdbesK", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdbesM", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdbcd", as_arc(b"xyz"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdbce", as_arc(b"xyz"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdbcf", as_arc(b"xyz"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdbcg", as_arc(b"xyz"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdgbcd", as_arc(b"xyz"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdgbce", as_arc(b"xyz"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdgbcf", as_arc(b"xyz"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdgbcg", as_arc(b"xyz"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdxxxesf", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdxxxesF", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdxxxesG", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdxxxesH", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdxxxesI", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdxxxesJ", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdxxxesK", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdxxxesM", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdxxxbesf", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdxxxbesF", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdxxxbesG", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdxxxbesH", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdxxxbesI", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdxxxbesJ", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdxxxbesK", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdxxxbesM", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdxxxbcd", as_arc(b"xyz"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdxxxbce", as_arc(b"xyz"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdxxxbcf", as_arc(b"xyz"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdxxxbcg", as_arc(b"xyz"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdxxxgbcd", as_arc(b"xyz"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdxxxgbce", as_arc(b"xyz"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdxxxgbcf", as_arc(b"xyz"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsdxxxgbcg", as_arc(b"xyz"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsd23dzxesf", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsd23dzxesF", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsd23dzxesG", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsd23dzxesH", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsd23dzxesI", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsd23dzxesJ", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsd23dzxesK", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsd23dzxesM", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsd23azxbesf", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsd23azxbesF", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsd23azxbesG", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsd23azxbesH", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsd23azxbesI", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsd23azxbesJ", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsd23azxbesK", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsd23azxbesM", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsd23azxbcd", as_arc(b"xyz"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsd23azxbce", as_arc(b"xyz"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsd23azxbcf", as_arc(b"xyz"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsd23azxbcg", as_arc(b"xyz"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsd23azxgbcd", as_arc(b"xyz"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsd23azxgbce", as_arc(b"xyz"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsd23azxgbcf", as_arc(b"xyz"));
    trie.set_val_at(b"57gfdgsfgfdgfsdggsd23azxgbcg", as_arc(b"xyz"));


    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57desf", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57desF", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57desG", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57desH", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57desI", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57desJ", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57desK", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57desM", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57abesf", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57abesF", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57abesG", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57abesH", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57abesI", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57abesJ", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57abesK", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57abesM", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57abcd", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57abce", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57abcf", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57abcg", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57agbcd", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57agbce", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57agbcf", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57agbcg", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57dxxxesf", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57dxxxesF", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57dxxxesG", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57dxxxesH", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57dxxxesI", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57dxxxesJ", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57dxxxesK", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57dxxxesM", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57axxxbesf", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57axxxbesF", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57axxxbesG", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57axxxbesH", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57axxxbesI", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57axxxbesJ", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57axxxbesK", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57axxxbesM", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57axxxbcd", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57axxxbce", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57axxxbcf", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57axxxbcg", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57axxxgbcd", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57axxxgbce", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57axxxgbcf", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57axxxgbcg", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57123dzxesf", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57123dzxesF", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57123dzxesG", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57123dzxesH", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57123dzxesI", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57123dzxesJ", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57123dzxesK", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57123dzxesM", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57123azxbesf", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57123azxbesF", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57123azxbesG", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57123azxbesH", as_arc(b"lmnopqrstuv"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57123azxbesI", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57123azxbesJ", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57123azxbesK", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57123azxbesM", as_arc(b"lmnopqrstu"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57123azxbcd", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57123azxbce", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57123azxbcf", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57123azxbcg", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57123azxgbcd", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57123azxgbce", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57123azxgbcf", as_arc(b"xyz"));
    trie.set_val_at(b"fgsfdfdsafssfdsfsdgsfdgfds57123azxgbcg", as_arc(b"xyz"));


    for l in 0..LEN {
      trie.set_val_at(&ARR[..l], as_arc(&ARR[..l]));
      if l >= 1 {trie.set_val_at(&ARR[1..l], as_arc(&ARR[..15]));};
    }
    for l in 0..LEN {
      trie.set_val_at(&ARR[l..], as_arc(&ARR[..l]));
      if l <= LEN {trie.set_val_at(&ARR[l..LEN], as_arc(&ARR[..25]));};
    }
    for l in 0..LEN {
      trie.set_val_at(&ARR[l..], as_arc(&ARR[..LEN]));
    }

    // trace(trie.clone());

    let trie_clone = trie.clone();

    match std::env::var("CARGO_MANIFEST_DIR") {
      Ok(manifest_dir) => {
        let path = std::path::PathBuf::from(manifest_dir).join(".tmp");
        let _ = std::fs::create_dir(&path);

        let serialized =  write_trie(
          format!("(file : \"{}\", module : \"{}\", line : \"{}\")", file!(), module_path!(), line!()),
          trie.clone(),
          |bs, v|{ v.extend_from_slice(bs); ValueSlice::Encode(
            v
          )}, &path
        ).unwrap();

        let read = std::fs::File::open(serialized.zeroes_compressed_data_path).unwrap();
        dbg_hex_line_numbers(&read, &path).unwrap();

        let de_path = path.join(ZERO_COMPRESSED_HEX_DATA_FILENAME);
        let de = deserialize_file(&de_path, |b|as_arc(b)).unwrap();

        let [src,de_] = [string_pathmap_as_btree_dbg(trie_clone), string_pathmap_as_btree_dbg(de)];
        // println!("src : {src:#?}\n de_ : {de_:#?}");

        core::assert!(src == de_);
      }
      _ => {
        #[cfg(not(miri))]
        panic!("Test should be running under Cargo")
      }
    }
  }

  // for doing test equality check
  fn string_pathmap_as_btree_dbg(map : PathMap<Arc<[u8]>>)->BTreeMap<String,String> {
    let mut map_ = BTreeMap::new();

    map.into_cata_jumping_side_effect(|_bytemask, _accs, _jump_len, v, o| {
      unsafe {
        if let Some(v) = v {
          map_.insert(std::str::from_utf8_unchecked(o).to_owned(), format!("{:?}",std::str::from_utf8_unchecked(v)));
        }
      }
    });
    map_
  }
}


// /////////
// TOOLS //
// ///////

// we could consider making some of the following public later

// for debugging
fn _trace<V: TrieValue + 'static>(trie : PathMap<V>) {
  let counter = core::sync::atomic::AtomicUsize::new(0);
  trie.into_cata_jumping_side_effect(|bytemask, accs, jump_len, v, o| {
    let n = counter.fetch_add(1, core::sync::atomic::Ordering::SeqCst);
    println!(
      "\
      \nALG {{ mask     : {:?}\
      \n      acc_ids  : {accs:?}\
      \n      jump_len : {jump_len}\
      \n      val      : {}\
      \n      origin   : {:?}\
      \n    }}",
      &bytemask.iter().map(|x| x as char).collect::<Vec<_>>(),
      v.is_some(),
      std::str::from_utf8(o).unwrap()
    );
    n
  });
}

/// debuggung tool for seeing the hex offsets in the file. works for raw and zeros conpressed.
pub fn dbg_hex_line_numbers(f : &std::fs::File, path : impl AsRef<std::path::Path>)->Result<std::fs::File, std::io::Error> {
  let mut line_number : u64 = 0;
  let read_buffer = BufReader::new(f);

  let mut path : PathBuf = path.as_ref().to_path_buf();
  path.push("line_number.dbg");

  let out_file = std::fs::File::create(&path)?;
  let mut out : BufWriter<std::fs::File> = BufWriter::new(out_file);


  let leading_offset =
      | line : u64 | 
      {
        let mut keep = false;
        let h = offset_to_hex_be(line.to_be_bytes());
        let last = h[HEX_OFFSET_LEN-1];
        let mut out = h.map(|b| {
          match b {
              b'x' => b'x',
              b'0' => if keep { b'0'} else {b'_'},
              _    => { keep = true; 
                        b
                      }
          }
        });
        out[HEX_OFFSET_LEN-1] = last;
        out
      };

  out.write_all(b"|   hex index   |")?;
  out.write_all(b" ")?;


  for b in read_buffer.bytes() {
    let c = b?;
    out.write_all(&[c])?;
    if let b'\n' = c {
      out.write_all(&leading_offset(line_number))?;
      out.write_all(b" ")?;
      line_number += INCR as u64;
    }
  }

  Ok(out.into_inner().unwrap())
}
