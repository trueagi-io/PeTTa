use crate::pathmap::TrieValue;
use crate::pathmap::alloc::Allocator;
use crate::pathmap::morphisms::{Catamorphism, new_map_from_ana_jumping};
use crate::pathmap::utils::{BitMask, ByteMask};
use crate::pathmap::write_zipper::ZipperWriting;
use crate::pathmap::zipper;
use std::cell::UnsafeCell;
use std::io::Write;
use std::ptr::slice_from_raw_parts;

/// WIP
pub fn serialize_fork<V: TrieValue, RZ: Catamorphism<V>, F: FnMut(usize, &[u8], &V) -> ()>(
    rz: RZ,
    target: &mut Vec<u8>,
    _fv: F,
) -> std::io::Result<usize> {
    unsafe {
        thread_local! {
            static WRITTEN: UnsafeCell<usize> = UnsafeCell::new(0)
        }
        WRITTEN.with(|w| {
            rz.into_cata_jumping_side_effect_fallible(
                |bm: &ByteMask, ws: &mut [usize], jump, _ov: Option<&V>, path: &[u8]| {
                    // let bs = bm.iter().collect::<Vec<u8>>();
                    // println!("at {path:?} #{} jump {} {:?}", bm.count_bits(), jump, &path[path.len()-jump..]);

                    let w0 = *w.get();
                    // println!("child ptrs {:?}, w0 {}, targetlen {}", ws, w0, target.len());
                    let mut l = 0;
                    target.write_all(jump.to_le_bytes().as_slice())?;
                    l += 8;
                    target.write_all(&path[path.len() - jump..])?;
                    l += jump;
                    target.write_all(
                        slice_from_raw_parts(bm.0.as_ptr() as *const u8, 32).as_ref().unwrap(),
                    )?;
                    l += 32;
                    target.write_all(
                        slice_from_raw_parts(ws.as_ptr() as *const u8, 8 * ws.len())
                            .as_ref()
                            .unwrap(),
                    )?;
                    l += 8 * ws.len();
                    *w.get() = w0 + l;

                    Ok(w0)
                },
            )
        })
    }
}

/// WIP
pub fn deserialize_fork<
    V: TrieValue + 'static,
    A: Allocator,
    WZ: ZipperWriting<V, A> + zipper::ZipperMoving,
    F: Fn(usize, &[u8]) -> V,
>(
    node: usize,
    wz: &mut WZ,
    source: &[u8],
    fv: F,
) -> std::io::Result<usize> {
    unsafe {
        // let mut recovered = 0;
        new_map_from_ana_jumping(wz, node, |n: usize, path: &[u8]| {
            // println!("n {}", n);
            let jump = usize::from_le_bytes((&source[n..n + 8]).try_into().unwrap());
            let jump_path = &source[n + 8..n + 8 + jump];
            let bm = ByteMask(std::ptr::read(&source[n + 8 + jump] as *const u8 as *const _));
            let ws = slice_from_raw_parts(
                &source[n + 8 + jump + 32] as *const u8 as *const usize,
                bm.count_bits(),
            )
            .as_ref()
            .unwrap();
            // println!("offset {}", n+8+jump+32);
            // println!("at {path:?} #{} jump {} {:?}", bm.count_bits(), jump, jump_path);
            // println!("child ptrs {:?}", ws);

            // recovered += 1;
            (jump_path, bm, ws.into_iter().cloned(), Some(fv(0, path)))
        });
        Ok(1)
    }
}

#[cfg(all(test, feature = "pathmap-internal-tests"))]
mod tests {
    use super::super::PathMap;
    use super::experimental::tree_serialization::{deserialize_fork, serialize_fork};
    use super::morphisms::Catamorphism;

    #[ignore] //GOAT, re-enable if/when this code is ready.
    #[test]
    fn tree_serde_2() {
        let keys = [vec![12, 13, 14], vec![12, 13, 14, 100, 101]];
        let btm: PathMap<usize> = keys.into_iter().enumerate().map(|(i, k)| (k, i)).collect();

        let mut v = vec![];
        let Ok(top_node) = serialize_fork(btm.read_zipper(), &mut v, |_1, _2, _3| {}) else {
            unreachable!()
        };
        let mut recovered = PathMap::new();
        deserialize_fork(top_node, &mut recovered.write_zipper(), &v[..], |_, _p| ()).unwrap();
        assert_eq!(btm.hash_with(|_| 0), recovered.hash_with(|_| 0));
    }
}
