use super::*;

/// Returns a coroutine to incrementally serialize into `.paths` data and write to `target`
///
/// Passing `None` signals the end of input.
/// Warning: the size of the individual path serialization can be double exponential in the size of the PathMap
pub fn paths_serialization_sink<'p, W: std::io::Write>(target: &mut W) -> impl std::ops::Coroutine<Option<&'p [u8]>, Yield=(), Return=std::io::Result<SerializationStats>> {
  #[coroutine] move |i: Option<&'p [u8]>| {
    const CHUNK: usize = 4096; // not tuned yet
    let mut buffer = [0u8; CHUNK];

    #[allow(invalid_value)] //Squish the warning about a Null function ptr, because zlib uses a default allocator if the the ptr is NULL
    //I filed https://github.com/rust-lang/libz-sys/issues/243 to track this issue, and I confirmed the easy fix works, but I didn't submit
    // a PR because their build and validation process is very confusing.
    let mut strm: z_stream = unsafe { std::mem::MaybeUninit::zeroed().assume_init() };
    let mut ret = unsafe { zng_deflateInit(&mut strm, 7) };
    assert_eq!(ret, Z_OK);

    let mut total_paths: usize = 0;
    if let Some(mut p) = i {
      loop {
        let l = p.len();
        let mut lin = (l as u32).to_le_bytes();
        strm.avail_in = 4;
        strm.next_in = lin.as_mut_ptr();

        // todo (Adam): this is stupid/simple code; the following two blocks should be merged and write out the path length and path together
        loop {
          strm.avail_out = CHUNK as _;
          strm.next_out = buffer.as_mut_ptr();
          ret = unsafe { deflate(&mut strm, Z_NO_FLUSH) };
          assert_ne!(ret, Z_STREAM_ERROR);
          let have = CHUNK - strm.avail_out as usize;
          target.write_all(&mut buffer[..have])?;
          if strm.avail_out != 0 { break }
        }
        assert_eq!(strm.avail_in, 0);

        strm.avail_in = l as _;
        strm.next_in = p.as_ptr().cast_mut();
        loop {
          strm.avail_out = CHUNK as _;
          strm.next_out = buffer.as_mut_ptr();
          ret = unsafe { deflate(&mut strm, Z_NO_FLUSH) };
          assert_ne!(ret, Z_STREAM_ERROR);
          let have = CHUNK - strm.avail_out as usize;
          target.write_all(&mut buffer[..have])?;
          if strm.avail_out != 0 { break }
        }
        assert_eq!(strm.avail_in, 0);

        total_paths += 1;
        match yield () {
          Some(np) => { p = np }
          None => break
        }
      }
    }
    loop {
      strm.avail_out = CHUNK as _;
      strm.next_out = buffer.as_mut_ptr();
      ret = unsafe { deflate(&mut strm, Z_FINISH) };
      let have = CHUNK - strm.avail_out as usize;
      target.write_all(&buffer[..have])?;
      if ret == Z_STREAM_END { break; }
      assert_eq!(ret, Z_OK);
    }
    ret = unsafe { deflateEnd(&mut strm) };
    assert_eq!(ret, Z_OK);

    Ok(SerializationStats {
      bytes_out  : strm.total_out,
      bytes_in   : strm.total_in,
      path_count : total_paths
    })
  }
}
