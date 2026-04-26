//! Nightly-only paths serialization using coroutines

use super::*;
use super::paths_serialization::SerializationStats;

/// Returns a coroutine to incrementally serialize into `.paths` data and write to `target`
///
/// Passing `None` signals the end of input.
/// Warning: the size of the individual path serialization can be double exponential in the size of the PathMap
pub fn paths_serialization_sink<'p, W: std::io::Write>(
    target: &mut W,
) -> impl std::ops::Coroutine<Option<&'p [u8]>, Yield = (), Return = std::io::Result<SerializationStats>>
{
    use super::paths_serialization::serialize_paths_from_funcs;

    #[coroutine]
    move |mut i: Option<&'p [u8]>| {
        let mut k: usize = 0;
        let _ = serialize_paths_from_funcs(
            target,
            &mut i,
            |input| match *input {
                Some(_) => Ok(true),
                None => Ok(false),
            },
            |input| {
                if input.is_some() {
                    k += 1;
                }
                *input
            },
        );
        yield ();
    }
}