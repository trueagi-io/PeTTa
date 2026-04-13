//! Handy utilities to help debug PathMap and software that uses `pathmap`

mod diff_zipper;
pub use diff_zipper::DiffZipper;

mod morphism_debug;
pub use morphism_debug::CatamorphismDebug;

extern crate alloc;
use alloc::borrow::Cow;

#[derive(PartialEq, Eq)]
pub(crate) enum PathRenderMode { RequireAscii, TryAscii, ByteList }

/// Internal method to render a path for the purposes of debug output
///
/// Returns `None` if ascii was requested but the string would not render as ascii.
pub(crate) fn render_debug_path(path: &[u8], mode: PathRenderMode) -> Option<Cow<'_, str>> {
    const MAX_DEBUG_ASCII_PATH_LEN: usize = 1024;
    const MAX_DEBUG_BYTE_LIST_LEN: usize = 256;

    if mode == PathRenderMode::RequireAscii || mode == PathRenderMode::TryAscii {
        let short_path = &path[..path.len().min(MAX_DEBUG_ASCII_PATH_LEN)];

        //QUESTION: Do we want to actually fall back if we encounter any control chars?
        //  Chars <32 might indicate that we would be better off printing this as
        //  a list as opposed to a str.
        match str::from_utf8(short_path) {
            Ok(key) => {
                if path.len() > MAX_DEBUG_ASCII_PATH_LEN {
                    return Some(Cow::Owned(format!("{key}...")))
                } else {
                    return Some(Cow::Borrowed(key))
                }
            },
            Err(_) => {
                if mode == PathRenderMode::RequireAscii {
                    return None
                }
            }
        }
    }

    if path.len() > MAX_DEBUG_BYTE_LIST_LEN {
        let short_path = &path[..MAX_DEBUG_BYTE_LIST_LEN];
        Some(Cow::Owned(format!("{short_path:?}...")))
    } else {
        Some(Cow::Owned(format!("{path:?}")))
    }
}