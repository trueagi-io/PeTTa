//! ANSI color utilities for terminal output

/// Apply ANSI color code to a string
#[inline]
pub fn ansi_color(s: &str, code: u8) -> String {
    format!("\x1b[{}m{}\x1b[0m", code, s)
}

/// Green colored string (code 32)
#[inline]
pub fn green(s: &str) -> String {
    ansi_color(s, 32)
}

/// Red colored string (code 31)
#[inline]
pub fn red(s: &str) -> String {
    ansi_color(s, 31)
}

/// Yellow colored string (code 33)
#[inline]
pub fn yellow(s: &str) -> String {
    ansi_color(s, 33)
}

/// Cyan colored string (code 36)
#[inline]
pub fn cyan(s: &str) -> String {
    ansi_color(s, 36)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_color_functions() {
        assert!(green("test").contains("test"));
        assert!(red("test").contains("test"));
        assert!(yellow("test").contains("test"));
        assert!(cyan("test").contains("test"));
    }

    #[test]
    fn test_ansi_color_codes() {
        assert!(green("test").starts_with("\x1b[32m"));
        assert!(red("test").starts_with("\x1b[31m"));
        assert!(yellow("test").starts_with("\x1b[33m"));
        assert!(cyan("test").starts_with("\x1b[36m"));
    }
}
