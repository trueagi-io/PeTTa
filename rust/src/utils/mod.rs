pub fn bold(s: &str) -> String {
    format!("\x1b[1m{}\x1b[0m", s)
}

pub fn green(s: &str) -> String {
    format!("\x1b[32m{}\x1b[0m", s)
}

pub fn red(s: &str) -> String {
    format!("\x1b[31m{}\x1b[0m", s)
}

pub fn yellow(s: &str) -> String {
    format!("\x1b[33m{}\x1b[0m", s)
}

pub fn cyan(s: &str) -> String {
    format!("\x1b[36m{}\x1b[0m", s)
}

pub fn format_duration_ms(ms: f64) -> String {
    if ms < 1.0 {
        cyan(&format!("{:.2}μs", ms * 1000.0))
    } else if ms < 1000.0 {
        green(&format!("{:.2}ms", ms))
    } else {
        yellow(&format!("{:.2}s", ms / 1000.0))
    }
}

pub fn truncate(s: &str, max_len: usize) -> &str {
    if s.len() <= max_len { s } else { &s[..max_len.saturating_sub(3) + 3] }
}

pub fn word_wrap(text: &str, width: usize) -> String {
    if width == 0 {
        return text.to_string();
    }
    let mut result = String::new();
    let mut line_len = 0;
    for word in text.split_whitespace() {
        let word_len = word.len();
        if line_len + word_len + 1 > width {
            if !result.is_empty() {
                result.push('\n');
            }
            result.push_str(word);
            line_len = word_len;
        } else {
            if line_len > 0 {
                result.push(' ');
                line_len += 1;
            }
            result.push_str(word);
            line_len += word_len;
        }
    }
    result
}
