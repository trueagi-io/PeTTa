//! Output formatters for PeTTa results
//!
//! Provides multiple output formats for displaying query results.

use crate::engine::values::MettaResult;

pub trait OutputFormatter: Send + Sync {
    fn format(&self, results: &[MettaResult]) -> String;
    fn format_single(&self, result: &MettaResult) -> String;
}

pub struct PrettyFormatter {
    use_color: bool,
}

impl PrettyFormatter {
    pub fn new(use_color: bool) -> Self {
        Self { use_color }
    }
}

impl OutputFormatter for PrettyFormatter {
    fn format(&self, results: &[MettaResult]) -> String {
        if results.is_empty() {
            return String::new();
        }
        let mut output = String::new();
        for (i, r) in results.iter().enumerate() {
            if i > 0 {
                output.push('\n');
            }
            output.push_str(&self.format_single(r));
        }
        output
    }

    fn format_single(&self, result: &MettaResult) -> String {
        if self.use_color {
            use crate::utils::cyan;
            format!(" {} ", cyan(&result.value))
        } else {
            format!(" {}", result.value)
        }
    }
}

#[derive(Default)]
pub struct CompactFormatter;

impl CompactFormatter {
    pub fn new() -> Self {
        Self
    }
}

impl OutputFormatter for CompactFormatter {
    fn format(&self, results: &[MettaResult]) -> String {
        results.iter().map(|r| &*r.value).collect::<Vec<_>>().join(" ")
    }

    fn format_single(&self, result: &MettaResult) -> String {
        result.value.clone()
    }
}

#[derive(Default)]
pub struct JsonFormatter;

impl JsonFormatter {
    pub fn new() -> Self {
        Self
    }
}

impl OutputFormatter for JsonFormatter {
    fn format(&self, results: &[MettaResult]) -> String {
        let values: Vec<&str> = results.iter().map(|r| r.value.as_str()).collect();
        serde_json::to_string(&values).unwrap_or_else(|_| "[]".to_string())
    }

    fn format_single(&self, result: &MettaResult) -> String {
        serde_json::to_string(&result.value).unwrap_or_else(|_| "\"\"".to_string())
    }
}

#[derive(Default)]
pub struct SExprFormatter;

impl SExprFormatter {
    pub fn new() -> Self {
        Self
    }
}

impl OutputFormatter for SExprFormatter {
    fn format(&self, results: &[MettaResult]) -> String {
        results.iter().map(|r| self.format_single(r)).collect::<Vec<_>>().join("\n")
    }

    fn format_single(&self, result: &MettaResult) -> String {
        let v = &result.value;
        if v.starts_with('(') || v.starts_with('[') {
            v.clone()
        } else {
            format!("({})", v)
        }
    }
}

pub fn create_formatter(format: &str, use_color: bool) -> Box<dyn OutputFormatter> {
    match format {
        "pretty" => Box::new(PrettyFormatter::new(use_color)),
        "compact" => Box::new(CompactFormatter::new()),
        "json" => Box::new(JsonFormatter::new()),
        "sexpr" => Box::new(SExprFormatter::new()),
        _ => Box::new(PrettyFormatter::new(use_color)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_result(value: &str) -> MettaResult {
        MettaResult { value: value.into() }
    }

    #[test]
    fn test_pretty_formatter() {
        let formatter = PrettyFormatter::new(false);
        let results = vec![make_result("42")];
        let output = formatter.format(&results);
        assert!(output.contains("42"));
    }

    #[test]
    fn test_compact_formatter() {
        let formatter = CompactFormatter::new();
        let results = vec![make_result("1"), make_result("2"), make_result("3")];
        let output = formatter.format(&results);
        assert_eq!(output, "1 2 3");
    }

    #[test]
    fn test_json_formatter() {
        let formatter = JsonFormatter::new();
        let results = vec![make_result("42")];
        let output = formatter.format(&results);
        assert!(output.contains("42"));
    }
}