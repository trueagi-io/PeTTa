//! PeTTa - MeTTa language implementation in Rust + SWI-Prolog
//!
//! This crate provides a MeTTa (Metalanguage for Transformation) engine
//! using SWI-Prolog as a subprocess.
//!
//! # Architecture
//! ```text
//! Rust PeTTaEngine
//!   └── SWI-Prolog subprocess (one per call)
//!         └── MeTTa runtime (7 Prolog files)
//! ```

use std::path::{Path, PathBuf};
use std::process::Command;

// ---------------------------------------------------------------------------
// Typed MeTTa values
// ---------------------------------------------------------------------------

/// A typed MeTTa value parsed from S-expression output.
#[derive(Debug, Clone, PartialEq)]
pub enum MettaValue {
    /// Integer
    Integer(String),
    /// Float
    Float(f64),
    /// Boolean
    Bool(bool),
    /// Atom / symbol
    Atom(String),
    /// List of values
    List(Vec<MettaValue>),
    /// S-expression / function application
    Expression(String, Vec<MettaValue>),
}

impl MettaValue {
    /// Parse an S-expression string into a typed value.
    pub fn parse(s: &str) -> Option<Self> {
        parse_sexpr(s.trim())
    }
}

fn parse_sexpr(s: &str) -> Option<MettaValue> {
    let s = s.trim();
    if s.is_empty() { return None; }
    if s == "true" { return Some(MettaValue::Bool(true)); }
    if s == "false" { return Some(MettaValue::Bool(false)); }
    if s.parse::<i64>().is_ok() { return Some(MettaValue::Integer(s.to_string())); }
    if let Ok(f) = s.parse::<f64>() { return Some(MettaValue::Float(f)); }
    if !s.starts_with('(') { return Some(MettaValue::Atom(s.to_string())); }
    if let Some(inner) = strip_parens(s) {
        let items = split_top_level(inner);
        if items.is_empty() { return Some(MettaValue::List(Vec::new())); }
        let parsed: Vec<MettaValue> = items.iter().filter_map(|x| parse_sexpr(x)).collect();
        if parsed.len() == items.len() && !parsed.is_empty() {
            if let MettaValue::Atom(head) = &parsed[0] {
                if parsed.len() > 1 {
                    return Some(MettaValue::Expression(head.clone(), parsed[1..].to_vec()));
                }
            }
            return Some(MettaValue::List(parsed));
        }
    }
    Some(MettaValue::Atom(s.to_string()))
}

fn strip_parens(s: &str) -> Option<&str> {
    if s.starts_with('(') && s.ends_with(')') && is_balanced(&s[1..s.len() - 1]) {
        Some(&s[1..s.len() - 1])
    } else { None }
}

fn is_balanced(s: &str) -> bool {
    let mut depth = 0i32;
    for ch in s.chars() {
        match ch {
            '(' => depth += 1,
            ')' => { depth -= 1; if depth < 0 { return false; } }
            _ => {}
        }
    }
    depth == 0
}

fn split_top_level(s: &str) -> Vec<String> {
    let mut items = Vec::new();
    let mut current = String::new();
    let mut depth = 0i32;
    let mut in_quote = false;
    let mut escape = false;
    for ch in s.chars() {
        if escape { current.push(ch); escape = false; continue; }
        if ch == '\\' { escape = true; current.push(ch); continue; }
        if ch == '"' { in_quote = !in_quote; current.push(ch); continue; }
        if in_quote { current.push(ch); continue; }
        match ch {
            '(' => { depth += 1; current.push(ch); }
            ')' => { depth -= 1; current.push(ch); }
            ' ' | '\t' | '\n' | '\r' if depth == 0 => {
                if !current.is_empty() { items.push(std::mem::take(&mut current)); }
            }
            _ => current.push(ch),
        }
    }
    if !current.is_empty() { items.push(current); }
    items
}

// ---------------------------------------------------------------------------
// MeTTa result
// ---------------------------------------------------------------------------

/// A single result from executing MeTTa code.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MettaResult {
    /// Raw string representation (S-expression format).
    pub value: String,
}

impl MettaResult {
    /// Parse the value into a typed [`MettaValue`].
    pub fn parsed_value(&self) -> Option<MettaValue> {
        MettaValue::parse(&self.value)
    }
}

// ---------------------------------------------------------------------------
// Human-readable error types
// ---------------------------------------------------------------------------

/// Structured SWI-Prolog error.
#[derive(Debug, Clone)]
pub enum SwiplErrorKind {
    UndefinedFunction { name: String, arity: usize },
    TypeMismatch { expected: String, found: String },
    SyntaxError { detail: String },
    UninstantiatedArgument,
    PermissionDenied { operation: String, target: String },
    StackOverflow,
    Generic(String),
}

impl std::fmt::Display for SwiplErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SwiplErrorKind::UndefinedFunction { name, arity } =>
                write!(f, "MeTTa function '{}/{}' is not defined", name, arity),
            SwiplErrorKind::TypeMismatch { expected, found } =>
                write!(f, "Type error: expected {}, got {}", expected, found),
            SwiplErrorKind::SyntaxError { detail } =>
                write!(f, "Syntax error: {}", detail),
            SwiplErrorKind::UninstantiatedArgument =>
                write!(f, "Argument is not sufficiently instantiated"),
            SwiplErrorKind::PermissionDenied { operation, target } =>
                write!(f, "Permission denied: {} on {}", operation, target),
            SwiplErrorKind::StackOverflow =>
                write!(f, "Stack overflow — increase stack limit with --stack_limit"),
            SwiplErrorKind::Generic(msg) => write!(f, "{}", msg),
        }
    }
}

fn parse_swipl_error(raw: &str) -> SwiplErrorKind {
    if raw.contains("existence_error") && raw.contains("procedure") {
        if let Some(cap) = extract_name_arity(raw) {
            return SwiplErrorKind::UndefinedFunction { name: cap.0, arity: cap.1 };
        }
    }
    if raw.contains("type_error") {
        return SwiplErrorKind::TypeMismatch { expected: "unknown".into(), found: "unknown".into() };
    }
    if raw.contains("syntax_error") {
        return SwiplErrorKind::SyntaxError { detail: extract_between(raw, "syntax_error(", ")").unwrap_or_else(|| "unknown".into()) };
    }
    if raw.contains("instantiation_error") { return SwiplErrorKind::UninstantiatedArgument; }
    if raw.contains("permission_error") {
        return SwiplErrorKind::PermissionDenied { operation: "unknown".into(), target: "unknown".into() };
    }
    if raw.contains("Stack depth") || raw.contains("stack_limit") { return SwiplErrorKind::StackOverflow; }
    SwiplErrorKind::Generic(raw.lines().next().unwrap_or(raw).trim().to_string())
}

fn extract_name_arity(raw: &str) -> Option<(String, usize)> {
    for token in raw.split(&['(', ')', ',', '/']) {
        let token = token.trim();
        let parts: Vec<&str> = token.split_whitespace().collect();
        if parts.len() == 2 {
            if let Ok(arity) = parts[1].parse::<usize>() {
                return Some((parts[0].to_string(), arity));
            }
        }
    }
    None
}

fn extract_between(s: &str, start: &str, end: &str) -> Option<String> {
    let start_idx = s.find(start)?;
    let rest = &s[start_idx + start.len()..];
    let end_idx = rest.find(end)?;
    Some(rest[..end_idx].to_string())
}

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

/// Errors that can occur during PeTTa execution.
#[derive(Debug)]
pub enum PeTTaError {
    FileNotFound(PathBuf),
    SpawnSwipl(String),
    PathError(String),
    WriteError(String),
    SwiplError(SwiplErrorKind),
    SwiplVersionError(String),
}

impl std::fmt::Display for PeTTaError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PeTTaError::FileNotFound(p) => write!(f, "File not found: {}", p.display()),
            PeTTaError::SpawnSwipl(e) => write!(f, "Failed to spawn swipl: {}", e),
            PeTTaError::PathError(e) => write!(f, "Path error: {}", e),
            PeTTaError::WriteError(e) => write!(f, "Write error: {}", e),
            PeTTaError::SwiplError(e) => write!(f, "{}", e),
            PeTTaError::SwiplVersionError(e) => write!(f, "SWI-Prolog version: {}", e),
        }
    }
}

impl std::error::Error for PeTTaError {}

// ---------------------------------------------------------------------------
// Engine
// ---------------------------------------------------------------------------

/// The main PeTTa engine wrapper.
pub struct PeTTaEngine {
    project_root: PathBuf,
    verbose: bool,
}

impl PeTTaEngine {
    /// Create a new PeTTa engine.
    pub fn new(project_root: &Path, verbose: bool) -> Result<Self, PeTTaError> {
        let src_dir = project_root.join("src");
        if !src_dir.exists() { return Err(PeTTaError::FileNotFound(src_dir)); }
        let abs_root = project_root
            .canonicalize()
            .map_err(|e| PeTTaError::PathError(e.to_string()))?;
        check_swipl_version()?;
        Ok(Self { project_root: abs_root, verbose })
    }

    /// Load and execute a MeTTa file.
    pub fn load_metta_file(&self, file_path: &Path) -> Result<Vec<MettaResult>, PeTTaError> {
        let abs_path = file_path.canonicalize().map_err(|e| PeTTaError::PathError(e.to_string()))?;
        if !abs_path.exists() { return Err(PeTTaError::FileNotFound(abs_path)); }
        let main_pl = self.project_root.join("src").join("main.pl");
        let rel_path = abs_path.strip_prefix(&self.project_root).unwrap_or(&abs_path);
        let mut cmd = Command::new("swipl");
        cmd.arg("-q").arg("-s").arg(&main_pl).arg("--").arg(rel_path);
        if !self.verbose { cmd.arg("--silent"); }
        let output = cmd.output().map_err(|e| PeTTaError::SpawnSwipl(e.to_string()))?;
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(PeTTaError::SwiplError(parse_swipl_error(&stderr)));
        }
        let stdout = String::from_utf8_lossy(&output.stdout);
        Ok(parse_output(&stdout, self.verbose))
    }

    /// Load and execute multiple MeTTa files by concatenating content into one subprocess.
    pub fn load_metta_files(&self, file_paths: &[&Path]) -> Result<Vec<MettaResult>, PeTTaError> {
        if file_paths.is_empty() { return Ok(Vec::new()); }
        let combined: String = file_paths.iter().map(|p| {
            let abs = p.canonicalize().map_err(|e| PeTTaError::PathError(e.to_string()))?;
            if !abs.exists() { return Err(PeTTaError::FileNotFound(abs)); }
            std::fs::read_to_string(&abs).map_err(|e| PeTTaError::PathError(e.to_string()))
        }).collect::<Result<Vec<String>, PeTTaError>>()?.join("\n");
        self.process_metta_string(&combined)
    }

    /// Process a MeTTa code string.
    pub fn process_metta_string(&self, metta_code: &str) -> Result<Vec<MettaResult>, PeTTaError> {
        let main_pl = self.project_root.join("src").join("main.pl");
        let escaped = metta_code.replace('\\', "\\\\").replace('\'', "\\'");
        let silent_flag = if self.verbose { "false" } else { "true" };
        let query = format!(
            "assertz(working_dir('{}')), assertz(silent({})), \
             process_metta_string('{}', Results), \
             maplist(swrite, Results, Strings), \
             (Strings == [] -> true ; maplist(writeln, Strings)), \
             halt.",
            self.project_root.to_string_lossy().replace('\\', "\\\\"),
            silent_flag, escaped,
        );
        let output = Command::new("swipl").arg("-q").arg("-s").arg(&main_pl)
            .arg("-g").arg(&query)
            .output().map_err(|e| PeTTaError::SpawnSwipl(e.to_string()))?;
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(PeTTaError::SwiplError(parse_swipl_error(&stderr)));
        }
        let stdout = String::from_utf8_lossy(&output.stdout);
        Ok(parse_output(&stdout, self.verbose))
    }
}

// ---------------------------------------------------------------------------
// Output parsing
// ---------------------------------------------------------------------------

fn parse_output(output: &str, _verbose: bool) -> Vec<MettaResult> {
    output.lines().filter_map(|line| {
        let cleaned = strip_ansi(line.trim());
        if cleaned.is_empty() || cleaned.starts_with('%') || cleaned.starts_with("?-")
            || cleaned.starts_with(":-") || cleaned.starts_with("-->")
            || cleaned.starts_with("^^^") || cleaned.starts_with('!')
            || cleaned.contains("metta function") || cleaned.contains("metta runnable")
            || cleaned.contains("prolog clause") || cleaned.contains("prolog goal")
        { return None; }
        Some(MettaResult { value: cleaned })
    }).collect()
}

fn strip_ansi(s: &str) -> String {
    let mut result = String::new();
    let mut in_escape = false;
    for ch in s.chars() {
        if ch == '\x1b' { in_escape = true; }
        else if in_escape {
            if ch == '[' { continue; }
            if (0x40..=0x7E).contains(&(ch as u32)) { in_escape = false; }
        } else { result.push(ch); }
    }
    result.trim().to_string()
}

// ---------------------------------------------------------------------------
// Version check
// ---------------------------------------------------------------------------

const MIN_SWIPL_MAJOR: u32 = 9;
const MIN_SWIPL_MINOR: u32 = 3;

fn check_swipl_version() -> Result<(), PeTTaError> {
    let output = Command::new("swipl").arg("--version").output().map_err(|_| {
        PeTTaError::SwiplVersionError(format!(
            "swipl not found on PATH. Install SWI-Prolog >= {}.{}.", MIN_SWIPL_MAJOR, MIN_SWIPL_MINOR))
    })?;
    if !output.status.success() {
        return Err(PeTTaError::SwiplVersionError("swipl --version failed".into()));
    }
    let version_str = String::from_utf8_lossy(&output.stdout);
    for part in version_str.split_whitespace() {
        let parts: Vec<&str> = part.split('.').collect();
        if parts.len() >= 2 {
            if let (Ok(major), Ok(minor)) = (parts[0].parse::<u32>(), parts[1].parse::<u32>()) {
                if major > MIN_SWIPL_MAJOR || (major == MIN_SWIPL_MAJOR && minor >= MIN_SWIPL_MINOR) {
                    return Ok(());
                }
                if major < MIN_SWIPL_MAJOR {
                    return Err(PeTTaError::SwiplVersionError(format!(
                        "SWI-Prolog {}.{} found, but >= {}.{} required",
                        major, minor, MIN_SWIPL_MAJOR, MIN_SWIPL_MINOR)));
                }
            }
        }
    }
    Ok(())
}

/// Check if SWI-Prolog is available.
pub fn swipl_available() -> bool { check_swipl_version().is_ok() }

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn project_root() -> PathBuf { Path::new(env!("CARGO_MANIFEST_DIR")).to_path_buf() }
    fn make_engine() -> PeTTaEngine {
        PeTTaEngine::new(&project_root(), false).expect("Failed to create engine")
    }

    #[test] fn test_engine_creation() {
        let root = project_root();
        let result = PeTTaEngine::new(&root, false);
        if let Err(ref e) = result { eprintln!("Engine creation failed: {}", e); }
        assert!(result.is_ok());
    }
    #[test] fn test_identity() {
        let engine = make_engine();
        let results = engine.process_metta_string("(= (myid $x) $x) !(myid 42)").unwrap();
        assert!(!results.is_empty());
        assert_eq!(results[0].value, "42");
    }
    #[test] fn test_arithmetic() {
        let engine = make_engine();
        let results = engine.process_metta_string("!(+ 1 2)").unwrap();
        assert!(!results.is_empty());
        assert_eq!(results[0].value, "3");
    }
    #[test] fn test_load_identity_file() {
        let engine = make_engine();
        let results = engine.load_metta_file(&project_root().join("examples/identity.metta")).unwrap();
        assert!(!results.is_empty());
    }
    #[test] fn test_boolean() {
        let engine = make_engine();
        let results = engine.process_metta_string("!(and true false)").unwrap();
        assert!(!results.is_empty());
        assert_eq!(results[0].value, "false");
    }
    #[test] fn test_comparison() {
        let engine = make_engine();
        let results = engine.process_metta_string("!(< 1 2)").unwrap();
        assert!(!results.is_empty());
        assert_eq!(results[0].value, "true");
    }
    #[test] fn test_fibonacci() {
        let engine = make_engine();
        let results = engine.load_metta_file(&project_root().join("examples/fib.metta")).unwrap();
        assert!(!results.is_empty());
    }
    #[test] fn test_state() {
        let engine = make_engine();
        let results = engine.load_metta_file(&project_root().join("examples/state.metta")).unwrap();
        assert!(!results.is_empty());
    }
    #[test] fn test_if() {
        let engine = make_engine();
        let results = engine.load_metta_file(&project_root().join("examples/if.metta")).unwrap();
        assert!(!results.is_empty());
    }
    #[test] fn test_math() {
        let engine = make_engine();
        let results = engine.load_metta_file(&project_root().join("examples/math.metta")).unwrap();
        assert!(!results.is_empty());
    }
    #[test] fn test_variable_renaming() {
        let engine = make_engine();
        let results = engine.process_metta_string("(= (fun ($a x)) ($b x)) !(fun (a x))").unwrap();
        assert!(!results.is_empty());
        let val = &results[0].value;
        assert!(val.contains("$") && val.contains('x'), "Expected variable pattern, got: {}", val);
    }
    #[test] fn test_file_imports() {
        let engine = make_engine();
        let root = project_root();
        let identity_code = std::fs::read_to_string(root.join("examples/identity.metta")).unwrap();
        let combined = format!("{}\n!(f 5)", identity_code);
        let results = engine.process_metta_string(&combined).unwrap();
        assert!(results.iter().any(|r| r.value == "25"), "Expected '25' in results: {:?}", results);
    }
    #[test] fn test_verbose_mode() {
        let root = project_root();
        let engine = PeTTaEngine::new(&root, true).unwrap();
        let results = engine.process_metta_string("!(+ 1 2)").unwrap();
        assert!(!results.is_empty());
        assert_eq!(results[0].value, "3");
    }
    #[test] fn test_parse_output() {
        let results = parse_output("42\n(a b c)\nhello", false);
        assert_eq!(results.len(), 3);
        assert_eq!(results[0].value, "42");
        assert_eq!(results[1].value, "(a b c)");
        assert_eq!(results[2].value, "hello");
    }
    #[test] fn test_parse_output_filters_debug() {
        let results = parse_output("--> metta function -->\n42\n^^^^^^^^^^^^^^^^^^^\n\x1b[36m!(+ 1 2)\n\x1b[33m-->", false);
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].value, "42");
    }
    #[test] fn test_parse_int() { assert_eq!(MettaValue::parse("42"), Some(MettaValue::Integer("42".into()))); }
    #[test] fn test_parse_bool() {
        assert_eq!(MettaValue::parse("true"), Some(MettaValue::Bool(true)));
        assert_eq!(MettaValue::parse("false"), Some(MettaValue::Bool(false)));
    }
    #[test] fn test_parse_float() { assert_eq!(MettaValue::parse("3.14"), Some(MettaValue::Float(3.14))); }
    #[test] fn test_parse_atom() { assert_eq!(MettaValue::parse("hello"), Some(MettaValue::Atom("hello".into()))); }
    #[test] fn test_parse_list() {
        let v = MettaValue::parse("(1 2 3)");
        assert!(matches!(v, Some(MettaValue::List(ref items)) if items.len() == 3));
    }
    #[test] fn test_parse_expression() {
        let v = MettaValue::parse("(+ 1 2)");
        assert!(matches!(v, Some(MettaValue::Expression(ref f, ref args)) if f == "+" && args.len() == 2));
    }
    #[test] fn test_parse_undefined_function_error() {
        let raw = "ERROR: existence_error(procedure, (/ foo 2))";
        let err = parse_swipl_error(raw);
        assert!(matches!(err, SwiplErrorKind::UndefinedFunction { .. }));
    }
    #[test] fn test_parse_stack_overflow() {
        let raw = "ERROR: Stack depth: 5000000";
        let err = parse_swipl_error(raw);
        assert!(matches!(err, SwiplErrorKind::StackOverflow));
    }
    #[test] fn test_result_parsed_value() {
        let r = MettaResult { value: "42".into() };
        assert_eq!(r.parsed_value(), Some(MettaValue::Integer("42".into())));
    }
}
