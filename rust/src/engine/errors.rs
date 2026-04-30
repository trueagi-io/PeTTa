//! Streamlined error handling

use std::path::PathBuf;
use std::time::Duration;

pub trait Diagnostic: Send + Sync {
fn severity(&self) -> DiagSeverity;
fn message(&self) -> String;
fn code(&self) -> Option<String>;
fn suggestions(&self) -> Vec<String>;
fn location(&self) -> Option<DiagLocation>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum DiagSeverity {
#[default]
Error,
Warning,
Hint,
Info,
}

#[derive(Debug, Clone, Default)]
pub struct DiagLocation {
pub file: String,
pub line: u32,
pub column: u32,
}

#[derive(Debug, Clone, thiserror::Error)]
pub enum BackendErrorKind {
#[error("🔴 undefined function `{name}/{arity}`\n{context}{suggestion}")]
UndefinedFunction { name: String, arity: usize, context: String, suggestion: String },

#[error("🟡 type error: expected {expected}, got {found}{context}")]
TypeMismatch { expected: String, found: String, context: String },

#[error("🔴 syntax error: {detail}{location}")]
SyntaxError { location: String, line: Option<u32>, column: Option<u32>, detail: String },

#[error("🟡 variable is not bound{location}")]
UnboundVariable { location: String },

#[error("🟡 argument not instantiated{location}")]
UninstantiatedArgument { location: String },

#[error("🔴 permission denied: {operation} on {target}")]
PermissionDenied { operation: String, target: String },

#[error("🔴 {error_type} {term} does not exist")]
ExistenceError { error_type: String, term: String },

#[error("🔴 stack overflow{limit}")]
StackOverflow { limit: String },

#[error("🔴 evaluation error: {0}")]
EvaluationError(String),

#[error("🔴 {0}")]
Generic(String),
}

#[derive(Debug, thiserror::Error)]
pub enum PeTTaError {
#[error("File not found: {0}")]
FileNotFound(PathBuf),

#[error("Failed to spawn swipl: {0}")]
SpawnSwipl(String),

#[error("Path error: {0}")]
PathError(String),

#[error("Write error: {0}")]
WriteError(String),

#[error(transparent)]
BackendError(#[from] BackendErrorKind),

#[error("MORK error: {0}")]
MorkError(String),

#[error("SWI-Prolog version: {0}")]
SwiplVersionError(String),

#[error("Protocol error: {0}")]
ProtocolError(String),

#[error("IO error: {0}")]
Io(#[from] std::io::Error),

#[error("Query timed out after {0:?}")]
Timeout(Duration),

#[error("Subprocess crashed after {restarts} restart(s)")]
SubprocessCrashed { restarts: u32 },
}

pub(crate) fn parse_backend_error(raw: &str) -> BackendErrorKind {
match parse_error_kind(raw) {
Ok(kind) => kind,
Err(_) => BackendErrorKind::Generic(raw.lines().next().unwrap_or(raw).trim().to_string()),
}
}

fn extract_name_arity(raw: &str) -> Option<(String, usize)> {
for t in raw.split(&['(', ')', ',', '/']) {
let t = t.trim();
let p: Vec<&str> = t.split_whitespace().collect();
if p.len() == 2 {
if let Ok(a) = p[1].parse::<usize>() {
return Some((p[0].to_string(), a));
}
}
}
None
}

fn extract_between(s: &str, start: &str, end: &str) -> Option<String> {
let si = s.find(start)?;
let rest = &s[si + start.len()..];
let ei = rest.find(end)?;
Some(rest[..ei].to_string())
}

fn parse_json_error(raw: &str) -> Option<BackendErrorKind> {
if raw.trim_start().starts_with('{') {
if let Ok(v) = serde_json::from_str::<serde_json::Value>(raw) {
if let Some(obj) = v.as_object() {
let message: Option<String> = obj.get("message").and_then(|m| m.as_str()).map(String::from);
let raw_field: Option<String> = obj.get("raw").and_then(|m| m.as_str()).map(String::from);
let functor: Option<String> = obj.get("functor").and_then(|v| v.as_str()).map(String::from);
let name: Option<String> = obj.get("name").and_then(|v| v.as_str()).map(String::from);
let name_arity: Option<String> = obj.get("name_arity").and_then(|v| v.as_str()).map(String::from);
let suggestion_str: Option<String> = obj.get("suggestion").and_then(|v| v.as_str()).map(String::from);
let context_str: Option<String> = obj.get("context").and_then(|v| v.as_str()).map(String::from);

if let Some(kind) = obj.get("kind").and_then(|k| k.as_str()) {
if matches!(kind, "swipl" | "prolog") {
if let (Some(n), Some(a)) = (name.clone(), name_arity.clone()) {
if let Ok(arity) = a.parse::<usize>() {
let suggestion = suggestion_str.clone().map(|s| format!("\n💡 suggestion: did you mean `{s}`?")).unwrap_or_default();
let context = context_str.clone().map(|c| format!(" at {c}")).unwrap_or_default();
return Some(BackendErrorKind::UndefinedFunction { name: n, arity, context, suggestion });
}
}
if let Some(f) = functor.clone() {
if f.contains("syntax_error") {
return Some(BackendErrorKind::SyntaxError {
location: context_str.clone().map(|c| format!(" at {c}")).unwrap_or_default(),
line: None, column: None,
detail: message.clone().unwrap_or_else(|| "syntax error".into()),
});
}
if f.contains("existence_error") {
return Some(BackendErrorKind::ExistenceError { error_type: f, term: raw_field.clone().unwrap_or_default() });
}
}
}
if let Some(formal) = obj.get("formal").and_then(|v| v.as_str()) {
if let Ok(k) = parse_error_kind(formal) { return Some(k); }
}
let probe = raw_field.as_ref().map(|s| s.as_str()).or_else(|| raw_field.as_ref().map(|s| s.as_str())).unwrap_or(raw);
return parse_error_kind(probe).ok();
}
if let Some(msg) = message.or(raw_field) { return Some(BackendErrorKind::Generic(msg)); }
}
}
}
None
}

fn parse_error_kind(raw: &str) -> Result<BackendErrorKind, String> {
if let Some(kind) = parse_json_error(raw) { return Ok(kind); }

if raw.contains("existence_error") && raw.contains("procedure") {
if let Some((name, arity)) = extract_name_arity(raw) {
let suggestion = extract_between(raw, "Did you mean ", "?")
.map(|s| format!("\n💡 suggestion: did you mean `{s}`?")).unwrap_or_default();
let context = extract_between(raw, "in ", " at")
.map(|loc| format!(" at {loc}")).unwrap_or_default();
return Ok(BackendErrorKind::UndefinedFunction { name, arity, context, suggestion });
}
}

if raw.contains("type_error") {
let expected = extract_between(raw, "expected ", ",").unwrap_or_else(|| "unknown".into());
let found = extract_between(raw, "got ", ")").unwrap_or_else(|| "unknown".into());
let context = extract_between(raw, "in ", " at").map(|loc| format!(" at {loc}")).unwrap_or_default();
return Ok(BackendErrorKind::TypeMismatch { expected, found, context });
}

if raw.contains("syntax_error") {
let location = extract_between(raw, "line ", ":").map(|line| format!(" at line {line}")).unwrap_or_default();
let detail = extract_between(raw, "syntax_error(", ")").unwrap_or_else(|| "unknown".into());
return Ok(BackendErrorKind::SyntaxError { location, line: None, column: None, detail });
}

if raw.contains("instantiation_error") || raw.contains("uninstantiated") {
let location = extract_between(raw, "in ", " at").map(|loc| format!(" at {loc}")).unwrap_or_default();
return Ok(BackendErrorKind::UninstantiatedArgument { location });
}

if raw.contains("Stack depth") || raw.contains("stack_limit") {
let limit = extract_between(raw, "limit: ", ")").map(|l| format!(" (limit: {l})")).unwrap_or_default();
return Ok(BackendErrorKind::StackOverflow { limit });
}

if raw.contains("permission_error") {
let operation = extract_between(raw, "permission_error(", ",").unwrap_or_else(|| "unknown".into());
let target = extract_between(raw, ", ", ")").unwrap_or_else(|| "unknown".into());
return Ok(BackendErrorKind::PermissionDenied { operation, target });
}

if raw.contains("existence_error") {
let error_type = extract_between(raw, "existence_error(", ",").unwrap_or_else(|| "unknown".into());
let term = extract_between(raw, ", ", ")").unwrap_or_else(|| "unknown".into());
return Ok(BackendErrorKind::ExistenceError { error_type, term });
}

Err(raw.lines().next().unwrap_or(raw).trim().to_string())
}
