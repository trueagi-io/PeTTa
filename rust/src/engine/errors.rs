use std::path::PathBuf;
use std::time::Duration;

// Engine error parsing and mapping
//
// This module provides two main responsibilities:
// 1) A unified set of backend error kinds (BackendErrorKind) used across
//    PeTTa to represent errors produced by SWI-Prolog or other backends.
// 2) Utilities to parse raw error strings emitted by SWI-Prolog. When
//    available, the Prolog side may emit a small JSON object with fields
//    such as {"kind":"swipl","message":"...","raw":"...","functor":"...","name":"foo","name_arity":"foo/2","suggestion":"bar"}.
//
// parse_backend_error prefers structured JSON when present, then falls back
// to heuristic string matching for common Prolog error forms (existence_error,
// type_error, syntax_error, permission_error, stack overflow, etc.).
//
// The parsing functions strive to be conservative and non-panicking: if parsing
// fails, the raw message is returned inside BackendErrorKind::Generic.


/// Unified backend error kinds (canonical type for all backends).
#[derive(Debug, Clone, thiserror::Error)]
pub enum BackendErrorKind {
    #[error("MeTTa function '{name}/{arity}' is not defined{}", .suggestion.as_ref().map(|s| format!(". Did you mean '{}'?", s)).unwrap_or_default())]
    UndefinedFunction {
        name: String,
        arity: usize,
        suggestion: Option<String>,
    },

    #[error("Type error: expected {expected}, got {found}{}", .context.as_ref().map(|c| format!(" ({})", c)).unwrap_or_default())]
    TypeMismatch {
        expected: String,
        found: String,
        context: Option<String>,
    },

    #[error("Syntax error: {detail}")]
    SyntaxError {
        location: Option<String>,
        line: Option<u32>,
        column: Option<u32>,
        detail: String,
    },

    #[error("Variable is not bound")]
    UnboundVariable {
        location: Option<String>,
    },

    #[error("Argument is not sufficiently instantiated{}", .location.as_ref().map(|l| format!(" at {}", l)).unwrap_or_default())]
    UninstantiatedArgument {
        location: Option<String>,
    },

    #[error("Permission denied: {operation} on {target}")]
    PermissionDenied { operation: String, target: String },

    #[error("{error_type} {term} does not exist")]
    ExistenceError { error_type: String, term: String },

    #[error("Stack overflow{}", .limit.map(|l| format!(" (limit: {})", l)).unwrap_or_default())]
    StackOverflow { limit: Option<u32> },

    #[error("Evaluation error: {0}")]
    EvaluationError(String),

    #[error("{0}")]
    Generic(String),
}

/// Top-level error type for PeTTa operations.
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
        Err(msg) => BackendErrorKind::Generic(msg.lines().next().unwrap_or(raw).trim().to_string()),
    }
}



// ---------------------------------------------------------------------------
// Shared error kind parser
// ---------------------------------------------------------------------------

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
    // Quick check: must start with '{' (allow whitespace)
    if raw.trim_start().starts_with('{') {
        if let Ok(v) = serde_json::from_str::<serde_json::Value>(raw) {
            if let Some(obj) = v.as_object() {
                // message/raw fields
                let message = obj.get("message").and_then(|m| m.as_str()).map(|s| s.to_string());
                let raw_field = obj.get("raw").and_then(|m| m.as_str()).map(|s| s.to_string());
                // Additional structured fields we may emit from Prolog
                let functor = obj.get("functor").and_then(|v| v.as_str()).map(|s| s.to_string());
                let _functor_arity = obj.get("functor_arity").and_then(|v| v.as_u64()).map(|n| n as usize);
                let name = obj.get("name").and_then(|v| v.as_str()).map(|s| s.to_string());
                let name_arity = obj.get("name_arity").and_then(|v| v.as_str()).map(|s| s.to_string());
                let suggestion = obj.get("suggestion").and_then(|v| v.as_str()).map(|s| s.to_string());
                let context = obj.get("context").and_then(|v| v.as_str()).map(|s| s.to_string());
// kind-based parsing (for now treat as swipl-like)
    if let Some(kind) = obj.get("kind").and_then(|k| k.as_str())
        && matches!(kind, "swipl" | "prolog")
    {
        // Use explicit structured fields when present to construct
        // precise BackendErrorKind variants.
        if let (Some(n), Some(a)) = (name.clone(), name_arity.clone()) {
            // Try to parse name_arity as an arity number
            if let Ok(arity) = a.parse::<usize>() {
                return Some(BackendErrorKind::UndefinedFunction { name: n, arity, suggestion });
            }
        }
        if let Some(f) = functor.clone() {
            // Map some known functors heuristically
            if f.contains("syntax_error") {
                return Some(BackendErrorKind::SyntaxError {
                    location: context.clone(),
                    line: None,
                    column: None,
                    detail: message.clone().unwrap_or_else(|| "syntax error".into()),
                });
            }
            if f.contains("existence_error") {
                return Some(BackendErrorKind::ExistenceError {
                    error_type: f,
                    term: raw_field.clone().unwrap_or_default(),
                });
            }
        }
        // Prefer the 'formal' or 'raw' fields if available; fall back to message.
        if let Some(formal) = obj.get("formal").and_then(|v| v.as_str()) {
            // Try parsing the formal representation first
            if let Ok(k) = parse_error_kind(formal) {
                return Some(k);
            }
        }
        let probe = raw_field.as_deref().or(message.as_deref()).unwrap_or(raw);
        return parse_error_kind(probe).ok();
    }
                // fallback: use message/raw or raw text
                if let Some(msg) = message.or(raw_field) {
                    return Some(BackendErrorKind::Generic(msg));
                }
            }
        }
    }
    None
}

fn parse_error_kind(raw: &str) -> Result<BackendErrorKind, String> {
    // Try JSON structured error first. Prolog may emit a JSON object with
    // deterministic fields when available. Example: {"kind":"swipl","message":"...","raw":"..."}
    if let Some(kind) = parse_json_error(raw) {
        return Ok(kind);
    }
    if raw.contains("existence_error") && raw.contains("procedure") {
        if let Some((name, arity)) = extract_name_arity(raw) {
            return Ok(BackendErrorKind::UndefinedFunction { name, arity, suggestion: None });
        }
    }
    if raw.contains("type_error") {
        return Ok(BackendErrorKind::TypeMismatch {
            expected: "unknown".into(),
            found: "unknown".into(),
            context: None,
        });
    }
    if raw.contains("syntax_error") {
        return Ok(BackendErrorKind::SyntaxError {
            location: extract_between(raw, "line ", ":"),
            line: extract_between(raw, "line ", ":").and_then(|s| s.parse().ok()),
            column: extract_between(raw, ":", "(").and_then(|s| s.parse().ok()),
            detail: extract_between(raw, "syntax_error(", ")").unwrap_or_else(|| "unknown".into()),
        });
    }
    if raw.contains("instantiation_error") {
        return Ok(BackendErrorKind::UninstantiatedArgument {
            location: extract_between(raw, "in ", " at"),
        });
    }
    if raw.contains("uninstantiated") {
        return Ok(BackendErrorKind::UnboundVariable {
            location: extract_between(raw, "in ", " at"),
        });
    }
    if raw.contains("Stack depth") || raw.contains("stack_limit") {
        let limit = extract_between(raw, "limit: ", ")").and_then(|s| s.parse().ok());
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
