use std::path::PathBuf;
use std::time::Duration;

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

    #[error("Variable {var} is not bound")]
    UnboundVariable {
        var: String,
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

/// SWI-Prolog-specific error kinds parsed from raw error messages.
#[derive(Debug, Clone, thiserror::Error)]
#[deprecated(since = "0.6.0", note = "Use BackendErrorKind instead for unified error handling")]
pub enum SwiplErrorKind {
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
        line: Option<u32>,
        column: Option<u32>,
        detail: String,
    },

    #[error("Argument is not sufficiently instantiated{}", .location.as_ref().map(|l| format!(" at {}", l)).unwrap_or_default())]
    UninstantiatedArgument { location: Option<String> },

    #[error("Permission denied: {operation} on {target}")]
    PermissionDenied { operation: String, target: String },

    #[error("{error_type} {term} does not exist")]
    ExistenceError { error_type: String, term: String },

    #[error("Stack overflow{}", .limit.map(|l| format!(" (limit: {})", l)).unwrap_or_default())]
    StackOverflow { limit: Option<u32> },

    #[error("{0}")]
    Generic(String),
}

#[allow(deprecated)]
impl From<BackendErrorKind> for SwiplErrorKind {
    fn from(kind: BackendErrorKind) -> Self {
        match kind {
            BackendErrorKind::UndefinedFunction { name, arity, suggestion } => SwiplErrorKind::UndefinedFunction { name, arity, suggestion },
            BackendErrorKind::TypeMismatch { expected, found, context } => SwiplErrorKind::TypeMismatch { expected, found, context },
            BackendErrorKind::SyntaxError { line, column, detail, .. } => SwiplErrorKind::SyntaxError { line, column, detail },
            BackendErrorKind::UninstantiatedArgument { location } => SwiplErrorKind::UninstantiatedArgument { location },
            BackendErrorKind::PermissionDenied { operation, target } => SwiplErrorKind::PermissionDenied { operation, target },
            BackendErrorKind::ExistenceError { error_type, term } => SwiplErrorKind::ExistenceError { error_type, term },
            BackendErrorKind::StackOverflow { limit } => SwiplErrorKind::StackOverflow { limit },
            BackendErrorKind::EvaluationError(msg) => SwiplErrorKind::Generic(msg),
            BackendErrorKind::UnboundVariable { var, location } => SwiplErrorKind::UninstantiatedArgument { location },
            BackendErrorKind::Generic(msg) => SwiplErrorKind::Generic(msg),
        }
    }
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

#[deprecated(since = "0.6.0", note = "Use parse_backend_error instead")]
#[allow(deprecated)]
pub(crate) fn parse_swipl_error(raw: &str) -> SwiplErrorKind {
    parse_backend_error(raw).into()
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

fn parse_error_kind(raw: &str) -> Result<BackendErrorKind, String> {
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
            var: "unknown".into(),
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