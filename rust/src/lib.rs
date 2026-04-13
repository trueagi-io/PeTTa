//! PeTTa - MeTTa language implementation in Rust + persistent SWI-Prolog subprocess
//!
//! This crate embeds SWI-Prolog as a persistent subprocess using a binary
//! length-prefixed protocol over stdin/stdout pipes. SWI is loaded once at
//! startup, and all queries go through the same process — no startup overhead.
//!
//! # Protocol
//! ```text
//! Request (Rust → Prolog):
//!   [1 byte: type]  'F'(70) = file, 'S'(83) = string, 'Q'(81) = quit
//!   [4 bytes: payload length, big-endian u32]
//!   [N bytes: UTF-8 payload]
//!
//! Response (Prolog → Rust):
//!   [1 byte: status]  0 = success, 1 = error
//!   If 0: [4 bytes: result count] then for each: [4 bytes: str len][N bytes: UTF-8]
//!   If 1: [4 bytes: error msg len][N bytes: UTF-8 error]
//! ```

#![allow(clippy::collapsible_if)]
#![allow(clippy::useless_conversion)]
// MORK feature flags - uncomment when mork feature is enabled in Cargo.toml
// #![cfg_attr(feature = "mork", allow(internal_features))]
// #![cfg_attr(feature = "mork", feature(core_intrinsics))]
// #![cfg_attr(feature = "mork", feature(portable_simd))]
// #![cfg_attr(feature = "mork", feature(allocator_api))]
// #![cfg_attr(feature = "mork", feature(coroutine_trait))]
// #![cfg_attr(feature = "mork", feature(coroutines))]
// #![cfg_attr(feature = "mork", feature(stmt_expr_attributes))]
// #![cfg_attr(feature = "mork", feature(gen_blocks))]
// #![cfg_attr(feature = "mork", feature(yield_expr))]

use std::io::{BufReader, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::time::{Duration, Instant};

use tracing::{debug, info, trace, warn};

pub mod petta_parser;
pub mod profiler;

/// Configuration options for the PeTTaEngine.
#[derive(Debug, Clone)]
pub struct EngineConfig {
    /// Path to the SWI-Prolog binary (defaults to "swipl").
    pub swipl_path: PathBuf,
    /// Source directory containing .pl files (defaults to `<project_root>/src`).
    pub src_dir: Option<PathBuf>,
    /// Enable verbose debug output from Prolog.
    pub verbose: bool,
    /// Enable query profiling.
    pub profile: bool,
    /// Timeout for query execution. None means no timeout.
    pub query_timeout: Option<Duration>,
    /// Maximum number of automatic restarts on subprocess crash (default: 0).
    pub max_restarts: u32,
    /// Minimum SWI-Prolog version required (major, minor).
    pub min_swipl_version: (u32, u32),
}

impl Default for EngineConfig {
    fn default() -> Self {
        Self {
            swipl_path: PathBuf::from("swipl"),
            src_dir: None,
            verbose: false,
            profile: false,
            query_timeout: None,
            max_restarts: 0,
            min_swipl_version: (9, 3),
        }
    }
}

impl EngineConfig {
    /// Create a new config with the given project root.
    /// The Prolog source files are expected to be in `<project_root>/prolog/`.
    pub fn new(project_root: &Path) -> Self {
        Self {
            src_dir: Some(project_root.join("prolog")),
            ..Default::default()
        }
    }

    /// Set the SWI-Prolog binary path.
    pub fn swipl_path(mut self, path: impl Into<PathBuf>) -> Self {
        self.swipl_path = path.into();
        self
    }

    /// Set the source directory for .pl files.
    pub fn src_dir(mut self, dir: impl Into<PathBuf>) -> Self {
        self.src_dir = Some(dir.into());
        self
    }

    /// Enable or disable verbose Prolog debug output.
    /// Enable or disable query profiling.
    pub fn profile(mut self, p: bool) -> Self {
        self.profile = p;
        self
    }

    pub fn verbose(mut self, v: bool) -> Self {
        self.verbose = v;
        self
    }

    /// Set a timeout for queries.
    pub fn query_timeout(mut self, timeout: Duration) -> Self {
        self.query_timeout = Some(timeout);
        self
    }

    /// Set the maximum number of automatic restarts on crash.
    pub fn max_restarts(mut self, n: u32) -> Self {
        self.max_restarts = n;
        self
    }
}

// ---------------------------------------------------------------------------
// Prolog server source
// ---------------------------------------------------------------------------

fn build_server_source(src_dir: &Path, verbose: bool) -> Result<String, PeTTaError> {
    let silent = if verbose { "false" } else { "true" };
    let mut src = String::new();

    src.push_str(":- initialization(server_main).\n");
    src.push_str(":- set_prolog_flag(verbose, silent).\n");
    src.push_str(":- style_check(-singleton).\n");
    src.push_str(":- style_check(-discontiguous).\n");
    src.push_str(":- discontiguous throw_dcg_expansion_error/1.\n");
    src.push_str(&format!(":- assertz(silent({})).\n", silent));

    for file in &[
        "parser.pl",
        "spaces.pl",
        "specializer.pl",
        "translator.pl",
        "filereader.pl",
        "utils.pl",
        "metta.pl",
    ] {
        let fpath = src_dir.join(file);
        if !fpath.exists() {
            return Err(PeTTaError::FileNotFound(fpath));
        }
        let fstr = fpath.to_string_lossy().replace('\\', "\\\\");
        src.push_str(&format!(":- consult('{}').\n", fstr));
        trace!("Consulting Prolog file: {}", fpath.display());
    }

    // filereader.pl asserts silent(false) at startup; re-assert our setting after all consults
    src.push_str(&format!(
        ":- retractall(silent(_)), assertz(silent({})).\n",
        silent
    ));

    // Initialize type lookup cache
    src.push_str(":- nb_setval(fun_types, []).\n");

    src.push_str(SERVER_LOOP);
    debug!("Built Prolog server source ({} bytes)", src.len());
    Ok(src)
}

const SERVER_LOOP: &str = r#"
server_main :-
    set_stream(user_input, type(binary)),
    set_stream(user_output, type(binary)),
    put_byte(user_output, 255),
    flush_output(user_output),
    server_loop.

server_loop :-
    get_byte(user_input, Type),
    ( Type =:= -1 -> halt
    ; Type =:= 81 -> halt
    ; Type =:= 67 -> handle_cancel
    ; read_u32(Len),
      read_bytes_fast(Len, Bytes),
      string_codes(Query, Bytes),
      execute_query(Type, Query, Results),
      write_response(Results),
      flush_output(user_output),
      server_loop
    ).

handle_cancel :-
    abort.

execute_query(70, Query, Results) :-  % 'F' = load_metta_file
    with_output_to(user_error, catch(load_metta_file(Query, Results), _, Results=[])).
execute_query(83, Query, Results) :-  % 'S' = process_metta_string
    with_output_to(user_error, catch(process_metta_string(Query, Results), _, Results=[])).
execute_query(_, _, Results) :- Results = [].

read_u32(V) :-
    get_byte(user_input,B0), get_byte(user_input,B1),
    get_byte(user_input,B2), get_byte(user_input,B3),
    V is (B0<<24) \/ (B1<<16) \/ (B2<<8) \/ B3.

write_u32(V) :-
    B0 is (V>>24) /\ 255, B1 is (V>>16) /\ 255,
    B2 is (V>>8)  /\ 255, B3 is V /\ 255,
    put_byte(user_output,B0), put_byte(user_output,B1),
    put_byte(user_output,B2), put_byte(user_output,B3).

%Bulk-read payload using efficient tail-recursive loop:
read_bytes_fast(0, []) :- !.
read_bytes_fast(N, Bytes) :-
    read_bytes_tail(N, Bytes).

read_bytes_tail(N, Bytes) :-
    read_bytes_loop(N, [], Rev),
    reverse(Rev, Bytes).

read_bytes_loop(0, Acc, Acc) :- !.
read_bytes_loop(N, Acc, Result) :-
    get_code(user_input, Code),
    ( Code =:= -1 -> !, reverse(Acc, Result)
    ; N1 is N - 1,
      read_bytes_loop(N1, [Code|Acc], Result) ).

%Tail-recursive byte writing:
write_bytes(Codes) :- write_bytes_loop(Codes).

write_bytes_loop([]) :- !.
write_bytes_loop([C|Cs]) :-
    put_byte(user_output, C),
    write_bytes_loop(Cs).

write_response(Results) :-
    length(Results, Count),
    put_byte(user_output, 0),
    write_u32(Count),
    maplist(write_result_str, Results).

write_error_response(Error) :-
    put_byte(user_output, 1),
    ( compound(Error) -> format(atom(Msg), '~q', [Error])
    ; atom(Error)     -> Msg = Error
    ; format(atom(Msg), '~w', [Error]) ),
    atom_codes(Msg, Codes),
    length(Codes, Len),
    write_u32(Len),
    write_bytes(Codes).

%Single-pass result serialization: DCG directly produces codes, skip atom intermediate:
write_result_str(Term) :-
    phrase(swrite_exp(Term), Codes),
    length(Codes, Len),
    write_u32(Len),
    write_bytes(Codes).

%Cached type lookup: avoids repeated match/4 calls for type declarations.
%Types are cached in a list of Fun-TypeChains pairs, invalidated when atoms are added/removed.
get_fun_types(Fun, TypeChains) :-
    catch(nb_getval(fun_types, Cache), _, fail),
    member(Fun-TypeChains, Cache), !.
get_fun_types(Fun, TypeChains) :-
    findall(TypeChain, catch(match('&self', [':', Fun, TypeChain], TypeChain, TypeChain), _, fail), TypeChains),
    ( catch(nb_getval(fun_types, Cache), _, fail)
      -> nb_setval(fun_types, [Fun-TypeChains|Cache])
      ; nb_setval(fun_types, [Fun-TypeChains]) ).

invalidate_type_cache :- nb_setval(fun_types, []).
"#;

// ---------------------------------------------------------------------------
// Typed MeTTa values
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq)]
pub enum MettaValue {
    Integer(String),
    Float(f64),
    Bool(bool),
    Atom(String),
    List(Vec<MettaValue>),
    Expression(String, Vec<MettaValue>),
}

impl MettaValue {
    pub fn parse(s: &str) -> Option<Self> {
        parse_sexpr(s.trim())
    }
}

fn parse_sexpr(s: &str) -> Option<MettaValue> {
    let s = s.trim();
    if s.is_empty() {
        return None;
    }
    if s == "true" {
        return Some(MettaValue::Bool(true));
    }
    if s == "false" {
        return Some(MettaValue::Bool(false));
    }
    if s.parse::<i64>().is_ok() {
        return Some(MettaValue::Integer(s.to_string()));
    }
    if let Ok(f) = s.parse::<f64>() {
        return Some(MettaValue::Float(f));
    }
    if !s.starts_with('(') {
        return Some(MettaValue::Atom(s.to_string()));
    }
    if let Some(inner) = strip_parens(s) {
        let items = split_top_level(inner);
        if items.is_empty() {
            return Some(MettaValue::List(Vec::new()));
        }
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
    } else {
        None
    }
}

fn is_balanced(s: &str) -> bool {
    let mut d = 0i32;
    for c in s.chars() {
        match c {
            '(' => d += 1,
            ')' => {
                d -= 1;
                if d < 0 {
                    return false;
                }
            }
            _ => {}
        }
    }
    d == 0
}

fn split_top_level(s: &str) -> Vec<String> {
    let mut items = Vec::new();
    let mut cur = String::new();
    let (mut depth, mut in_q, mut esc) = (0i32, false, false);
    for ch in s.chars() {
        if esc {
            cur.push(ch);
            esc = false;
            continue;
        }
        if ch == '\\' {
            esc = true;
            cur.push(ch);
            continue;
        }
        if ch == '"' {
            in_q = !in_q;
            cur.push(ch);
            continue;
        }
        if in_q {
            cur.push(ch);
            continue;
        }
        match ch {
            '(' => {
                depth += 1;
                cur.push(ch);
            }
            ')' => {
                depth -= 1;
                cur.push(ch);
            }
            ' ' | '\t' | '\n' | '\r' if depth == 0 => {
                if !cur.is_empty() {
                    items.push(std::mem::take(&mut cur));
                }
            }
            _ => cur.push(ch),
        }
    }
    if !cur.is_empty() {
        items.push(cur);
    }
    items
}

// ---------------------------------------------------------------------------
// MeTTa result
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MettaResult {
    pub value: String,
}

impl MettaResult {
    pub fn parsed_value(&self) -> Option<MettaValue> {
        MettaValue::parse(&self.value)
    }
}

// ---------------------------------------------------------------------------
// Error types (thiserror)
// ---------------------------------------------------------------------------

/// SWI-Prolog-specific error kinds parsed from raw error messages.
#[derive(Debug, Clone, thiserror::Error)]
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
    SwiplError(#[from] SwiplErrorKind),

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

pub(crate) fn parse_swipl_error(raw: &str) -> SwiplErrorKind {
    if raw.contains("existence_error") && raw.contains("procedure") {
        if let Some((name, arity)) = extract_name_arity(raw) {
            return SwiplErrorKind::UndefinedFunction {
                name,
                arity,
                suggestion: None,
            };
        }
    }
    if raw.contains("type_error") {
        return SwiplErrorKind::TypeMismatch {
            expected: "unknown".into(),
            found: "unknown".into(),
            context: None,
        };
    }
    if raw.contains("syntax_error") {
        return SwiplErrorKind::SyntaxError {
            line: None,
            column: None,
            detail: extract_between(raw, "syntax_error(", ")").unwrap_or_else(|| "unknown".into()),
        };
    }
    if raw.contains("instantiation_error") {
        return SwiplErrorKind::UninstantiatedArgument {
            location: extract_between(raw, "in ", " at line"),
        };
    }
    if raw.contains("Stack depth") || raw.contains("stack_limit") {
        return SwiplErrorKind::StackOverflow { limit: None };
    }
    SwiplErrorKind::Generic(raw.lines().next().unwrap_or(raw).trim().to_string())
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

// ---------------------------------------------------------------------------
// Persistent engine
// ---------------------------------------------------------------------------

pub struct PeTTaEngine {
    child: Option<Child>,
    stdin_pipe: Option<std::process::ChildStdin>,
    stdout_pipe: Option<BufReader<std::process::ChildStdout>>,
    stderr_output: std::sync::Arc<std::sync::Mutex<Vec<u8>>>,
    config: EngineConfig,
    restart_count: u32,
}

impl PeTTaEngine {
    /// Create a new engine with default config for the given project root.
    pub fn new(project_root: &Path, verbose: bool) -> Result<Self, PeTTaError> {
        let config = EngineConfig::new(project_root).verbose(verbose);
        Self::with_config(&config)
    }

    /// Create a new engine with a custom configuration.
    pub fn with_config(config: &EngineConfig) -> Result<Self, PeTTaError> {
        info!(
            "Creating PeTTaEngine (swipl={}, verbose={}, max_restarts={})",
            config.swipl_path.display(),
            config.verbose,
            config.max_restarts
        );

        let src_dir = config
            .src_dir
            .as_ref()
            .ok_or_else(|| PeTTaError::PathError("No source directory configured".into()))?;

        if !src_dir.exists() {
            return Err(PeTTaError::FileNotFound(src_dir.clone()));
        }

        check_swipl_version(&config.swipl_path, config.min_swipl_version)?;

        let server_source = build_server_source(src_dir, config.verbose)?;
        let tmp = tempfile::Builder::new()
            .prefix("petta_srv_")
            .suffix(".pl")
            .tempfile()
            .map_err(|e| PeTTaError::WriteError(e.to_string()))?;
        let tmp_path = tmp.path().to_path_buf();
        std::fs::write(&tmp_path, &server_source)
            .map_err(|e| PeTTaError::WriteError(e.to_string()))?;

        debug!("Launching SWI-Prolog subprocess: {:?}", config.swipl_path);
        let mut child = Command::new(&config.swipl_path)
            .arg("-q")
            .arg("-t")
            .arg("halt")
            .arg(&tmp_path)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .map_err(|e| PeTTaError::SpawnSwipl(e.to_string()))?;

        let stderr = child.stderr.take();
        let stderr_output = std::sync::Arc::new(std::sync::Mutex::new(Vec::new()));
        let stderr_output_clone = std::sync::Arc::clone(&stderr_output);
        std::thread::spawn(move || {
            if let Some(mut s) = stderr {
                let mut buf = [0u8; 4096];
                while let Ok(n) = s.read(&mut buf) {
                    if n == 0 {
                        break;
                    }
                    trace!(
                        "Prolog stderr: {}",
                        String::from_utf8_lossy(&buf[..n])
                    );
                    stderr_output_clone
                        .lock()
                        .unwrap()
                        .extend_from_slice(&buf[..n]);
                }
            }
        });

        let stdin = child
            .stdin
            .take()
            .ok_or_else(|| PeTTaError::SpawnSwipl("no stdin".into()))?;
        let stdout = BufReader::new(
            child
                .stdout
                .take()
                .ok_or_else(|| PeTTaError::SpawnSwipl("no stdout".into()))?,
        );
        std::mem::forget(tmp);

        let mut engine = Self {
            child: Some(child),
            stdin_pipe: Some(stdin),
            stdout_pipe: Some(stdout),
            stderr_output,
            config: config.clone(),
            restart_count: 0,
        };

        // Wait for the ready signal (0xFF), discarding any startup warnings
        engine.wait_for_ready()?;

        info!("PeTTaEngine initialized successfully");
        Ok(engine)
    }

    fn wait_for_ready(&mut self) -> Result<(), PeTTaError> {
        trace!("Waiting for Prolog ready signal (0xFF)");
        let reader = self.stdout_pipe.as_mut().ok_or_else(|| {
            PeTTaError::ProtocolError("stdout pipe unavailable".into())
        })?;
        loop {
            let mut b = [0u8; 1];
            reader.read_exact(&mut b).map_err(|e| {
                PeTTaError::ProtocolError(format!("failed to read ready signal: {}", e))
            })?;
            if b[0] == 0xFF {
                debug!("Prolog ready signal received");
                return Ok(());
            }
            trace!("Discarding startup byte: {:?}", b[0]);
        }
    }

    fn read_u32(&mut self) -> Result<u32, PeTTaError> {
        let mut b = [0u8; 4];
        let reader = self.stdout_pipe.as_mut().ok_or_else(|| {
            PeTTaError::ProtocolError("stdout pipe unavailable".into())
        })?;
        reader.read_exact(&mut b).map_err(|e| {
            if e.kind() == std::io::ErrorKind::UnexpectedEof {
                PeTTaError::ProtocolError("child closed".into())
            } else {
                PeTTaError::ProtocolError(e.to_string())
            }
        })?;
        Ok(u32::from_be_bytes(b))
    }

    /// Check if the Prolog subprocess is alive and responsive.
    pub fn is_alive(&mut self) -> bool {
        self.stdin_pipe.is_some() && self.stdout_pipe.is_some()
    }

    /// Attempt to recover from a protocol error by restarting the subprocess and retrying the query.
    fn try_recover_and_retry<F, T>(&mut self, query_fn: F) -> Result<T, PeTTaError>
    where
        F: FnOnce(&mut Self) -> Result<T, PeTTaError>,
    {
        warn!("Attempting to recover from subprocess failure");
        self.restart()?;
        query_fn(self)
    }

    /// Restart the Prolog subprocess after a crash.
    #[allow(dead_code)]
    fn restart(&mut self) -> Result<(), PeTTaError> {
        if self.restart_count >= self.config.max_restarts {
            return Err(PeTTaError::SubprocessCrashed {
                restarts: self.restart_count,
            });
        }

        warn!(
            "Restarting Prolog subprocess (attempt {}/{})",
            self.restart_count + 1,
            self.config.max_restarts
        );

        // Clean up old child process
        if let Some(mut child) = self.child.take() {
            let _ = child.kill();
            let _ = child.wait();
        }

        // Reinitialize pipes and restart
        let src_dir = self
            .config
            .src_dir
            .as_ref()
            .ok_or_else(|| PeTTaError::PathError("No source directory configured".into()))?;

        let server_source = build_server_source(src_dir, self.config.verbose)?;
        let tmp = tempfile::Builder::new()
            .prefix("petta_srv_")
            .suffix(".pl")
            .tempfile()
            .map_err(|e| PeTTaError::WriteError(e.to_string()))?;
        let tmp_path = tmp.path().to_path_buf();
        std::fs::write(&tmp_path, &server_source)
            .map_err(|e| PeTTaError::WriteError(e.to_string()))?;

        let mut child = Command::new(&self.config.swipl_path)
            .arg("-q")
            .arg("-t")
            .arg("halt")
            .arg(&tmp_path)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .map_err(|e| PeTTaError::SpawnSwipl(e.to_string()))?;

        let stderr = child.stderr.take();
        let stderr_output = std::sync::Arc::new(std::sync::Mutex::new(Vec::new()));
        let stderr_output_clone = std::sync::Arc::clone(&stderr_output);
        std::thread::spawn(move || {
            if let Some(mut s) = stderr {
                let mut buf = [0u8; 4096];
                while let Ok(n) = s.read(&mut buf) {
                    if n == 0 {
                        break;
                    }
                    stderr_output_clone
                        .lock()
                        .unwrap()
                        .extend_from_slice(&buf[..n]);
                }
            }
        });

        self.stdin_pipe = Some(
            child
                .stdin
                .take()
                .ok_or_else(|| PeTTaError::SpawnSwipl("no stdin".into()))?,
        );
        self.stdout_pipe = Some(BufReader::new(
            child
                .stdout
                .take()
                .ok_or_else(|| PeTTaError::SpawnSwipl("no stdout".into()))?,
        ));
        self.child = Some(child);
        self.stderr_output = stderr_output;
        self.restart_count += 1;

        std::mem::forget(tmp);
        self.wait_for_ready()?;

        info!("Prolog subprocess restarted successfully");
        Ok(())
    }

    fn send_query(
        &mut self,
        query_type: u8,
        payload: &str,
    ) -> Result<Vec<MettaResult>, PeTTaError> {
        trace!(
            "Sending query: type={}, payload_len={}",
            query_type,
            payload.len()
        );

        match self.send_query_inner(query_type, payload) {
            Ok(results) => Ok(results),
            Err(PeTTaError::ProtocolError(ref msg)) if msg.contains("child closed") => {
                warn!("Subprocess crashed during query, attempting recovery");
                self.try_recover_and_retry(|e| e.send_query_inner(query_type, payload))
            }
            Err(e) => Err(e),
        }
    }

    fn send_query_inner(
        &mut self,
        query_type: u8,
        payload: &str,
    ) -> Result<Vec<MettaResult>, PeTTaError> {
        let pb = payload.as_bytes();
        let len = pb.len() as u32;
        let sin = self.stdin_pipe.as_mut().ok_or_else(|| {
            PeTTaError::ProtocolError("stdin pipe unavailable".into())
        })?;
        sin.write_all(&[query_type])
            .map_err(|e| PeTTaError::WriteError(e.to_string()))?;
        sin.write_all(&len.to_be_bytes())
            .map_err(|e| PeTTaError::WriteError(e.to_string()))?;
        sin.write_all(pb)
            .map_err(|e| PeTTaError::WriteError(e.to_string()))?;
        sin.flush()
            .map_err(|e| PeTTaError::WriteError(e.to_string()))?;

        let status = {
            let mut b = [0u8; 1];
            let reader = self.stdout_pipe.as_mut().ok_or_else(|| {
                PeTTaError::ProtocolError("stdout pipe unavailable".into())
            })?;
            reader
                .read_exact(&mut b)
                .map_err(|e| {
                    if e.kind() == std::io::ErrorKind::UnexpectedEof {
                        PeTTaError::ProtocolError("child closed".into())
                    } else {
                        PeTTaError::ProtocolError(e.to_string())
                    }
                })?;
            b[0]
        };

        match status {
            0 => {
                let count = self.read_u32()?;
                trace!("Query succeeded: {} result(s)", count);
                let mut results = Vec::with_capacity(count as usize);
                for _ in 0..count {
                    let len = self.read_u32()?;
                    let mut buf = vec![0u8; len as usize];
                    let reader = self.stdout_pipe.as_mut().ok_or_else(|| {
                        PeTTaError::ProtocolError("stdout pipe unavailable".into())
                    })?;
                    reader
                        .read_exact(&mut buf)
                        .map_err(|e| PeTTaError::ProtocolError(e.to_string()))?;
                    let value = String::from_utf8(buf)
                        .map_err(|e| PeTTaError::ProtocolError(e.to_string()))?;
                    results.push(MettaResult { value });
                }
                Ok(results)
            }
            1 => {
                let len = self.read_u32()?;
                let mut buf = vec![0u8; len as usize];
                let reader = self.stdout_pipe.as_mut().ok_or_else(|| {
                    PeTTaError::ProtocolError("stdout pipe unavailable".into())
                })?;
                reader
                    .read_exact(&mut buf)
                    .map_err(|e| PeTTaError::ProtocolError(e.to_string()))?;
                let msg = String::from_utf8_lossy(&buf).to_string();
                debug!("Prolog error response: {}", msg);
                Err(PeTTaError::SwiplError(parse_swipl_error(&msg).into()))
            }
            _ => Err(PeTTaError::ProtocolError(format!(
                "unknown status: {}",
                status
            ))),
        }
    }

    pub fn load_metta_file(&mut self, file_path: &Path) -> Result<Vec<MettaResult>, PeTTaError> {
        let abs = file_path
            .canonicalize()
            .map_err(|e| PeTTaError::PathError(e.to_string()))?;
        if !abs.exists() {
            return Err(PeTTaError::FileNotFound(abs));
        }
        debug!("Loading MeTTa file: {}", abs.display());
        self.send_query(b'F', &abs.to_string_lossy())
    }

    pub fn load_metta_files(
        &mut self,
        file_paths: &[&Path],
    ) -> Result<Vec<MettaResult>, PeTTaError> {
        if file_paths.is_empty() {
            return Ok(Vec::new());
        }
        debug!("Loading {} MeTTa files", file_paths.len());
        let combined: String = file_paths
            .iter()
            .map(|p| {
                let abs = p
                    .canonicalize()
                    .map_err(|e| PeTTaError::PathError(e.to_string()))?;
                if !abs.exists() {
                    return Err(PeTTaError::FileNotFound(abs));
                }
                std::fs::read_to_string(&abs).map_err(|e| PeTTaError::PathError(e.to_string()))
            })
            .collect::<Result<Vec<String>, PeTTaError>>()?
            .join("\n");
        self.process_metta_string(&combined)
    }

    pub fn process_metta_string(
        &mut self,
        metta_code: &str,
    ) -> Result<Vec<MettaResult>, PeTTaError> {
        debug!("Processing MeTTa string ({} bytes)", metta_code.len());
        self.send_query(b'S', metta_code)
    }

    pub fn stderr_output(&self) -> String {
        let data = self
            .stderr_output
            .lock()
            .unwrap_or_else(|e| panic!("mutex poisoned: {}", e));
        String::from_utf8_lossy(&data).to_string()
    }

    /// Returns the current configuration.
    pub fn config(&self) -> &EngineConfig {
        &self.config
    }

    /// Returns the number of times the subprocess has been restarted.
    pub fn restart_count(&self) -> u32 {
        self.restart_count
    }

    pub fn shutdown(&mut self) {
        info!("Shutting down PeTTaEngine");
        if let Some(sin) = self.stdin_pipe.as_mut() {
            let _ = sin.write_all(&[b'Q', 0, 0, 0, 0]);
            let _ = sin.flush();
        }
        if let Some(mut child) = self.child.take() {
            let _ = child.wait();
        }
    }
    // Profiling methods
    // -----------------------------------------------------------------------

    /// Execute a query with profiling enabled. Returns the results and profile data.
    pub fn process_metta_string_profiled(
        &mut self,
        metta_code: &str,
    ) -> Result<(Vec<MettaResult>, profiler::QueryProfile), PeTTaError> {
        let mut profile = profiler::QueryProfile::new("process_metta_string", metta_code.len());
        let total_start = Instant::now();

        let send_start = Instant::now();
        let results = self.send_query(b'S', metta_code);
        profile.serialization_time = send_start.elapsed();

        let parse_start = Instant::now();
        let results = results?;
        profile.parse_time = parse_start.elapsed();

        profile.round_trip_time = profile.serialization_time;
        profile.total_time = total_start.elapsed();
        profile.result_count = results.len();

        if self.config.profile {
            info!("{}", profile.summary());
        }

        Ok((results, profile))
    }

    /// Execute a file query with profiling enabled.
    pub fn load_metta_file_profiled(
        &mut self,
        file_path: &Path,
    ) -> Result<(Vec<MettaResult>, profiler::QueryProfile), PeTTaError> {
        let abs = file_path
            .canonicalize()
            .map_err(|e| PeTTaError::PathError(e.to_string()))?;
        if !abs.exists() {
            return Err(PeTTaError::FileNotFound(abs));
        }
        let path_str = abs.to_string_lossy();
        let mut profile = profiler::QueryProfile::new("load_metta_file", path_str.len());
        let total_start = Instant::now();

        let send_start = Instant::now();
        let results = self.send_query(b'F', &path_str);
        profile.serialization_time = send_start.elapsed();

        let parse_start = Instant::now();
        let results = results?;
        profile.parse_time = parse_start.elapsed();

        profile.round_trip_time = profile.serialization_time;
        profile.total_time = total_start.elapsed();
        profile.result_count = results.len();

        if self.config.profile {
            info!("{}", profile.summary());
        }

        Ok((results, profile))
    }

    // -----------------------------------------------------------------------
    // Parallel batch execution (requires 'parallel' feature)
    // -----------------------------------------------------------------------

    /// Execute multiple independent MeTTa strings in parallel using rayon.
    /// Each string gets its own engine instance for true parallelism.
    #[cfg(feature = "parallel")]
    /// Execute multiple independent MeTTa strings in parallel using rayon.
    /// Each string gets its own engine instance for true parallelism.
    #[cfg(feature = "parallel")]
    pub fn process_metta_strings_parallel(
        &self,
        queries: &[&str],
    ) -> Vec<Result<Vec<MettaResult>, PeTTaError>> {
        use rayon::prelude::*;
        use std::sync::Arc;

        let config = Arc::new(self.config.clone());

        queries.par_iter().map(|&q| {
            let cfg = Arc::clone(&config);
            let mut c = (*cfg).clone();
            c.verbose = false;
            let mut engine = PeTTaEngine::with_config(&c)?;
            engine.process_metta_string(q)
        }).collect()
    }

    /// Execute multiple independent MeTTa files in parallel using rayon.
    #[cfg(feature = "parallel")]
    pub fn load_metta_files_parallel(
        &self,
        file_paths: &[PathBuf],
    ) -> Vec<Result<Vec<MettaResult>, PeTTaError>> {
        use rayon::prelude::*;
        use std::sync::Arc;

        let config = Arc::new(self.config.clone());

        file_paths.par_iter().map(|path| {
            let cfg = Arc::clone(&config);
            let mut c = (*cfg).clone();
            c.verbose = false;
            let path = path.clone();
            let mut engine = PeTTaEngine::with_config(&c)?;
            engine.load_metta_file(&path)
        }).collect()
    }
}

impl Drop for PeTTaEngine {
    fn drop(&mut self) {
        self.shutdown();
    }
}

// ---------------------------------------------------------------------------
// Output parsing (legacy, kept for test compatibility)
// ---------------------------------------------------------------------------

#[allow(dead_code)]
fn parse_output(output: &str, _verbose: bool) -> Vec<MettaResult> {
    output
        .lines()
        .filter_map(|line| {
            let cleaned = strip_ansi(line.trim());
            if cleaned.is_empty()
                || cleaned.starts_with('%')
                || cleaned.starts_with("?-")
                || cleaned.starts_with(":-")
                || cleaned.starts_with("-->")
                || cleaned.starts_with("^^^")
                || cleaned.starts_with('!')
                || cleaned.contains("metta function")
                || cleaned.contains("metta runnable")
                || cleaned.contains("prolog clause")
                || cleaned.contains("prolog goal")
            {
                return None;
            }
            Some(MettaResult { value: cleaned })
        })
        .collect()
}

#[allow(dead_code)]
fn strip_ansi(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut in_esc = false;
    for ch in s.chars() {
        if ch == '\x1b' {
            in_esc = true;
        } else if in_esc {
            if ch == '[' {
                continue;
            }
            if (0x40..=0x7E).contains(&(ch as u32)) {
                in_esc = false;
            }
        } else {
            result.push(ch);
        }
    }
    result.trim().to_string()
}

// ---------------------------------------------------------------------------
// Version check
// ---------------------------------------------------------------------------

fn check_swipl_version(
    swipl_path: &Path,
    min_version: (u32, u32),
) -> Result<(), PeTTaError> {
    let output = Command::new(swipl_path)
        .arg("--version")
        .output()
        .map_err(|_| {
            PeTTaError::SwiplVersionError(format!(
                "swipl not found at {}. Install SWI-Prolog >= {}.{}.",
                swipl_path.display(),
                min_version.0,
                min_version.1
            ))
        })?;
    if !output.status.success() {
        return Err(PeTTaError::SwiplVersionError(
            "swipl --version failed".into(),
        ));
    }
    let vs = String::from_utf8_lossy(&output.stdout);
    for part in vs.split_whitespace() {
        let p: Vec<&str> = part.split('.').collect();
        if p.len() >= 2 {
            if let (Ok(ma), Ok(mi)) = (p[0].parse::<u32>(), p[1].parse::<u32>()) {
                if ma > min_version.0 || (ma == min_version.0 && mi >= min_version.1) {
                    debug!(
                        "SWI-Prolog version {}.{} meets minimum {}.{}",
                        ma, mi, min_version.0, min_version.1
                    );
                    return Ok(());
                }
                if ma < min_version.0 {
                    return Err(PeTTaError::SwiplVersionError(format!(
                        "SWI-Prolog {}.{} found, need >= {}.{}",
                        ma, mi, min_version.0, min_version.1
                    )));
                }
            }
        }
    }
    Ok(())
}

/// Check if SWI-Prolog is available with default settings.
pub fn swipl_available() -> bool {
    check_swipl_version(Path::new("swipl"), (9, 3)).is_ok()
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn project_root() -> PathBuf {
        Path::new(env!("CARGO_MANIFEST_DIR")).to_path_buf()
    }
    fn make_engine() -> PeTTaEngine {
        PeTTaEngine::new(&project_root(), false).expect("Failed to create engine")
    }

    #[test]
    fn test_engine_creation() {
        let r = PeTTaEngine::new(&project_root(), false);
        if let Err(ref e) = r {
            eprintln!("Engine creation failed: {}", e);
        }
        assert!(r.is_ok());
    }
    #[test]
    fn test_booleansolver() {
        let mut e = make_engine();
        let r = e.load_metta_file(&project_root().join("examples/booleansolver.metta"));
        let stderr = e.stderr_output();
        if !stderr.is_empty() {
            eprintln!("STDERR: {}", stderr);
        }
        match &r {
            Ok(results) => {
                for res in results {
                    eprintln!("RESULT: {}", res.value);
                }
                assert!(!results.is_empty());
            }
            Err(e) => {
                eprintln!("ERROR: {:?}", e);
                panic!("booleansolver failed");
            }
        }
    }
    #[test]
    fn test_identity() {
        let mut e = make_engine();
        let r = e
            .process_metta_string("(= (myid $x) $x) !(myid 42)")
            .unwrap();
        assert!(!r.is_empty());
        assert_eq!(r[0].value, "42");
    }
    #[test]
    fn test_arithmetic() {
        let mut e = make_engine();
        let r = e.process_metta_string("!(+ 1 2)").unwrap();
        assert!(!r.is_empty());
        assert_eq!(r[0].value, "3");
    }
    #[test]
    fn test_load_identity_file() {
        let mut e = make_engine();
        let r = e
            .load_metta_file(&project_root().join("examples/identity.metta"))
            .unwrap();
        assert!(!r.is_empty());
    }
    #[test]
    fn test_boolean() {
        let mut e = make_engine();
        let r = e.process_metta_string("!(and true false)").unwrap();
        assert!(!r.is_empty());
        assert_eq!(r[0].value, "false");
    }
    #[test]
    fn test_comparison() {
        let mut e = make_engine();
        let r = e.process_metta_string("!(< 1 2)").unwrap();
        assert!(!r.is_empty());
        assert_eq!(r[0].value, "true");
    }
    #[test]
    fn test_fibonacci() {
        let mut e = make_engine();
        let r = e
            .load_metta_file(&project_root().join("examples/fib.metta"))
            .unwrap();
        assert!(!r.is_empty());
    }
    #[test]
    fn test_state() {
        let mut e = make_engine();
        let r = e
            .load_metta_file(&project_root().join("examples/state.metta"))
            .unwrap();
        assert!(!r.is_empty());
    }
    #[test]
    fn test_if() {
        let mut e = make_engine();
        let r = e
            .load_metta_file(&project_root().join("examples/if.metta"))
            .unwrap();
        assert!(!r.is_empty());
    }
    #[test]
    fn test_math() {
        let mut e = make_engine();
        let r = e
            .load_metta_file(&project_root().join("examples/math.metta"))
            .unwrap();
        assert!(!r.is_empty());
    }
    #[test]
    fn test_variable_renaming() {
        let mut e = make_engine();
        let r = e
            .process_metta_string("(= (fun ($a x)) ($b x)) !(fun (a x))")
            .unwrap();
        assert!(!r.is_empty());
        let v = &r[0].value;
        assert!(
            v.contains("$") && v.contains('x'),
            "Expected variable pattern, got: {}",
            v
        );
    }
    #[test]
    fn test_file_imports() {
        let mut e = make_engine();
        let root = project_root();
        let id = std::fs::read_to_string(root.join("examples/identity.metta")).unwrap();
        let r = e.process_metta_string(&format!("{}\n!(f 5)", id)).unwrap();
        assert!(r.iter().any(|x| x.value == "25"), "Expected '25': {:?}", r);
    }
    #[test]
    fn test_verbose_mode() {
        let root = project_root();
        let mut e = PeTTaEngine::new(&root, true).unwrap();
        let r = e.process_metta_string("!(+ 1 2)").unwrap();
        assert!(!r.is_empty());
        assert_eq!(r[0].value, "3");
    }
    #[test]
    fn test_parse_output() {
        let r = parse_output("42\n(a b c)\nhello", false);
        assert_eq!(r.len(), 3);
        assert_eq!(r[0].value, "42");
    }
    #[test]
    fn test_parse_output_filters_debug() {
        let r = parse_output(
            "--> metta function -->\n42\n^^^^^^^^^^^^^^^^^^^\n\x1b[36m!(+ 1 2)\n\x1b[33m-->",
            false,
        );
        assert_eq!(r.len(), 1);
        assert_eq!(r[0].value, "42");
    }
    #[test]
    fn test_parse_int() {
        assert_eq!(
            MettaValue::parse("42"),
            Some(MettaValue::Integer("42".into()))
        );
    }
    #[test]
    fn test_parse_bool() {
        assert_eq!(MettaValue::parse("true"), Some(MettaValue::Bool(true)));
        assert_eq!(MettaValue::parse("false"), Some(MettaValue::Bool(false)));
    }
    #[test]
    fn test_parse_float() {
        assert_eq!(MettaValue::parse("3.14"), Some(MettaValue::Float(3.14)));
    }
    #[test]
    fn test_parse_atom() {
        assert_eq!(
            MettaValue::parse("hello"),
            Some(MettaValue::Atom("hello".into()))
        );
    }
    #[test]
    fn test_parse_list() {
        let v = MettaValue::parse("(1 2 3)");
        assert!(matches!(v, Some(MettaValue::List(ref items)) if items.len() == 3));
    }
    #[test]
    fn test_parse_expression() {
        let v = MettaValue::parse("(+ 1 2)");
        assert!(
            matches!(v, Some(MettaValue::Expression(ref f, ref args)) if f == "+" && args.len() == 2)
        );
    }
    #[test]
    fn test_parse_undefined_function_error() {
        assert!(matches!(
            parse_swipl_error("ERROR: existence_error(procedure, (/ foo 2))"),
            SwiplErrorKind::UndefinedFunction {
                name: _,
                arity: 2,
                ..
            }
        ));
    }
    #[test]
    fn test_parse_stack_overflow() {
        assert!(matches!(
            parse_swipl_error("ERROR: Stack depth: 5000000"),
            SwiplErrorKind::StackOverflow { .. }
        ));
    }
    #[test]
    fn test_result_parsed_value() {
        let r = MettaResult { value: "42".into() };
        assert_eq!(r.parsed_value(), Some(MettaValue::Integer("42".into())));
    }

    #[test]
    fn test_engine_config_builder() {
        let config = EngineConfig::new(&project_root())
            .verbose(true)
            .max_restarts(3);
        assert!(config.verbose);
        assert_eq!(config.max_restarts, 3);
    }

    #[test]
    fn test_engine_with_config() {
        let config = EngineConfig::new(&project_root())
            .verbose(false)
            .max_restarts(1);
        let engine = PeTTaEngine::with_config(&config);
        assert!(engine.is_ok());
    }

    #[test]
    fn test_is_alive() {
        let mut e = make_engine();
        assert!(e.is_alive(), "Engine should be alive after creation");
        let r = e.process_metta_string("!(+ 1 2)").unwrap();
        assert!(e.is_alive(), "Engine should be alive after query");
        assert_eq!(r[0].value, "3");
    }

    #[test]
    fn test_error_display() {
        let e = PeTTaError::FileNotFound(PathBuf::from("/nonexistent"));
        assert!(e.to_string().contains("/nonexistent"));

        let e = PeTTaError::ProtocolError("test error".into());
        assert!(e.to_string().contains("test error"));

        let e = SwiplErrorKind::UndefinedFunction {
            name: "foo".into(),
            arity: 2,
            suggestion: Some("bar".into()),
        };
        let s = e.to_string();
        assert!(s.contains("foo/2"));
        assert!(s.contains("bar"));
    }

    #[test]
    fn test_config_defaults() {
        let config = EngineConfig::default();
        assert_eq!(config.swipl_path, PathBuf::from("swipl"));
        assert!(!config.verbose);
        assert!(config.query_timeout.is_none());
        assert_eq!(config.max_restarts, 0);
        assert_eq!(config.min_swipl_version, (9, 3));
    }

    #[test]
    fn test_swipl_available() {
        assert!(swipl_available());
    }
}
