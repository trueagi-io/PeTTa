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

use std::io::{BufReader, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};

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
        "metta.pl",
    ] {
        let fpath = src_dir.join(file);
        if !fpath.exists() {
            return Err(PeTTaError::FileNotFound(fpath));
        }
        let fstr = fpath.to_string_lossy().replace('\\', "\\\\");
        src.push_str(&format!(":- consult('{}').\n", fstr));
    }

    // filereader.pl asserts silent(false) at startup; re-assert our setting after all consults
    src.push_str(&format!(
        ":- retractall(silent(_)), assertz(silent({})).\n",
        silent
    ));

    src.push_str(SERVER_LOOP);
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
      read_bytes(Len, Bytes),
      atom_codes(Query, Bytes),
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

read_bytes(0,[]) :- !.
read_bytes(N,[B|Bs]) :- get_byte(user_input,B), N1 is N-1, read_bytes(N1,Bs).

write_bytes([]).
write_bytes([B|Bs]) :- put_byte(user_output,B), write_bytes(Bs).

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

write_result_str(Term) :-
    swrite(Term, Str),
    atom_codes(Str, Codes),
    length(Codes, Len),
    write_u32(Len),
    write_bytes(Codes).
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
// Human-readable errors
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub enum SwiplErrorKind {
    UndefinedFunction {
        name: String,
        arity: usize,
        suggestion: Option<String>,
    },
    TypeMismatch {
        expected: String,
        found: String,
        context: Option<String>,
    },
    SyntaxError {
        line: Option<u32>,
        column: Option<u32>,
        detail: String,
    },
    UninstantiatedArgument {
        location: Option<String>,
    },
    PermissionDenied {
        operation: String,
        target: String,
    },
    ExistenceError {
        error_type: String,
        term: String,
    },
    StackOverflow {
        limit: Option<u32>,
    },
    Generic(String),
}

impl std::fmt::Display for SwiplErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SwiplErrorKind::UndefinedFunction {
                name,
                arity,
                suggestion,
            } => {
                write!(f, "MeTTa function '{}/{}' is not defined", name, arity)?;
                if let Some(s) = suggestion {
                    write!(f, ". Did you mean '{}'?", s)?;
                }
                Ok(())
            }
            SwiplErrorKind::TypeMismatch {
                expected,
                found,
                context,
            } => {
                write!(f, "Type error: expected {}, got {}", expected, found)?;
                if let Some(c) = context {
                    write!(f, " ({})", c)?;
                }
                Ok(())
            }
            SwiplErrorKind::SyntaxError {
                line,
                column,
                detail,
            } => {
                write!(f, "Syntax error")?;
                if let (Some(l), Some(c)) = (line, column) {
                    write!(f, " at {}:{}", l, c)?;
                }
                write!(f, ": {}", detail)
            }
            SwiplErrorKind::UninstantiatedArgument { location } => {
                write!(f, "Argument is not sufficiently instantiated")?;
                if let Some(l) = location {
                    write!(f, " at {}", l)?;
                }
                Ok(())
            }
            SwiplErrorKind::PermissionDenied { operation, target } => {
                write!(f, "Permission denied: {} on {}", operation, target)
            }
            SwiplErrorKind::ExistenceError { error_type, term } => {
                write!(f, "{} {} does not exist", error_type, term)
            }
            SwiplErrorKind::StackOverflow { limit } => {
                write!(f, "Stack overflow")?;
                if let Some(l) = limit {
                    write!(f, " (limit: {})", l)?;
                }
                Ok(())
            }
            SwiplErrorKind::Generic(msg) => write!(f, "{}", msg),
        }
    }
}

fn parse_swipl_error(raw: &str) -> SwiplErrorKind {
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
// Errors
// ---------------------------------------------------------------------------

#[derive(Debug)]
pub enum PeTTaError {
    FileNotFound(PathBuf),
    SpawnSwipl(String),
    PathError(String),
    WriteError(String),
    SwiplError(SwiplErrorKind),
    SwiplVersionError(String),
    ProtocolError(String),
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
            PeTTaError::ProtocolError(e) => write!(f, "Protocol error: {}", e),
        }
    }
}
impl std::error::Error for PeTTaError {}

// ---------------------------------------------------------------------------
// Persistent engine
// ---------------------------------------------------------------------------

pub struct PeTTaEngine {
    child: Option<Child>,
    stdin_pipe: Option<std::process::ChildStdin>,
    stdout_pipe: Option<BufReader<std::process::ChildStdout>>,
}

impl PeTTaEngine {
    pub fn new(project_root: &Path, verbose: bool) -> Result<Self, PeTTaError> {
        let src_dir = project_root.join("src");
        if !src_dir.exists() {
            return Err(PeTTaError::FileNotFound(src_dir));
        }
        check_swipl_version()?;

        let server_source = build_server_source(&src_dir, verbose)?;
        let tmp = tempfile::Builder::new()
            .prefix("petta_srv_")
            .suffix(".pl")
            .tempfile()
            .map_err(|e| PeTTaError::WriteError(e.to_string()))?;
        let tmp_path = tmp.path().to_path_buf();
        std::fs::write(&tmp_path, &server_source)
            .map_err(|e| PeTTaError::WriteError(e.to_string()))?;

        let mut child = Command::new("swipl")
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
        std::thread::spawn(move || {
            if let Some(mut s) = stderr {
                let mut buf = [0u8; 4096];
                while let Ok(n) = s.read(&mut buf) {
                    if n == 0 {
                        break;
                    }
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
        };

        // Wait for the ready signal (0xFF), discarding any startup warnings
        engine.wait_for_ready()?;

        Ok(engine)
    }

    fn wait_for_ready(&mut self) -> Result<(), PeTTaError> {
        loop {
            let mut b = [0u8; 1];
            self.stdout_pipe
                .as_mut()
                .unwrap()
                .read_exact(&mut b)
                .map_err(|e| {
                    PeTTaError::ProtocolError(format!("failed to read ready signal: {}", e))
                })?;
            if b[0] == 0xFF {
                return Ok(());
            }
        }
    }

    fn read_u32(&mut self) -> Result<u32, PeTTaError> {
        let mut b = [0u8; 4];
        self.stdout_pipe
            .as_mut()
            .unwrap()
            .read_exact(&mut b)
            .map_err(|e| {
                if e.kind() == std::io::ErrorKind::UnexpectedEof {
                    PeTTaError::ProtocolError("child closed".into())
                } else {
                    PeTTaError::ProtocolError(e.to_string())
                }
            })?;
        Ok(u32::from_be_bytes(b))
    }

    fn send_query(
        &mut self,
        query_type: u8,
        payload: &str,
    ) -> Result<Vec<MettaResult>, PeTTaError> {
        let pb = payload.as_bytes();
        let len = pb.len() as u32;
        let sin = self.stdin_pipe.as_mut().unwrap();
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
            self.stdout_pipe
                .as_mut()
                .unwrap()
                .read_exact(&mut b)
                .map_err(|e| PeTTaError::ProtocolError(e.to_string()))?;
            b[0]
        };

        match status {
            0 => {
                let count = self.read_u32()?;
                let mut results = Vec::with_capacity(count as usize);
                for _ in 0..count {
                    let len = self.read_u32()?;
                    let mut buf = vec![0u8; len as usize];
                    self.stdout_pipe
                        .as_mut()
                        .unwrap()
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
                self.stdout_pipe
                    .as_mut()
                    .unwrap()
                    .read_exact(&mut buf)
                    .map_err(|e| PeTTaError::ProtocolError(e.to_string()))?;
                let msg = String::from_utf8_lossy(&buf).to_string();
                Err(PeTTaError::SwiplError(parse_swipl_error(&msg)))
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
        self.send_query(b'F', &abs.to_string_lossy())
    }

    pub fn load_metta_files(
        &mut self,
        file_paths: &[&Path],
    ) -> Result<Vec<MettaResult>, PeTTaError> {
        if file_paths.is_empty() {
            return Ok(Vec::new());
        }
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
        self.send_query(b'S', metta_code)
    }

    pub fn shutdown(&mut self) {
        if let Some(sin) = self.stdin_pipe.as_mut() {
            let _ = sin.write_all(&[b'Q', 0, 0, 0, 0]);
            let _ = sin.flush();
        }
        if let Some(mut child) = self.child.take() {
            let _ = child.wait();
        }
    }
}

impl Drop for PeTTaEngine {
    fn drop(&mut self) {
        self.shutdown();
    }
}

// ---------------------------------------------------------------------------
// Output parsing (legacy)
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
    let mut result = String::new();
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

const MIN_MAJOR: u32 = 9;
const MIN_MINOR: u32 = 3;

fn check_swipl_version() -> Result<(), PeTTaError> {
    let output = Command::new("swipl")
        .arg("--version")
        .output()
        .map_err(|_| {
            PeTTaError::SwiplVersionError(format!(
                "swipl not found. Install SWI-Prolog >= {}.{}.",
                MIN_MAJOR, MIN_MINOR
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
                if ma > MIN_MAJOR || (ma == MIN_MAJOR && mi >= MIN_MINOR) {
                    return Ok(());
                }
                if ma < MIN_MAJOR {
                    return Err(PeTTaError::SwiplVersionError(format!(
                        "SWI-Prolog {}.{} found, need >= {}.{}",
                        ma, mi, MIN_MAJOR, MIN_MINOR
                    )));
                }
            }
        }
    }
    Ok(())
}

pub fn swipl_available() -> bool {
    check_swipl_version().is_ok()
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
}
