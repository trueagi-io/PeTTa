//! Protocol client for PeTTa ↔ Prolog communication
//!
//! Encapsulates the binary length-prefixed protocol used to communicate
//! with the SWI-Prolog subprocess over stdin/stdout pipes.

use std::io::{BufReader, Read, Write};
use std::time::Instant;

use tracing::{debug, trace, warn};

use super::config::EngineConfig;
use super::errors::{parse_backend_error, PeTTaError};
use crate::engine::values::MettaResult;

/// Manages communication pipes for protocol operations
struct PipeManager<'a> {
    stdin_pipe: &'a mut Option<std::process::ChildStdin>,
    stdout_pipe: &'a mut Option<BufReader<std::process::ChildStdout>>,
}

impl<'a> PipeManager<'a> {
    fn new(
        stdin_pipe: &'a mut Option<std::process::ChildStdin>,
        stdout_pipe: &'a mut Option<BufReader<std::process::ChildStdout>>,
    ) -> Self {
        Self {
            stdin_pipe,
            stdout_pipe,
        }
    }

    fn stdin(&mut self) -> Result<&mut std::process::ChildStdin, PeTTaError> {
        self.stdin_pipe
            .as_mut()
            .ok_or_else(|| PeTTaError::ProtocolError("stdin pipe unavailable".into()))
    }

    fn stdout(&mut self) -> Result<&mut BufReader<std::process::ChildStdout>, PeTTaError> {
        self.stdout_pipe
            .as_mut()
            .ok_or_else(|| PeTTaError::ProtocolError("stdout pipe unavailable".into()))
    }
}

/// Extension trait for write operations with error handling
trait WriteExt {
    fn write_checked(&mut self, data: &[u8]) -> Result<(), PeTTaError>;
}

impl<W: Write> WriteExt for W {
    fn write_checked(&mut self, data: &[u8]) -> Result<(), PeTTaError> {
        self.write_all(data)
            .map_err(|e| PeTTaError::WriteError(e.to_string()))
    }
}

/// Protocol client for sending queries to Prolog backend
pub struct ProtocolClient;

impl ProtocolClient {
    /// Send a query and receive results
    pub fn send_query(
        stdin_pipe: &mut Option<std::process::ChildStdin>,
        stdout_pipe: &mut Option<BufReader<std::process::ChildStdout>>,
        query_type: u8,
        payload: &str,
        config: &EngineConfig,
    ) -> Result<Vec<MettaResult>, PeTTaError> {
        trace!(
            "Sending query: type={}, payload_len={}",
            query_type,
            payload.len()
        );

        match send_query_inner(stdin_pipe, stdout_pipe, query_type, payload, config) {
            Ok(results) => Ok(results),
            Err(PeTTaError::ProtocolError(ref msg)) if msg.contains("child closed") => {
                warn!(
                    "Subprocess crashed during query (type={}), payload preview: {:?}",
                    query_type as char,
                    &payload[..payload.len().min(120)]
                );
                Err(PeTTaError::ProtocolError(format!(
                    "child closed (query type '{}', payload: {:?})",
                    query_type as char,
                    &payload[..payload.len().min(120)]
                )))
            }
            Err(e) => Err(e),
        }
    }
}

fn send_query_inner(
    stdin_pipe: &mut Option<std::process::ChildStdin>,
    stdout_pipe: &mut Option<BufReader<std::process::ChildStdout>>,
    query_type: u8,
    payload: &str,
    config: &EngineConfig,
) -> Result<Vec<MettaResult>, PeTTaError> {
    let start_time = Instant::now();
    let mut pipes = PipeManager::new(stdin_pipe, stdout_pipe);

    // Send request
    let pb = payload.as_bytes();
    let len = pb.len() as u32;
    let sin = pipes.stdin()?;
    sin.write_checked(&[query_type])?;
    sin.write_checked(&len.to_be_bytes())?;
    sin.write_checked(pb)?;
    sin.flush()
        .map_err(|e| PeTTaError::WriteError(e.to_string()))?;

    check_timeout(start_time, config)?;

    // Read response status
    let status = {
        let mut b = [0u8; 1];
        let reader = pipes.stdout()?;
        read_exact_with_timeout(reader, &mut b, start_time, config)?;
        b[0]
    };

    match status {
        0 => {
            // Success: read results
            let count = read_u32_with_timeout(pipes.stdout()?, start_time, config)?;
            trace!("Query succeeded: {} result(s)", count);
            let mut results = Vec::with_capacity(count as usize);
            for _ in 0..count {
                let len = read_u32_with_timeout(pipes.stdout()?, start_time, config)?;
                let mut buf = vec![0u8; len as usize];
                let reader = pipes.stdout()?;
                read_exact_with_timeout(reader, &mut buf, start_time, config)?;
                let value =
                    String::from_utf8(buf).map_err(|e| PeTTaError::ProtocolError(e.to_string()))?;
                results.push(MettaResult { value });
            }
            Ok(results)
        }
        1 => {
            // Error: read error message
            let len = read_u32_with_timeout(pipes.stdout()?, start_time, config)?;
            let mut buf = vec![0u8; len as usize];
            let reader = pipes.stdout()?;
            read_exact_with_timeout(reader, &mut buf, start_time, config)?;
            let msg = String::from_utf8_lossy(&buf).to_string();
            debug!("Prolog error response: {}", msg);
            Err(PeTTaError::BackendError(parse_backend_error(&msg)))
        }
        _ => Err(PeTTaError::ProtocolError(format!("unknown status: {}", status))),
    }
}

/// Check if the query has exceeded its configured timeout
pub fn check_timeout(start_time: Instant, config: &EngineConfig) -> Result<(), PeTTaError> {
    if let Some(timeout) = config.query_timeout {
        if start_time.elapsed() >= timeout {
            return Err(PeTTaError::Timeout(timeout));
        }
    }
    Ok(())
}

/// Read exactly `buf.len()` bytes, checking timeout before each read attempt
pub fn read_exact_with_timeout<R: Read>(
    reader: &mut R,
    buf: &mut [u8],
    start_time: Instant,
    config: &EngineConfig,
) -> Result<(), PeTTaError> {
    let mut read_count = 0;
    while read_count < buf.len() {
        check_timeout(start_time, config)?;
        let n = reader
            .read(&mut buf[read_count..])
            .map_err(|e| {
                if e.kind() == std::io::ErrorKind::UnexpectedEof {
                    PeTTaError::ProtocolError("child closed".into())
                } else {
                    PeTTaError::ProtocolError(e.to_string())
                }
            })?;
        if n == 0 {
            return Err(PeTTaError::ProtocolError("child closed".into()));
        }
        read_count += n;
    }
    Ok(())
}

/// Read a big-endian u32 with timeout checking
pub fn read_u32_with_timeout<R: Read>(
    reader: &mut R,
    start_time: Instant,
    config: &EngineConfig,
) -> Result<u32, PeTTaError> {
    let mut b = [0u8; 4];
    read_exact_with_timeout(reader, &mut b, start_time, config)?;
    Ok(u32::from_be_bytes(b))
}

/// Load a single MeTTa file
pub fn load_metta_file(
    stdin_pipe: &mut Option<std::process::ChildStdin>,
    stdout_pipe: &mut Option<BufReader<std::process::ChildStdout>>,
    file_path: &std::path::Path,
    config: &EngineConfig,
) -> Result<Vec<MettaResult>, PeTTaError> {
    let abs = file_path
        .canonicalize()
        .map_err(|e| PeTTaError::PathError(e.to_string()))?;
    if !abs.exists() {
        return Err(PeTTaError::FileNotFound(abs));
    }
    debug!("Loading MeTTa file: {}", abs.display());
    ProtocolClient::send_query(stdin_pipe, stdout_pipe, b'F', &abs.to_string_lossy(), config)
}

/// Load multiple MeTTa files
pub fn load_metta_files(
    stdin_pipe: &mut Option<std::process::ChildStdin>,
    stdout_pipe: &mut Option<BufReader<std::process::ChildStdout>>,
    file_paths: &[&std::path::Path],
    config: &EngineConfig,
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
    ProtocolClient::send_query(stdin_pipe, stdout_pipe, b'S', &combined, config)
}

/// Process a MeTTa string
pub fn process_metta_string(
    stdin_pipe: &mut Option<std::process::ChildStdin>,
    stdout_pipe: &mut Option<BufReader<std::process::ChildStdout>>,
    metta_code: &str,
    config: &EngineConfig,
) -> Result<Vec<MettaResult>, PeTTaError> {
    debug!("Processing MeTTa string ({} bytes)", metta_code.len());
    ProtocolClient::send_query(stdin_pipe, stdout_pipe, b'S', metta_code, config)
}
