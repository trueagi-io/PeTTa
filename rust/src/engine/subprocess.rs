//! Subprocess lifecycle management for SWI-Prolog backend
//!
//! Handles spawning, restarting, and monitoring the Prolog subprocess.

use std::io::{BufReader, Read};
use std::process::{Child, Command, Stdio};
use std::sync::{Arc, Mutex};


use tracing::{debug, info, trace};

use super::config::EngineConfig;
use super::errors::PeTTaError;
use super::server::build_server_source;
use super::version::check_swipl_version;

/// Type alias for the spawned subprocess handle
pub type SpawnHandle = (
    std::process::Child,
    std::process::ChildStdin,
    BufReader<std::process::ChildStdout>,
    Arc<Mutex<Vec<u8>>>,
);

/// Manages the SWI-Prolog subprocess lifecycle
pub struct SubprocessManager {
    config: EngineConfig,
    stderr_output: Arc<Mutex<Vec<u8>>>,
}

impl SubprocessManager {
    /// Create a new subprocess manager
    pub fn new(config: EngineConfig) -> Self {
        Self {
            config,
            stderr_output: Arc::new(Mutex::new(Vec::new())),
        }
    }

    /// Spawn the SWI-Prolog child process
    pub fn spawn(&self) -> Result<SpawnHandle, PeTTaError> {
        debug!("Launching SWI-Prolog subprocess: {:?}", self.config.swipl_path);

        check_swipl_version(&self.config.swipl_path, self.config.min_swipl_version)?;

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

        // Wire stderr to background thread
        let stderr = child.stderr.take();
        let stderr_output = self.stderr_output.clone();
        std::thread::spawn(move || {
            if let Some(mut s) = stderr {
                let mut buf = [0u8; 4096];
                while let Ok(n) = s.read(&mut buf) {
                    if n == 0 {
                        break;
                    }
                    trace!("Prolog stderr: {}", String::from_utf8_lossy(&buf[..n]));
                    if let Ok(mut guard) = stderr_output.lock() {
                        guard.extend_from_slice(&buf[..n]);
                    } else {
                        // If the mutex is poisoned, drop the data rather than panic
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
        Ok((child, stdin, stdout, self.stderr_output.clone()))
    }

    /// Get the stderr output buffer
    pub fn stderr_output(&self) -> String {
        match self.stderr_output.lock() {
            Ok(data) => String::from_utf8_lossy(&data).to_string(),
            Err(e) => format!("<stderr buffer poisoned: {}>", e),
        }
    }

    /// Get the configuration
    pub fn config(&self) -> &EngineConfig {
        &self.config
    }
}

/// Waits for the Prolog ready signal (0xFF)
pub fn wait_for_ready<R: Read>(reader: &mut R) -> Result<(), PeTTaError> {
    trace!("Waiting for Prolog ready signal (0xFF)");
    loop {
        let mut b = [0u8; 1];
        reader
            .read_exact(&mut b)
            .map_err(|e| PeTTaError::ProtocolError(format!("failed to read ready signal: {}", e)))?;
        if b[0] == 0xFF {
            debug!("Prolog ready signal received");
            return Ok(());
        }
        trace!("Discarding startup byte: {:?}", b[0]);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_subprocess_manager_creation() {
        let config = EngineConfig::default();
        let manager = SubprocessManager::new(config.clone());
        assert_eq!(manager.config, config);
    }
}
