//! PeTTa engine: persistent SWI-Prolog subprocess management.
//!
//! The engine communicates with SWI-Prolog over a binary length-prefixed
//! protocol via stdin/stdout pipes. A single Prolog process lives for the
//! entire session, eliminating startup overhead.

mod config;
pub(crate) mod errors;
mod server;
mod values;
mod protocol;
mod version;

pub use config::EngineConfig;
pub use errors::{SwiplErrorKind, PeTTaError};
pub use values::{MettaValue, MettaResult};
pub use version::{swipl_available, MIN_SWIPL_VERSION};

use std::io::{BufReader, Read, Write};
use std::path::Path;
#[cfg(feature = "parallel")]
use std::path::PathBuf;
use std::process::{Child, Command, Stdio};
#[cfg(feature = "profiling")]
use std::time::Instant;

use tracing::{debug, info, trace};

use self::protocol::{
    load_metta_file as proto_load_metta_file,
    load_metta_files as proto_load_metta_files,
    process_metta_string as proto_process_metta_string,
};
use self::version::check_swipl_version;
use self::server::build_server_source;
#[cfg(feature = "profiling")]
use crate::profiler;

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

    /// Check if the Prolog subprocess is alive and responsive.
    pub fn is_alive(&mut self) -> bool {
        self.stdin_pipe.is_some() && self.stdout_pipe.is_some()
    }

    pub fn load_metta_file(&mut self, file_path: &Path) -> Result<Vec<MettaResult>, PeTTaError> {
        proto_load_metta_file(&mut self.stdin_pipe, &mut self.stdout_pipe, file_path, &self.config)
    }

    pub fn load_metta_files(
        &mut self,
        file_paths: &[&Path],
    ) -> Result<Vec<MettaResult>, PeTTaError> {
        proto_load_metta_files(&mut self.stdin_pipe, &mut self.stdout_pipe, file_paths, &self.config)
    }

    pub fn process_metta_string(
        &mut self,
        metta_code: &str,
    ) -> Result<Vec<MettaResult>, PeTTaError> {
        proto_process_metta_string(&mut self.stdin_pipe, &mut self.stdout_pipe, metta_code, &self.config)
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
    #[cfg(feature = "profiling")]
    pub fn process_metta_string_profiled(
        &mut self,
        metta_code: &str,
    ) -> Result<(Vec<MettaResult>, profiler::QueryProfile), PeTTaError> {
        let mut profile = profiler::QueryProfile::new("process_metta_string", metta_code.len());
        let total_start = Instant::now();

        let send_start = Instant::now();
        let results = proto_process_metta_string(&mut self.stdin_pipe, &mut self.stdout_pipe, metta_code, &self.config);
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
    #[cfg(feature = "profiling")]
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
        let results = proto_load_metta_file(&mut self.stdin_pipe, &mut self.stdout_pipe, &abs, &self.config);
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
            proto_process_metta_string(&mut engine.stdin_pipe, &mut engine.stdout_pipe, q, &engine.config)
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
            proto_load_metta_file(&mut engine.stdin_pipe, &mut engine.stdout_pipe, &path, &engine.config)
        }).collect()
    }
}

impl Drop for PeTTaEngine {
    fn drop(&mut self) {
        self.shutdown();
    }
}
