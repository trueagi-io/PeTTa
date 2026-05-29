//! Backend implementations for SWI-Prolog and MORK
//!
//! This module provides concrete backend implementations that wrap the
//! low-level execution engines with the unified BackendImpl trait.

use std::io::BufReader;
use std::path::Path;
use std::sync::{Arc, Mutex};

use super::backend::BackendImpl;
use super::client;
use super::config::EngineConfig;
use super::errors::Error;
use super::subprocess::SubprocessManager;
use crate::core::BackendCapabilities;
use crate::values::MettaResult;

#[cfg(feature = "mork")]
use crate::mork::interpreter::Interpreter;

// ============================================================================
// SWI-Prolog Backend
// ============================================================================

/// SWI-Prolog subprocess backend
pub struct SwiplBackend {
    child: Option<std::process::Child>,
    stdin: Option<std::process::ChildStdin>,
    stdout: Option<BufReader<std::process::ChildStdout>>,
    stderr: Arc<Mutex<Vec<u8>>>,
    config: EngineConfig,
}

impl SwiplBackend {
    pub fn new(config: &EngineConfig) -> Result<Self, Error> {
        let manager = SubprocessManager::new(config.clone());
        let (child, stdin, stdout, stderr) = manager.spawn()?;

        Ok(Self {
            child: Some(child),
            stdin: Some(stdin),
            stdout: Some(stdout),
            stderr,
            config: config.clone(),
        })
    }

    fn with_child<T, F>(&mut self, f: F) -> Result<T, Error>
    where
        F: FnOnce(&mut std::process::Child) -> Result<T, Error>,
    {
        if let Some(child) = self.child.as_mut() {
            f(child)
        } else {
            Err(Error::Protocol("backend not initialized".into()))
        }
    }

    fn with_streams<T, F>(&mut self, f: F) -> Result<T, Error>
    where
        F: FnOnce(
            &mut std::process::ChildStdin,
            &mut BufReader<std::process::ChildStdout>,
        ) -> Result<T, Error>,
    {
        let stdin = self.stdin.take();
        let stdout = self.stdout.take();

        if let (Some(mut stdin), Some(mut stdout)) = (stdin, stdout) {
            let result = f(&mut stdin, &mut stdout);
            self.stdin = Some(stdin);
            self.stdout = Some(stdout);
            result
        } else {
            Err(Error::Protocol("streams not available".into()))
        }
    }
}

impl super::backend::BackendImpl for SwiplBackend {
    fn version(&self) -> &'static str {
        "9.0"
    }

    fn capabilities(&self) -> BackendCapabilities {
        BackendCapabilities::new().with_streaming(true).with_transactions(false)
    }

    fn name(&self) -> &'static str {
        "SWI-Prolog"
    }

    fn is_alive(&mut self) -> bool {
        self.with_child(|c| Ok(c.try_wait().map(|s| s.is_none()).unwrap_or(false))).unwrap_or(false)
    }

    fn load_metta_file(
        &mut self,
        path: &Path,
        config: &EngineConfig,
    ) -> Result<Vec<MettaResult>, Error> {
        self.with_streams(|stdin, stdout| client::load_metta_file(stdin, stdout, path, config))
    }

    fn load_metta_files(
        &mut self,
        paths: &[&Path],
        config: &EngineConfig,
    ) -> Result<Vec<MettaResult>, Error> {
        self.with_streams(|stdin, stdout| client::load_metta_files(stdin, stdout, paths, config))
    }

    fn process_metta_string(
        &mut self,
        code: &str,
        config: &EngineConfig,
    ) -> Result<Vec<MettaResult>, Error> {
        self.with_streams(|stdin, stdout| client::process_metta_string(stdin, stdout, code, config))
    }

    fn stderr_output(&self) -> String {
        self.stderr
            .lock()
            .map(|d| String::from_utf8_lossy(&d).into_owned())
            .unwrap_or_else(|_| String::new())
    }

    fn shutdown(&mut self) {
        if let Some(mut child) = self.child.take() {
            let _ = child.kill();
            let _ = child.wait();
        }
    }

    fn restart(&mut self, config: &EngineConfig) -> Result<(), Error> {
        self.shutdown();
        let manager = SubprocessManager::new(config.clone());
        let (child, stdin, stdout, _stderr) = manager.spawn()?;

        self.child = Some(child);
        self.stdin = Some(stdin);
        self.stdout = Some(stdout);
        self.config = config.clone();
        // stats removed

        Ok(())
    }
}

impl Drop for SwiplBackend {
    fn drop(&mut self) {
        self.shutdown();
    }
}

// ============================================================================
// MORK Backend
// ============================================================================

#[cfg(feature = "mork")]
pub struct MorkBackend {
    interpreter: Interpreter,
}

#[cfg(feature = "mork")]
impl MorkBackend {
    pub fn new() -> Self {
        use crate::mork::space::Space;
        use std::sync::Arc;

        let space = Arc::new(Mutex::new(Space::new()));
        let interpreter = Interpreter::new(space);

        Self { interpreter }
    }
}

#[cfg(feature = "mork")]
impl super::backend::BackendImpl for MorkBackend {
    fn version(&self) -> &'static str {
        "9.0"
    }

    fn capabilities(&self) -> BackendCapabilities {
        BackendCapabilities::new().with_streaming(true).with_transactions(false)
    }

    fn name(&self) -> &'static str {
        "MORK"
    }

    fn is_alive(&mut self) -> bool {
        true // MORK is always alive (in-process)
    }

    fn load_metta_file(
        &mut self,
        path: &Path,
        _config: &EngineConfig,
    ) -> Result<Vec<MettaResult>, Error> {
        let content = std::fs::read_to_string(path).map_err(|e| Error::PathError(e.to_string()))?;
        Ok(self.process_metta_string(&content, _config)?)
    }

    fn load_metta_files(
        &mut self,
        paths: &[&Path],
        config: &EngineConfig,
    ) -> Result<Vec<MettaResult>, Error> {
        let mut all_results = Vec::new();
        for path in paths {
            all_results.extend(self.load_metta_file(path, config)?);
        }
        Ok(all_results)
    }

    fn process_metta_string(
        &mut self,
        code: &str,
        _config: &EngineConfig,
    ) -> Result<Vec<MettaResult>, Error> {
        let results =
            self.interpreter.process(code).into_iter().map(|v| MettaResult { value: v }).collect();
        Ok(results)
    }

    fn stderr_output(&self) -> String {
        String::new() // MORK doesn't have stderr
    }

    fn shutdown(&mut self) {
        // No-op for in-process backend
    }

    fn restart(&mut self, _config: &EngineConfig) -> Result<(), Error> {
        // stats removed
        Ok(())
    }
}

#[cfg(feature = "mork")]
impl Default for MorkBackend {
    fn default() -> Self {
        Self::new()
    }
}
