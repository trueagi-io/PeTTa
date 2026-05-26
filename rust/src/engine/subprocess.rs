//! Subprocess management for SWI-Prolog backend

use std::io::{BufReader, Read};
use std::process::{Command, Stdio};
use std::sync::{Arc, Mutex};

use super::config::EngineConfig;
use super::errors::Error;
use super::server::build_server_source;
use super::version::check_swipl_version;

pub type SpawnHandle = (
    std::process::Child,
    std::process::ChildStdin,
    BufReader<std::process::ChildStdout>,
    Arc<Mutex<Vec<u8>>>,
);

pub struct SubprocessManager {
    config: EngineConfig,
    stderr: Arc<Mutex<Vec<u8>>>,
}

impl SubprocessManager {
    pub fn new(config: EngineConfig) -> Self {
        Self { config, stderr: Arc::new(Mutex::new(Vec::new())) }
    }
    
    pub fn spawn(&self) -> Result<SpawnHandle, Error> {
        check_swipl_version(&self.config.swipl_path, (9, 3))?;
        
        let src_dir = &self.config.src_dir;
        
        let server = build_server_source(src_dir, self.config.verbose)?;
        let tmp = tempfile::Builder::new()
            .prefix("petta_srv_").suffix(".pl").tempfile()
            .map_err(|e| Error::WriteError(e.to_string()))?;
        
        std::fs::write(tmp.path(), &server)
            .map_err(|e| Error::WriteError(e.to_string()))?;
        
        let mut cmd = Command::new(&self.config.swipl_path);
        cmd.args(["-q", "-t", "halt", tmp.path().to_str().unwrap()]);

        #[cfg(feature = "websocket")]
        if let Some(port) = self.config.ws_port {
            cmd.env("WS_PORT", port.to_string());
        }

        let mut child = cmd
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .map_err(|e| Error::SpawnError(e.to_string()))?;
        
        let stderr = child.stderr.take();
        let stderr_out = self.stderr.clone();
        std::thread::spawn(move || {
            if let Some(mut s) = stderr {
                let mut buf = [0u8; 4096];
                while let Ok(n) = s.read(&mut buf) {
                    if n == 0 { break; }
                    let _ = stderr_out.lock().map(|mut g| g.extend_from_slice(&buf[..n]));
                }
            }
        });
        
        let stdin = child.stdin.take().ok_or_else(|| Error::SpawnError("no stdin".into()))?;
        let stdout = BufReader::new(
            child.stdout.take().ok_or_else(|| Error::SpawnError("no stdout".into()))?
        );
        
        std::mem::forget(tmp);
        Ok((child, stdin, stdout, self.stderr.clone()))
    }
}

#[allow(dead_code)]
pub fn wait_for_ready<R: Read>(reader: &mut R) -> Result<(), Error> {
    loop {
        let mut b = [0u8; 1];
        reader.read_exact(&mut b).map_err(|e| {
            Error::Protocol(format!("read ready signal: {e}"))
        })?;
        if b[0] == 0xFF { return Ok(()); }
    }
}
