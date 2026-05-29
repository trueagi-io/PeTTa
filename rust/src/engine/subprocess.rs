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
        eprintln!("[DBG] SubprocessManager::spawn called, swipl_path={:?}", self.config.swipl_path);
        check_swipl_version(&self.config.swipl_path, (9, 0))?;
        eprintln!("[DBG] swipl version check OK");

        let src_dir = &self.config.src_dir;
        eprintln!("[DBG] src_dir={:?}", src_dir);

        let server = build_server_source(src_dir, self.config.verbose)?;
        eprintln!("[DBG] server source built, length={}", server.len());
        let tmp = tempfile::Builder::new()
            .prefix("petta_srv_")
            .suffix(".pl")
            .tempfile()
            .map_err(|e| Error::WriteError(e.to_string()))?;
        eprintln!("[DBG] temp file created: {:?}", tmp.path());

        std::fs::write(tmp.path(), &server).map_err(|e| Error::WriteError(e.to_string()))?;
        eprintln!("[DBG] temp file written");

        let mut cmd = Command::new(&self.config.swipl_path);
        cmd.args(["-q", "-t", "halt", tmp.path().to_str().unwrap()]);
        eprintln!("[DBG] command: {:?}", cmd);
        for arg in &self.config.extra_args {
            cmd.arg(arg);
        }

        #[cfg(feature = "websocket")]
        if let Some(port) = self.config.ws_port {
            eprintln!("[DBG] setting WS_PORT={}", port);
            cmd.env("WS_PORT", port.to_string());
        }

        eprintln!("[DBG] spawning child process...");
        let mut child = cmd
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .map_err(|e| Error::SpawnError(e.to_string()))?;
        eprintln!("[DBG] child spawned, pid={}", child.id());

        let stderr = child.stderr.take();
        let stderr_out = self.stderr.clone();
        std::thread::spawn(move || {
            eprintln!("[DBG] stderr reader thread started");
            if let Some(mut s) = stderr {
                let mut buf = [0u8; 4096];
                while let Ok(n) = s.read(&mut buf) {
                    if n == 0 {
                        break;
                    }
                    let _ = stderr_out.lock().map(|mut g| g.extend_from_slice(&buf[..n]));
                }
            }
            eprintln!("[DBG] stderr reader thread exiting");
        });

        eprintln!("[DBG] taking stdin/stdout from child");
        let stdin = child.stdin.take().ok_or_else(|| Error::SpawnError("no stdin".into()))?;
        let stdout = BufReader::new(
            child.stdout.take().ok_or_else(|| Error::SpawnError("no stdout".into()))?,
        );

        std::mem::forget(tmp);
        eprintln!("[DBG] spawn returning Ok");
        Ok((child, stdin, stdout, self.stderr.clone()))
    }
}

#[allow(dead_code)]
pub fn wait_for_ready<R: Read>(reader: &mut R) -> Result<(), Error> {
    loop {
        let mut b = [0u8; 1];
        reader
            .read_exact(&mut b)
            .map_err(|e| Error::Protocol(format!("read ready signal: {e}")))?;
        if b[0] == 0xFF {
            return Ok(());
        }
    }
}
