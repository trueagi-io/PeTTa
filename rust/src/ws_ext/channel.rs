use serde_json::Value;
use std::io::{self, BufRead, Write};

pub fn send(params: &Value) -> Result<String, String> {
    let msg = params["msg"].as_str().unwrap_or("");
    let stdout = io::stdout();
    let mut handle = stdout.lock();
    writeln!(handle, "{}", msg).map_err(|e| format!("channel send: {}", e))?;
    handle.flush().map_err(|e| format!("channel flush: {}", e))?;
    Ok("ok".into())
}

pub fn recv() -> Result<String, String> {
    let stdin = io::stdin();
    let mut line = String::new();
    stdin.lock().read_line(&mut line).map_err(|e| format!("channel recv: {}", e))?;
    Ok(line.trim().to_string())
}
