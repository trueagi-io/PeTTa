use serde_json::Value;
use std::io::{self, BufRead, Write};
use std::sync::{Mutex, OnceLock};

#[cfg(feature = "repl")]
static EDITOR: OnceLock<Mutex<rustyline::DefaultEditor>> = OnceLock::new();

pub fn send(params: &Value) -> Result<String, String> {
    let msg = params["msg"].as_str().unwrap_or("");
    let stdout = io::stdout();
    let mut handle = stdout.lock();
    writeln!(handle, "{}", msg).map_err(|e| format!("channel send: {}", e))?;
    handle.flush().map_err(|e| format!("channel flush: {}", e))?;
    Ok("ok".into())
}

pub fn recv() -> Result<String, String> {
    #[cfg(feature = "repl")]
    {
        use rustyline::error::ReadlineError;
        let editor_mutex = EDITOR.get_or_init(|| {
            Mutex::new(rustyline::DefaultEditor::new().expect("Failed to init rustyline editor"))
        });
        let mut rl = editor_mutex.lock().unwrap();
        match rl.readline("OmegaClaw> ") {
            Ok(line) => {
                let _ = rl.add_history_entry(line.as_str());
                Ok(line.trim().to_string())
            }
            Err(ReadlineError::Interrupted) => Err("Interrupted".into()),
            Err(ReadlineError::Eof) => Err("EOF".into()),
            Err(err) => Err(format!("channel recv: {}", err)),
        }
    }

    #[cfg(not(feature = "repl"))]
    {
        let stdin = io::stdin();
        let mut line = String::new();
        stdin.lock().read_line(&mut line).map_err(|e| format!("channel recv: {}", e))?;
        Ok(line.trim().to_string())
    }
}
