use serde_json::Value;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

pub fn balance_parens(params: &Value) -> Result<String, String> {
    let s = params["str"].as_str().unwrap_or("");
    let s = s.replace("_quote_", "\"").replace("_newline_", "\n");
    let mut sexprs = Vec::new();

    let special_two_arg_cmds = ["write-file", "append-file"];

    for line in s.lines() {
        let mut line = line.trim().to_string();
        if line.is_empty() {
            continue;
        }
        if line.starts_with("(-") {
            line = format!("(pin -{}", &line[2..]);
        } else if line.starts_with("-") {
            line = format!("pin {}", line);
        }

        if line.starts_with('(') && line.ends_with(')') {
            line = line[1..line.len() - 1].trim().to_string();
        }

        let parts: Vec<&str> = line.splitn(2, |c: char| c.is_whitespace()).collect();
        let cmd = parts[0];
        let rest = if parts.len() > 1 { parts[1].trim() } else { "" };

        if special_two_arg_cmds.contains(&cmd) {
            if rest.is_empty() {
                sexprs.push(format!("({})", cmd));
                continue;
            }

            let mut filename = String::new();
            let mut content = String::new();

            if rest.starts_with('"') {
                let mut end = 1;
                let mut escaped = false;
                let chars: Vec<char> = rest.chars().collect();
                while end < chars.len() {
                    let ch = chars[end];
                    if ch == '"' && !escaped {
                        break;
                    }
                    escaped = ch == '\\' && !escaped;
                    if ch != '\\' {
                        escaped = false;
                    }
                    end += 1;
                }
                if end < chars.len() && chars[end] == '"' {
                    filename = chars[..=end].iter().collect();
                    content = chars[end + 1..].iter().collect::<String>().trim().to_string();
                } else {
                    filename = format!("\"{}\"", rest[1..].replace('"', "\\\""));
                }
            } else {
                let split_rest: Vec<&str> = rest.splitn(2, |c: char| c.is_whitespace()).collect();
                filename = format!("\"{}\"", split_rest[0].replace('"', "\\\""));
                if split_rest.len() > 1 {
                    content = split_rest[1].trim().to_string();
                }
            }

            if !content.is_empty() {
                if content.starts_with('"') && content.ends_with('"') {
                    sexprs.push(format!("({} {} {})", cmd, filename, content));
                } else {
                    let c = content.replace('"', "\\\"");
                    sexprs.push(format!("({} {} \"{}\")", cmd, filename, c));
                }
            } else {
                sexprs.push(format!("({} {})", cmd, filename));
            }
            continue;
        }

        if !rest.is_empty() {
            if rest.starts_with('"') && rest.ends_with('"') {
                sexprs.push(format!("({} {})", cmd, rest));
            } else {
                let r = rest.replace('"', "\\\"");
                sexprs.push(format!("({} \"{}\")", cmd, r));
            }
        } else {
            sexprs.push(format!("({})", cmd));
        }
    }

    let ret = sexprs.join(" ");
    Ok(format!("({})", ret))
}

pub fn normalize_string(params: &Value) -> Result<String, String> {
    let x = &params["str"];
    match x {
        Value::String(s) => Ok(s.to_string()),
        _ => Ok(x.to_string()),
    }
}

pub fn around_time(params: &Value) -> Result<String, String> {
    let time_str = params["time"]
        .as_str()
        .unwrap_or("")
        .replace("\\\"", "")
        .replace("\"", "")
        .trim()
        .to_string();
    let k = params["k"].as_u64().unwrap_or(20) as usize;

    // Fallback logic for reading history and parsing date
    // Simple implementation mimicking the python script.

    let target_time = match chrono::NaiveDateTime::parse_from_str(&time_str, "%Y-%m-%d %H:%M:%S") {
        Ok(t) => t.timestamp(),
        Err(_) => return Ok("".to_string()),
    };

    let history_path = Path::new("repos/OmegaClaw-Core/memory/history.metta");
    if !history_path.exists() {
        return Ok("".to_string());
    }

    let file = File::open(history_path).map_err(|e| e.to_string())?;
    let reader = BufReader::new(file);

    let mut buffer = Vec::new();
    let mut best_diff = i64::MAX;
    let mut best_idx = None;

    for (i, line) in reader.lines().enumerate() {
        if let Ok(l) = line {
            buffer.push((i + 1, l.clone()));
            if let Some(ts_start) = l.find("(\"") {
                let ts_part = &l[ts_start + 2..];
                if ts_part.len() >= 19 {
                    if let Ok(ts) =
                        chrono::NaiveDateTime::parse_from_str(&ts_part[0..19], "%Y-%m-%d %H:%M:%S")
                    {
                        let diff = (ts.timestamp() - target_time).abs();
                        if diff < best_diff {
                            best_diff = diff;
                            best_idx = Some(buffer.len() - 1);
                        }
                    }
                }
            }
        }
    }

    if let Some(idx) = best_idx {
        let start = idx.saturating_sub(k);
        let end = (idx + k + 1).min(buffer.len());

        let mut ret = String::new();
        for (lineno, line) in &buffer[start..end] {
            ret.push_str(&format!("{}:{}\n", lineno, line));
        }
        return Ok(ret);
    }

    Ok("".to_string())
}
