use std::collections::VecDeque;
use std::io::{BufRead, Write};
use std::net::TcpStream;
use std::sync::{Arc, Mutex, OnceLock};
use std::time::Duration;

struct IrcState {
    writer: Option<Arc<Mutex<TcpStream>>>,
    messages: VecDeque<String>,
    connected: bool,
    channel: String,
    auth_secret: String,
    authed_nick: Option<String>,
    running: bool,
}

static STATE: OnceLock<Arc<Mutex<IrcState>>> = OnceLock::new();

fn state() -> &'static Arc<Mutex<IrcState>> {
    STATE.get_or_init(|| {
        Arc::new(Mutex::new(IrcState {
            writer: None,
            messages: VecDeque::new(),
            connected: false,
            channel: String::new(),
            auth_secret: String::new(),
            authed_nick: None,
            running: false,
        }))
    })
}

fn normalize_nick(nick: &str) -> String {
    nick.trim().to_lowercase()
}

fn is_allowed_msg(secret: &str, authed: &Option<String>, nick: &str, text: &str) -> &'static str {
    if secret.is_empty() {
        return "allow";
    }
    let lower = text.trim().to_lowercase();
    let candidate = if lower.starts_with("auth ") {
        text.trim()[5..].trim().to_string()
    } else if lower.starts_with("/auth ") {
        text.trim()[6..].trim().to_string()
    } else {
        return if authed.is_none() {
            "ignore"
        } else if normalize_nick(nick) == *authed.as_deref().unwrap_or("") {
            "allow"
        } else {
            "ignore"
        };
    };
    if candidate == secret {
        if authed.is_none() {
            return "auth_bound";
        }
        return "ignore";
    }
    "ignore"
}

fn irc_loop(server: String, port: u16, nick: String, channel: String, auth_secret: String) {
    eprintln!("[IRC] Connecting to {}:{} as {} for {}", server, port, nick, channel);

    let sock = match TcpStream::connect(format!("{}:{}", server, port)) {
        Ok(s) => {
            eprintln!("[IRC] TCP connected");
            s
        }
        Err(e) => {
            eprintln!("[IRC] Connect failed: {}", e);
            return;
        }
    };
    let _ = sock.set_read_timeout(Some(Duration::from_secs(120)));
    let writer = Arc::new(Mutex::new(sock.try_clone().unwrap()));

    {
        let mut s = state().lock().unwrap();
        s.writer = Some(writer.clone());
    }

    let irc_send = |cmd: &str| {
        if let Ok(mut w) = writer.lock() {
            let _ = w.write_all(format!("{}\r\n", cmd).as_bytes());
            let _ = w.flush();
        }
    };

    irc_send(&format!("NICK {}", nick));
    irc_send(&format!("USER {} 0 * :{}", nick, nick));

    let reader = std::io::BufReader::new(sock);
    let mut connected = false;

    for line in reader.lines().flatten() {
        let running = state().lock().unwrap().running;
        if !running {
            break;
        }

        if line.starts_with("PING") {
            let token = line.split_whitespace().nth(1).unwrap_or("");
            irc_send(&format!("PONG {}", token));
            continue;
        }

        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.len() > 1 {
            match parts[1] {
                "001" => {
                    connected = true;
                    state().lock().unwrap().connected = true;
                    eprintln!("[IRC] Registered. Joining {}", channel);
                    irc_send(&format!("JOIN {}", channel));
                }
                "403" | "405" | "471" | "473" | "474" | "475" => {
                    eprintln!("[IRC] Join failed: {}", line);
                }
                "433" => {
                    eprintln!("[IRC] Nickname in use: {}", line);
                }
                _ => {}
            }
        }

        if let Some((prefix, trailing)) = line.split_once(" PRIVMSG ") {
            let nick_from = prefix.split('!').next().unwrap_or("");
            if let Some((_, msg_text)) = trailing.split_once(" :") {
                let mut s = state().lock().unwrap();
                let result = is_allowed_msg(&s.auth_secret, &s.authed_nick, nick_from, msg_text);
                match result {
                    "allow" => {
                        s.messages.push_back(format!("{}: {}", nick_from, msg_text));
                    }
                    "auth_bound" => {
                        irc_send(&format!("PRIVMSG {} :Authentication successful for {}.", channel, nick_from));
                        s.authed_nick = Some(normalize_nick(nick_from));
                    }
                    _ => {}
                }
            }
        }
    }

    {
        let mut s = state().lock().unwrap();
        s.connected = false;
        s.writer = None;
    }
    eprintln!("[IRC] Disconnected");
}

fn rand_suffix() -> String {
    let n = (std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .subsec_nanos()
        % 9000
        + 1000) as u16;
    n.to_string()
}

pub fn connect(server: &str, port: u16, nick: &str, channel: &str, auth_secret: &str) -> Result<String, String> {
    let nick = format!("{}{}", nick, rand_suffix());
    let ch = if channel.starts_with('#') {
        channel.to_string()
    } else {
        format!("#{}", channel)
    };

    {
        let mut s = state().lock().unwrap();
        s.channel = ch.clone();
        s.auth_secret = auth_secret.to_string();
        s.authed_nick = None;
        s.connected = false;
        s.running = true;
        s.messages.clear();
        s.writer = None;
    }

    let srv = server.to_string();
    let n = nick.clone();
    let c = ch.clone();
    let a = auth_secret.to_string();

    std::thread::Builder::new()
        .name("irc-connection".into())
        .spawn(move || irc_loop(srv, port, n, c, a))
        .map_err(|e| format!("Failed to spawn IRC thread: {}", e))?;

    Ok(format!("Connecting to {} as {} on {}", ch, nick, server))
}

pub fn send(msg: &str) -> Result<String, String> {
    let (channel, connected) = {
        let s = state().lock().unwrap();
        (s.channel.clone(), s.connected)
    };
    if !connected {
        return Err("IRC not connected".into());
    }

    let segments: Vec<&str> = msg.split("\\n").collect();
    for segment in segments {
        let clean = segment.replace('\n', " ");
        for chunk in clean.as_bytes().chunks(400) {
            let chunk_str = std::str::from_utf8(chunk).unwrap_or("");
            let cmd = format!("PRIVMSG {} :{}", channel, chunk_str);
            let s = state().lock().unwrap();
            if let Some(ref w) = s.writer {
                if let Ok(mut wlock) = w.lock() {
                    let _ = writeln!(wlock, "{}", cmd);
                    let _ = wlock.flush();
                }
            }
        }
    }
    Ok("ok".into())
}

pub fn recv() -> Result<String, String> {
    let mut s = state().lock().unwrap();
    let mut msgs = Vec::new();
    while let Some(msg) = s.messages.pop_front() {
        msgs.push(msg);
    }
    Ok(msgs.join(" | "))
}

pub fn stop() -> Result<String, String> {
    state().lock().unwrap().running = false;
    Ok("stopped".into())
}
