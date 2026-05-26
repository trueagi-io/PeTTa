use std::time::Duration;
use tungstenite::{connect, Message, WebSocket, stream::MaybeTlsStream};

type WsStream = WebSocket<MaybeTlsStream<std::net::TcpStream>>;

fn connect_ws(port: u16) -> WsStream {
    let url = format!("ws://127.0.0.1:{}", port);
    let (ws, _) = connect(url).unwrap();
    ws
}

fn send_req(ws: &mut WsStream, id: u64, method: &str, params: serde_json::Value) -> serde_json::Value {
    let req = serde_json::json!({"id": id, "method": method, "params": params});
    ws.write(Message::Text(req.to_string())).unwrap();
    ws.flush().unwrap();
    let resp = ws.read().unwrap();
    serde_json::from_str(resp.to_text().unwrap()).unwrap()
}

#[test]
fn test_ws_server_spawn_and_lifecycle() {
    let _server = petta::ws_ext::WsExtensionServer::spawn("/tmp/test_vs.json".into()).unwrap();
}

#[test]
fn test_ws_channel_send() {
    let server = petta::ws_ext::WsExtensionServer::spawn("/tmp/test_vs.json".into()).unwrap();
    std::thread::sleep(Duration::from_millis(200));
    let mut ws = connect_ws(server.port);

    let resp = send_req(&mut ws, 1, "channel_send", serde_json::json!({"msg": "hello"}));
    assert_eq!(resp["id"], 1);
    assert_eq!(resp["result"], "ok");
}

#[test]
fn test_ws_vector_remember_and_query() {
    let server = petta::ws_ext::WsExtensionServer::spawn("/tmp/test_vs2.json".into()).unwrap();
    std::thread::sleep(Duration::from_millis(200));
    let mut ws = connect_ws(server.port);

    let resp = send_req(&mut ws, 1, "vector_remember", serde_json::json!({
        "text": "test entry A", "vector": [0.1, 0.2, 0.3], "ts": "2026-01-01"
    }));
    assert_eq!(resp["result"], "ok");

    let resp = send_req(&mut ws, 2, "vector_remember", serde_json::json!({
        "text": "test entry B", "vector": [0.4, 0.5, 0.6], "ts": "2026-01-02"
    }));
    assert_eq!(resp["result"], "ok");

    let resp = send_req(&mut ws, 3, "vector_query", serde_json::json!({
        "vector": [0.1, 0.2, 0.3], "n": 10
    }));
    let results = resp["result"].as_array().unwrap();
    assert!(results.len() >= 2);
    assert_eq!(results[0]["text"], "test entry A");
    assert!(results[0]["score"].as_f64().unwrap() > 0.99);
}

#[test]
fn test_ws_unknown_method() {
    let server = petta::ws_ext::WsExtensionServer::spawn("/tmp/test_vs3.json".into()).unwrap();
    std::thread::sleep(Duration::from_millis(200));
    let mut ws = connect_ws(server.port);

    let resp = send_req(&mut ws, 99, "nonexistent", serde_json::json!({}));
    assert_eq!(resp["id"], 99);
    assert!(!resp["error"].is_null());
    assert!(resp["result"].is_null());
}

#[test]
fn test_ws_bad_json() {
    let server = petta::ws_ext::WsExtensionServer::spawn("/tmp/test_vs4.json".into()).unwrap();
    std::thread::sleep(Duration::from_millis(200));
    let mut ws = connect_ws(server.port);

    ws.write(Message::Text("not json".into())).unwrap();
    ws.flush().unwrap();
    let resp = ws.read().unwrap();
    let val: serde_json::Value = serde_json::from_str(resp.to_text().unwrap()).unwrap();
    assert!(!val["error"].is_null());
}

#[test]
fn test_ws_llm_call_missing_key() {
    unsafe { std::env::remove_var("OPENAI_API_KEY"); }
    let server = petta::ws_ext::WsExtensionServer::spawn("/tmp/test_vs5.json".into()).unwrap();
    std::thread::sleep(Duration::from_millis(200));
    let mut ws = connect_ws(server.port);

    let resp = send_req(&mut ws, 1, "llm_call", serde_json::json!({
        "provider": "OpenAI", "prompt": "hello", "max_tokens": 100
    }));
    assert_eq!(resp["id"], 1);
    assert!(!resp["error"].is_null());
    let err = resp["error"].as_str().unwrap();
    assert!(err.contains("not configured") || err.contains("OPENAI_API_KEY"));
}

#[test]
fn test_ws_llm_call_ollama() {
    let server = petta::ws_ext::WsExtensionServer::spawn("/tmp/test_vs6.json".into()).unwrap();
    std::thread::sleep(Duration::from_millis(500));
    let mut ws = connect_ws(server.port);

    let resp = send_req(&mut ws, 1, "llm_call", serde_json::json!({
        "provider": "Ollama",
        "prompt": "say hello in one word",
        "max_tokens": 20
    }));
    assert_eq!(resp["id"], 1);
    if let Some(err) = resp.get("error") {
        eprintln!("Ollama LLM error (may be expected if env not set): {}", err);
    } else if let Some(result) = resp.get("result") {
        eprintln!("Ollama LLM result: {}", result);
    }
}

#[test]
fn test_ws_irc_connect() {
    let server = petta::ws_ext::WsExtensionServer::spawn("/tmp/test_vs7.json".into()).unwrap();
    std::thread::sleep(Duration::from_millis(200));
    let mut ws = connect_ws(server.port);

    let resp = send_req(&mut ws, 1, "irc_connect", serde_json::json!({
        "server": "irc.quakenet.org",
        "port": 6667,
        "nick": "omegaclaw_test",
        "channel": "##metta",
        "auth_secret": ""
    }));
    assert_eq!(resp["id"], 1);
    if let Some(err) = resp.get("error") {
        eprintln!("IRC connect error: {}", err);
    } else {
        eprintln!("IRC connect result: {}", resp["result"]);
        std::thread::sleep(Duration::from_secs(10));
        // Check if we got any messages
        let resp2 = send_req(&mut ws, 2, "irc_recv", serde_json::json!({}));
        if let Some(err) = resp2.get("error") {
            eprintln!("IRC recv error: {}", err);
        } else {
            eprintln!("IRC recv result: '{}'", resp2["result"]);
        }
        // Send a test message
        let resp3 = send_req(&mut ws, 3, "irc_send", serde_json::json!({"msg": "Hello from OmegaClaw test!"}));
        if let Some(err) = resp3.get("error") {
            eprintln!("IRC send error: {}", err);
        } else {
            eprintln!("IRC send result: {}", resp3["result"]);
        }
        // Receive again after send
        std::thread::sleep(Duration::from_secs(2));
        let resp4 = send_req(&mut ws, 4, "irc_recv", serde_json::json!({}));
        eprintln!("IRC recv after send: '{}'", resp4["result"]);
        // Stop IRC
        let _ = send_req(&mut ws, 5, "irc_stop", serde_json::json!({}));
    }
}
