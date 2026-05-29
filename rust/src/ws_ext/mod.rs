mod channel;
mod embed;
mod helpers;
mod irc;
mod llm;
mod protocol;
mod search;
mod vector_store;

use futures_util::{SinkExt, StreamExt};
use protocol::{WsRequest, WsResponse};
use tokio::net::TcpListener;
use tokio_tungstenite::accept_async;
use tokio_tungstenite::tungstenite::Message;

pub struct WsExtensionServer {
    pub port: u16,
    shutdown_tx: Option<tokio::sync::oneshot::Sender<()>>,
    thread: Option<std::thread::JoinHandle<()>>,
}

impl WsExtensionServer {
    pub fn spawn(store_path: String) -> Result<Self, String> {
        let (shutdown_tx, shutdown_rx) = tokio::sync::oneshot::channel::<()>();
        let (ready_tx, ready_rx) = std::sync::mpsc::channel();

        let runtime = tokio::runtime::Builder::new_multi_thread()
            .worker_threads(4)
            .enable_all()
            .build()
            .map_err(|e| format!("tokio runtime: {}", e))?;

        let listener = runtime
            .block_on(TcpListener::bind("127.0.0.1:0"))
            .map_err(|e| format!("bind: {}", e))?;
        let port = listener.local_addr().map_err(|e| format!("local addr: {}", e))?.port();

        let thread = std::thread::Builder::new()
            .name("ws-ext-server".into())
            .spawn(move || {
                runtime.block_on(run_server(listener, store_path, shutdown_rx, ready_tx));
            })
            .map_err(|e| format!("thread spawn: {}", e))?;

        // Wait for server to be ready to accept connections
        ready_rx.recv().map_err(|_| "WS server failed to start".to_string())?;

        Ok(Self { port, shutdown_tx: Some(shutdown_tx), thread: Some(thread) })
    }
}

impl Drop for WsExtensionServer {
    fn drop(&mut self) {
        if let Some(tx) = self.shutdown_tx.take() {
            let _ = tx.send(());
        }
        if let Some(thread) = self.thread.take() {
            let _ = thread.join();
        }
    }
}

async fn run_server(
    listener: TcpListener,
    store_path: String,
    mut shutdown_rx: tokio::sync::oneshot::Receiver<()>,
    ready_tx: std::sync::mpsc::Sender<()>,
) {
    vector_store::init(&store_path);
    let _ = ready_tx.send(()); // Signal that we're ready to accept connections

    tokio::select! {
        _ = &mut shutdown_rx => {}
        result = accept_loop(&listener) => {
            if let Err(e) = result {
                eprintln!("WS server error: {}", e);
            }
        }
    }
}

async fn accept_loop(listener: &TcpListener) -> Result<(), String> {
    eprintln!("[WS] accept_loop started, waiting for connections");
    let (stream, _) = listener.accept().await.map_err(|e| format!("accept: {}", e))?;
    eprintln!("[WS] first connection accepted");
    let ws_stream = accept_async(stream).await.map_err(|e| format!("ws accept: {}", e))?;
    let (mut write, mut read) = ws_stream.split();

    eprintln!("[WS] WebSocket handshake complete");
    while let Some(msg) = read.next().await {
        if let Ok(msg) = msg {
            if let Some(text) = msg.to_text().ok().map(String::from) {
                eprintln!("[WS] received: {}", &text[..text.len().min(200)]);
                let response = handle_message(&text).await;
                if let Ok(json) = serde_json::to_string(&response) {
                    let _ = write.send(Message::Text(json)).await;
                }
            }
        }
    }

    Ok(())
}

async fn handle_message(text: &str) -> WsResponse {
    let req: WsRequest = match serde_json::from_str(text) {
        Ok(r) => r,
        Err(e) => return WsResponse::err(0, format!("invalid JSON: {}", e)),
    };

    let result = match req.method.as_str() {
        "llm_call" => match llm::call(&req.params).await {
            Ok(s) => serde_json::Value::String(s),
            Err(e) => return WsResponse::err(req.id, e),
        },
        "embed" => match embed::call(&req.params).await {
            Ok(v) => serde_json::Value::Array(
                v.into_iter().map(|f| serde_json::Value::from(f as f64)).collect(),
            ),
            Err(e) => return WsResponse::err(req.id, e),
        },
        "vector_remember" => match vector_store::remember(&req.params) {
            Ok(s) => serde_json::Value::String(s),
            Err(e) => return WsResponse::err(req.id, e),
        },
        "vector_query" => match vector_store::query(&req.params) {
            Ok(v) => serde_json::Value::Array(v),
            Err(e) => return WsResponse::err(req.id, e),
        },
        "web_search" => match search::search(&req.params).await {
            Ok(s) => serde_json::Value::String(s),
            Err(e) => return WsResponse::err(req.id, e),
        },
        "channel_send" => match channel::send(&req.params) {
            Ok(s) => serde_json::Value::String(s),
            Err(e) => return WsResponse::err(req.id, e),
        },
        "channel_recv" => match channel::recv() {
            Ok(s) => serde_json::Value::String(s),
            Err(e) => return WsResponse::err(req.id, e),
        },
        "helper_balance_parens" => match helpers::balance_parens(&req.params) {
            Ok(s) => serde_json::Value::String(s),
            Err(e) => return WsResponse::err(req.id, e),
        },
        "helper_normalize_string" => match helpers::normalize_string(&req.params) {
            Ok(s) => serde_json::Value::String(s),
            Err(e) => return WsResponse::err(req.id, e),
        },
        "helper_around_time" => match helpers::around_time(&req.params) {
            Ok(s) => serde_json::Value::String(s),
            Err(e) => return WsResponse::err(req.id, e),
        },
        "irc_connect" => {
            let server = req.params["server"].as_str().unwrap_or("irc.libera.chat");
            let port = req.params["port"].as_u64().unwrap_or(6667) as u16;
            let nick = req.params["nick"].as_str().unwrap_or("omegaclaw");
            let channel = req.params["channel"].as_str().unwrap_or("#omegaclaw");
            let auth_secret = req.params["auth_secret"].as_str().unwrap_or("");
            eprintln!(
                "[WS] irc_connect: srv={} port={} nick={} ch={}",
                server, port, nick, channel
            );
            match irc::connect(server, port, nick, channel, auth_secret) {
                Ok(s) => serde_json::Value::String(s),
                Err(e) => return WsResponse::err(req.id, e),
            }
        }
        "irc_send" => {
            let msg = req.params["msg"].as_str().unwrap_or("");
            match irc::send(msg) {
                Ok(s) => serde_json::Value::String(s),
                Err(e) => return WsResponse::err(req.id, e),
            }
        }
        "irc_recv" => match irc::recv() {
            Ok(s) => serde_json::Value::String(s),
            Err(e) => return WsResponse::err(req.id, e),
        },
        "irc_stop" => match irc::stop() {
            Ok(s) => serde_json::Value::String(s),
            Err(e) => return WsResponse::err(req.id, e),
        },
        other => return WsResponse::err(req.id, format!("unknown method '{}'", other)),
    };

    WsResponse::ok(req.id, result)
}
