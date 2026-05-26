use serde::{Deserialize, Serialize};
use serde_json::Value;

#[derive(Debug, Deserialize)]
pub struct WsRequest {
    pub id: u64,
    pub method: String,
    pub params: Value,
}

#[derive(Debug, Serialize)]
pub struct WsResponse {
    pub id: u64,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<String>,
}

impl WsResponse {
    pub fn ok(id: u64, result: Value) -> Self {
        Self { id, result: Some(result), error: None }
    }

    pub fn err(id: u64, msg: String) -> Self {
        Self { id, result: None, error: Some(msg) }
    }
}
