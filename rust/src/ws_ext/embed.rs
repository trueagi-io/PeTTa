use serde_json::Value;
use std::collections::VecDeque;
use std::sync::Mutex;

static EMBED_CACHE: once_cell::sync::Lazy<Mutex<VecDeque<(String, Vec<f32>)>>> =
    once_cell::sync::Lazy::new(|| Mutex::new(VecDeque::with_capacity(1024)));

pub async fn call(params: &Value) -> Result<Vec<f32>, String> {
    let text = params["text"].as_str().unwrap_or("");
    let provider = params["provider"].as_str().unwrap_or("OpenAI");

    if text.is_empty() {
        return Err("empty text".into());
    }

    if let Some(cached) = check_cache(text) {
        return Ok(cached);
    }

    match provider {
        "OpenAI" => {
            let key = std::env::var("OPENAI_API_KEY")
                .map_err(|_| "OpenAI embedding not configured (set OPENAI_API_KEY)".to_string())?;
            let client = reqwest::Client::new();
            let body = serde_json::json!({
                "model": "text-embedding-3-small",
                "input": text,
            });
            let resp = client.post("https://api.openai.com/v1/embeddings")
                .header("Authorization", format!("Bearer {}", key))
                .header("Content-Type", "application/json")
                .json(&body)
                .send().await.map_err(|e| format!("Embedding request failed: {}", e))?;
            let val: Value = resp.json().await.map_err(|e| format!("Embedding response parse: {}", e))?;
            let vec: Vec<f32> = val["data"][0]["embedding"]
                .as_array().ok_or("no embedding in response")?
                .iter().map(|v| v.as_f64().unwrap_or(0.0) as f32).collect();
            cache(text, vec.clone());
            Ok(vec)
        }
        "Local" => Err("Local embedding not supported yet".into()),
        other => Err(format!("Unknown embedding provider '{}'", other)),
    }
}

fn check_cache(text: &str) -> Option<Vec<f32>> {
    let cache = EMBED_CACHE.lock().ok()?;
    for (t, v) in cache.iter() {
        if t == text {
            return Some(v.clone());
        }
    }
    None
}

fn cache(text: &str, vec: Vec<f32>) {
    if let Ok(mut cache) = EMBED_CACHE.lock() {
        if cache.len() >= 1024 {
            cache.pop_front();
        }
        cache.push_back((text.to_string(), vec));
    }
}
