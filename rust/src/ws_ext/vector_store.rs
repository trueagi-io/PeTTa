use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::sync::{RwLock, OnceLock};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VectorDbEntry {
    pub text: String,
    pub vector: Vec<f32>,
    pub timestamp: String,
}

pub struct VectorStore {
    entries: Vec<VectorDbEntry>,
    max_entries: usize,
    dirty: bool,
}

impl VectorStore {
    pub fn new(max_entries: usize) -> Self {
        Self { entries: Vec::new(), max_entries, dirty: false }
    }

    pub fn remember(&mut self, text: String, vector: Vec<f32>, timestamp: String) {
        if self.entries.len() >= self.max_entries {
            self.entries.remove(0);
        }
        self.entries.push(VectorDbEntry { text, vector, timestamp });
        self.dirty = true;
    }

    pub fn query(&self, query_vec: &[f32], n: usize) -> Vec<Value> {
        let mut scored: Vec<(f32, &VectorDbEntry)> = self.entries.iter()
            .map(|e| (cosine_similarity(query_vec, &e.vector), e))
            .collect();
        scored.sort_by(|a, b| b.0.partial_cmp(&a.0).unwrap_or(std::cmp::Ordering::Equal));
        scored.truncate(n);
        scored.into_iter().map(|(score, e)| {
            serde_json::json!({"text": e.text, "score": score, "timestamp": e.timestamp})
        }).collect()
    }

    pub fn save(&self, path: &str) {
        if !self.dirty { return; }
        if let Ok(json) = serde_json::to_string(&self.entries) {
            let _ = std::fs::write(path, &json);
        }
    }

    pub fn load(&mut self, path: &str) {
        if let Ok(data) = std::fs::read_to_string(path) {
            if let Ok(entries) = serde_json::from_str::<Vec<VectorDbEntry>>(&data) {
                self.entries = entries;
                self.dirty = false;
            }
        }
    }
}

fn cosine_similarity(a: &[f32], b: &[f32]) -> f32 {
    let dot: f32 = a.iter().zip(b).map(|(x, y)| x * y).sum();
    let na: f32 = a.iter().map(|x| x * x).sum();
    let nb: f32 = b.iter().map(|x| x * x).sum();
    let denom = na.sqrt() * nb.sqrt();
    if denom == 0.0 { 0.0 } else { dot / denom }
}

static VECTOR_STORE: OnceLock<RwLock<VectorStore>> = OnceLock::new();

pub fn init(store_path: &str) {
    let path = store_path.to_string();
    VECTOR_STORE.get_or_init(|| {
        let mut store = VectorStore::new(10000);
        store.load(&path);
        RwLock::new(store)
    });
}

pub fn remember(params: &Value) -> Result<String, String> {
    let text = params["text"].as_str().ok_or("missing text")?;
    let ts = params["ts"].as_str().unwrap_or("");
    let vec: Vec<f32> = params["vector"].as_array()
        .ok_or("missing vector")?
        .iter().map(|v| v.as_f64().unwrap_or(0.0) as f32).collect();
    if let Some(store) = VECTOR_STORE.get() {
        if let Ok(mut s) = store.write() {
            s.remember(text.to_string(), vec, ts.to_string());
            s.save("repos/OmegaClaw-Core/memory/vector_store.json");
        }
    }
    Ok("ok".into())
}

pub fn query(params: &Value) -> Result<Vec<Value>, String> {
    let vec: Vec<f32> = params["vector"].as_array()
        .ok_or("missing vector")?
        .iter().map(|v| v.as_f64().unwrap_or(0.0) as f32).collect();
    let n = params["n"].as_u64().unwrap_or(20) as usize;
    if let Some(store) = VECTOR_STORE.get() {
        if let Ok(s) = store.read() {
            return Ok(s.query(&vec, n));
        }
    }
    Ok(Vec::new())
}
