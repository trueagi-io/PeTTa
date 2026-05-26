use serde_json::Value;

fn api_key(env_var: &str, name: &str) -> Result<String, String> {
    std::env::var(env_var).map_err(|_| format!("{} not configured (set {})", name, env_var))
}

async fn openai_compatible(base_url: &str, model: &str, api_key: &str, prompt: &str, max_tokens: u32) -> Result<String, String> {
    let client = reqwest::Client::new();
    let body = serde_json::json!({
        "model": model,
        "messages": [{"role": "user", "content": prompt}],
        "max_tokens": max_tokens,
    });
    let resp = client.post(format!("{}/chat/completions", base_url))
        .header("Authorization", format!("Bearer {}", api_key))
        .header("Content-Type", "application/json")
        .json(&body)
        .send().await.map_err(|e| format!("LLM request failed: {}", e))?;
    let val: Value = resp.json().await.map_err(|e| format!("LLM response parse: {}", e))?;
    val["choices"][0]["message"]["content"].as_str().map(String::from)
        .ok_or_else(|| format!("LLM response missing content: {}", val))
}

async fn anthropic_call(api_key: &str, model: &str, prompt: &str, max_tokens: u32) -> Result<String, String> {
    let client = reqwest::Client::new();
    let body = serde_json::json!({
        "model": model,
        "max_tokens": max_tokens,
        "messages": [{"role": "user", "content": prompt}],
    });
    let resp = client.post("https://api.anthropic.com/v1/messages")
        .header("x-api-key", api_key)
        .header("anthropic-version", "2023-06-01")
        .header("Content-Type", "application/json")
        .json(&body)
        .send().await.map_err(|e| format!("Anthropic request failed: {}", e))?;
    let val: Value = resp.json().await.map_err(|e| format!("Anthropic response parse: {}", e))?;
    val["content"][0]["text"].as_str().map(String::from)
        .ok_or_else(|| format!("Anthropic response missing text: {}", val))
}

async fn asi_one_call(api_key: &str, prompt: &str, max_tokens: u32) -> Result<String, String> {
    let parts: Vec<&str> = prompt.splitn(2, ":-:-:-:").collect();
    let (system, user) = if parts.len() == 2 { (parts[0], parts[1]) } else { ("", parts[0]) };
    let client = reqwest::Client::new();
    let mut messages = Vec::new();
    if !system.is_empty() {
        messages.push(serde_json::json!({"role": "system", "content": system}));
    }
    messages.push(serde_json::json!({"role": "user", "content": user}));
    let body = serde_json::json!({
        "model": "asi1-ultra",
        "messages": messages,
        "max_tokens": max_tokens,
        "extra_body": {"enable_thinking": true, "thinking_budget": 6000},
    });
    let resp = client.post("https://api.asi1.ai/v1/chat/completions")
        .header("Authorization", format!("Bearer {}", api_key))
        .header("Content-Type", "application/json")
        .json(&body)
        .send().await.map_err(|e| format!("ASIOne request failed: {}", e))?;
    let val: Value = resp.json().await.map_err(|e| format!("ASIOne response parse: {}", e))?;
    val["choices"][0]["message"]["content"].as_str().map(String::from)
        .ok_or_else(|| format!("ASIOne response missing content: {}", val))
}

pub async fn call(params: &Value) -> Result<String, String> {
    let provider = params["provider"].as_str().unwrap_or("OpenAI");
    let prompt = params["prompt"].as_str().unwrap_or("");
    let max_tokens = params["max_tokens"].as_u64().unwrap_or(6000) as u32;

    match provider {
        "Anthropic" => {
            let key = api_key("ANTHROPIC_API_KEY", "Anthropic")?;
            anthropic_call(&key, "claude-opus-4-6", prompt, max_tokens).await
        }
        "OpenAI" => {
            let key = api_key("OPENAI_API_KEY", "OpenAI")?;
            openai_compatible("https://api.openai.com/v1", "gpt-5.4", &key, prompt, max_tokens).await
        }
        "ASICloud" => {
            let key = api_key("ASI_API_KEY", "ASICloud")?;
            openai_compatible("https://inference.asicloud.cudos.org/v1", "minimax/minimax-m2.5", &key, prompt, max_tokens).await
        }
        "ASIOne" => {
            let key = api_key("ASIONE_API_KEY", "ASIOne")?;
            asi_one_call(&key, prompt, max_tokens).await
        }
        "Ollama" => {
            let key = api_key("OLLAMA_API_KEY", "Ollama")?;
            openai_compatible("http://localhost:11434/v1", "qwen3.5:9b", &key, prompt, max_tokens).await
        }
        "OpenRouter" => {
            let key = api_key("OPENROUTER_API_KEY", "OpenRouter")?;
            openai_compatible("https://openrouter.ai/api/v1", "z-ai/glm-5.1", &key, prompt, max_tokens).await
        }
        other => Err(format!("Unknown LLM provider '{}'", other)),
    }
}
