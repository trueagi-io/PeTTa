use serde_json::Value;

pub async fn search(params: &Value) -> Result<String, String> {
    let query = params["query"].as_str().ok_or("missing query")?;
    let url = format!("https://html.duckduckgo.com/html/?q={}", urlencode(query));
    let client = reqwest::Client::builder()
        .user_agent("Mozilla/5.0")
        .build().map_err(|e| format!("client build: {}", e))?;
    let resp = client.get(&url)
        .send().await.map_err(|e| format!("search request: {}", e))?;
    let html = resp.text().await.map_err(|e| format!("search body: {}", e))?;
    extract_snippets(&html)
}

fn urlencode(s: &str) -> String {
    s.chars().map(|c| match c {
        'A'..='Z' | 'a'..='z' | '0'..='9' | '-' | '_' | '.' | '~' => c.to_string(),
        ' ' => "+".into(),
        c => format!("%{:02X}", c as u8),
    }).collect()
}

fn extract_snippets(html: &str) -> Result<String, String> {
    let doc = scraper::Html::parse_document(html);
    let sel = scraper::Selector::parse(".result__snippet")
        .map_err(|e| format!("selector: {}", e))?;
    let mut results = Vec::new();
    for (i, snippet) in doc.select(&sel).enumerate() {
        let text = snippet.text().collect::<String>();
        results.push(format!("{}. {}", i + 1, text.trim()));
    }
    if results.is_empty() {
        Ok("No results found".into())
    } else {
        Ok(results.join("\n"))
    }
}
