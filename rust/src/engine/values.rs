// ---------------------------------------------------------------------------
// Typed MeTTa values
// ---------------------------------------------------------------------------

/// Represents a parsed MeTTa value (AST node).
///
/// MeTTa values can be integers, floats, booleans, atoms,
/// lists, or function applications (expressions).
#[derive(Debug, Clone, PartialEq)]
pub enum MettaValue {
    Integer(String),
    Float(f64),
    Bool(bool),
    Atom(String),
    List(Vec<MettaValue>),
    Expression(String, Vec<MettaValue>),
}

impl MettaValue {
    pub fn parse(s: &str) -> Option<Self> {
        parse_sexpr(s.trim())
    }
}

fn parse_sexpr(s: &str) -> Option<MettaValue> {
    let s = s.trim();
    if s.is_empty() {
        return None;
    }
    if s == "true" {
        return Some(MettaValue::Bool(true));
    }
    if s == "false" {
        return Some(MettaValue::Bool(false));
    }
    if s.parse::<i64>().is_ok() {
        return Some(MettaValue::Integer(s.to_string()));
    }
    if let Ok(f) = s.parse::<f64>() {
        return Some(MettaValue::Float(f));
    }
    if !s.starts_with('(') {
        return Some(MettaValue::Atom(s.to_string()));
    }
    if let Some(inner) = strip_parens(s) {
        let items = split_top_level(inner);
        if items.is_empty() {
            return Some(MettaValue::List(Vec::new()));
        }
        let parsed: Vec<MettaValue> = items.iter().filter_map(|x| parse_sexpr(x)).collect();
        if parsed.len() == items.len() && !parsed.is_empty() {
            if let MettaValue::Atom(head) = &parsed[0] {
                if parsed.len() > 1 {
                    return Some(MettaValue::Expression(head.clone(), parsed[1..].to_vec()));
                }
            }
            return Some(MettaValue::List(parsed));
        }
    }
    Some(MettaValue::Atom(s.to_string()))
}

fn strip_parens(s: &str) -> Option<&str> {
    if s.starts_with('(') && s.ends_with(')') && is_balanced(&s[1..s.len() - 1]) {
        Some(&s[1..s.len() - 1])
    } else {
        None
    }
}

fn is_balanced(s: &str) -> bool {
    let mut d = 0i32;
    for c in s.chars() {
        match c {
            '(' => d += 1,
            ')' => {
                d -= 1;
                if d < 0 {
                    return false;
                }
            }
            _ => {}
        }
    }
    d == 0
}

fn split_top_level(s: &str) -> Vec<String> {
    let mut items = Vec::new();
    let mut cur = String::new();
    let (mut depth, mut in_q, mut esc) = (0i32, false, false);
    for ch in s.chars() {
        if esc {
            cur.push(ch);
            esc = false;
            continue;
        }
        if ch == '\\' {
            esc = true;
            cur.push(ch);
            continue;
        }
        if ch == '"' {
            in_q = !in_q;
            cur.push(ch);
            continue;
        }
        if in_q {
            cur.push(ch);
            continue;
        }
        match ch {
            '(' => {
                depth += 1;
                cur.push(ch);
            }
            ')' => {
                depth -= 1;
                cur.push(ch);
            }
            ' ' | '\t' | '\n' | '\r' if depth == 0 => {
                if !cur.is_empty() {
                    items.push(std::mem::take(&mut cur));
                }
            }
            _ => cur.push(ch),
        }
    }
    if !cur.is_empty() {
        items.push(cur);
    }
    items
}

// ---------------------------------------------------------------------------
// MeTTa result
// ---------------------------------------------------------------------------

/// A result from executing a MeTTa query.
///
/// Contains the raw string value returned by the Prolog backend.
/// Use [`parsed_value`](Self::parsed_value) to get a typed [`MettaValue`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MettaResult {
    pub value: String,
}

impl MettaResult {
    pub fn parsed_value(&self) -> Option<MettaValue> {
        MettaValue::parse(&self.value)
    }
}
