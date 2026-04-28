//! MORK backend implementation using internal Rust MORK.

use crate::mork::interpreter::{Interpreter, MettaValue};
use crate::mork::space::Space;
use crate::pathmap::PathMap;
use std::sync::{Arc, Mutex};

pub struct MORKEngine {
    space: Arc<Mutex<Space>>,
    interpreter: Interpreter,
}

impl MORKEngine {
    pub fn new() -> Self {
        let space = Space::new();
        let space_arc = Arc::new(Mutex::new(space));
        MORKEngine {
            space: space_arc.clone(),
            interpreter: Interpreter::new(space_arc),
        }
    }

    pub fn add_atoms(&mut self, input: &str) -> Result<String, String> {
        let mut space = self.space.lock().map_err(|e| e.to_string())?;
        match space.add_all_sexpr(input.as_bytes()) {
            Ok(_) => Ok("OK: loaded".to_string()),
            Err(e) => Ok(format!("ERR: {}", e)),
        }
    }

    pub fn remove_atoms(&mut self, input: &str) -> Result<String, String> {
        let mut space = self.space.lock().map_err(|e| e.to_string())?;
        match space.remove_all_sexpr(input.as_bytes()) {
            Ok(_) => Ok("OK: loaded".to_string()),
            Err(e) => Ok(format!("ERR: {}", e)),
        }
    }

    pub fn get_atoms(&mut self) -> Result<String, String> {
        let space = self.space.lock().map_err(|e| e.to_string())?;
        let mut output = Vec::new();
        match space.dump_all_sexpr(&mut output) {
            Ok(_) => Ok(String::from_utf8_lossy(&output).to_string()),
            Err(e) => Ok(format!("ERR: {}", e)),
        }
    }

    pub fn match_pattern(&mut self, input: &str) -> Result<String, String> {
        let mut space = self.space.lock().map_err(|e| e.to_string())?;
        let mut parsebuf = [0u8; 4096];
        let mut output = Vec::new();
        
        let (qexpr, _used) = match space.parse_sexpr(input.as_bytes(), parsebuf.as_mut_ptr()) {
            Ok(r) => r,
            Err(e) => return Ok(format!("ERR: parse failed")),
        };
        
        let mut ez = crate::mork::expr::ExprZipper::new(qexpr);
        if !ez.next_child() {
            return Ok("ERR: empty pattern".to_string());
        }
        let pattern = ez.subexpr();
        if !ez.next_child() {
            return Ok("ERR: pattern requires tuple".to_string());
        }
        let template = ez.subexpr();
        
        let count = space.dump_sexpr(pattern, template, &mut output);
        Ok(String::from_utf8_lossy(&output).to_string())
    }

    pub fn mm2_exec(&mut self, steps: &str) -> Result<String, String> {
        let mut space = self.space.lock().map_err(|e| e.to_string())?;
        let steps: usize = steps.trim().parse().unwrap_or(1);
        space.metta_calculus(steps);
        Ok("OK: executed".to_string())
    }

pub fn process(&mut self, code: &str) -> Vec<String> {
let mut results = Vec::new();
let mut buffer = String::new();
let mut paren_depth = 0;

for line in code.lines() {
let line = line.trim();
if line.is_empty() {
continue;
}

// If we're accumulating a multi-line expression, keep going
if !buffer.is_empty() {
buffer.push(' ');
buffer.push_str(line);
// Recalculate paren depth
paren_depth = buffer.chars().filter(|&c| c == '(').count() as i32
- buffer.chars().filter(|&c| c == ')').count() as i32;

if paren_depth <= 0 {
// Complete expression - process it
let result = self.process_line(&buffer);
results.push(result);
buffer.clear();
paren_depth = 0;
}
continue;
}

// Check if this line starts a multi-line expression
if line.starts_with('(') {
paren_depth = line.chars().filter(|&c| c == '(').count() as i32
- line.chars().filter(|&c| c == ')').count() as i32;

if paren_depth > 0 {
// Multi-line expression - start buffering
buffer = line.to_string();
continue;
}
}

let result = self.process_line(line);
results.push(result);
}

// Process any remaining buffered content
if !buffer.is_empty() {
let result = self.process_line(&buffer);
results.push(result);
}

if results.is_empty() {
results.push("true".to_string());
}

results
}

fn process_line(&mut self, line: &str) -> String {
let line = line.trim();
if line.is_empty() {
return "OK".to_string();
}

let parts: Vec<&str> = line.splitn(2, ' ').collect();
let cmd = parts.get(0).map(|s| *s).unwrap_or("");

match cmd {
"add-atoms" => self.add_atoms(parts.get(1).unwrap_or(&"")).unwrap_or_else(|e| format!("ERR: {}", e)),
"remove-atoms" => self.remove_atoms(parts.get(1).unwrap_or(&"")).unwrap_or_else(|e| format!("ERR: {}", e)),
"get-atoms" => self.get_atoms().unwrap_or_else(|e| format!("ERR: {}", e)),
"match" => self.match_pattern(parts.get(1).unwrap_or(&"")).unwrap_or_else(|e| format!("ERR: {}", e)),
"mm2-exec" => self.mm2_exec(parts.get(1).unwrap_or(&"")).unwrap_or_else(|e| format!("ERR: {}", e)),
_ => {
if line.starts_with('!') && line.ends_with(')') {
// Evaluation: !(...)
let inner = &line[1..];
self.interpreter.eval(inner)
.map(|v| v.to_string())
.unwrap_or_else(|e| format!("ERR: {}", e))
} else if line.starts_with("(= ") {
// Definition - add to space
let mut space = match self.space.lock() {
Ok(s) => s,
Err(e) => return format!("LOCK ERR: {}", e),
};
match space.add_all_sexpr(line.as_bytes()) {
Ok(_) => "OK".to_string(),
Err(e) => format!("ERR: {}", e),
}
} else {
// Comment or unknown - OK
"OK".to_string()
}
}
}
}
}

impl Default for MORKEngine {
    fn default() -> Self {
        Self::new()
    }
}