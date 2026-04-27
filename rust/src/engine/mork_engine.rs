//! MORK backend implementation using internal Rust MORK.

use crate::mork::space::Space;
use crate::pathmap::PathMap;
use std::sync::{Arc, Mutex};

pub struct MORKEngine {
    space: Arc<Mutex<Space>>,
}

impl MORKEngine {
    pub fn new() -> Self {
        let space = Space::new();
        MORKEngine {
            space: Arc::new(Mutex::new(space)),
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
        
        for line in code.lines() {
            let line = line.trim();
            if line.is_empty() {
                continue;
            }
            
            let parts: Vec<&str> = line.splitn(2, ' ').collect();
            let cmd = parts.get(0).map(|s| *s).unwrap_or("");
            let input = parts.get(1).map(|s| *s).unwrap_or("");
            
            let result = match cmd {
                "add-atoms" => self.add_atoms(input),
                "remove-atoms" => self.remove_atoms(input),
                "get-atoms" => self.get_atoms(),
                "match" => self.match_pattern(input),
                "mm2-exec" => self.mm2_exec(input),
                _ => {
                    if line.starts_with("!(") {
                        Ok(format!("TODO: eval {}", &line[2..]))
                    } else {
                        Ok(line.to_string())
                    }
                }
            };
            
            results.push(result.unwrap_or_else(|e| e));
        }
        
        if results.is_empty() {
            results.push("true".to_string());
        }
        
        results
    }
}

impl Default for MORKEngine {
    fn default() -> Self {
        Self::new()
    }
}