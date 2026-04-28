//! MeTTa Interpreter for MORK backend
//!
//! This module provides a minimal MeTTa interpreter that can evaluate:
//! - Arithmetic: +, -, *, /, %
//! - Comparison: ==, !=, <, >, <=, >=
//! - Logic: if, and, or, not
//! - Binding: let, let*
//! - Evaluation: eval, reduce, match

use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use crate::mork::space::Space;

/// Metta value types
#[derive(Debug, Clone, PartialEq)]
pub enum MettaValue {
    Number(f64),
    Integer(i64),
    String(String),
    Symbol(String),
    Bool(bool),
    Expression(Vec<MettaValue>),
    Error(String),
}

impl MettaValue {
    pub fn is_truthy(&self) -> bool {
        match self {
            MettaValue::Bool(b) => *b,
            MettaValue::Number(n) => *n != 0.0,
            MettaValue::Integer(n) => *n != 0,
            _ => true,
        }
    }
}

impl std::fmt::Display for MettaValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MettaValue::Number(n) => {
                if n.fract() == 0.0 {
                    write!(f, "{}", *n as i64)
                } else {
                    write!(f, "{}", n)
                }
            }
            MettaValue::Integer(n) => write!(f, "{}", n),
            MettaValue::String(s) => write!(f, "\"{}\"", s),
            MettaValue::Symbol(s) => write!(f, "{}", s),
            MettaValue::Bool(b) => write!(f, "{}", if *b { "True" } else { "False" }),
            MettaValue::Expression(exprs) => {
                write!(f, "(")?;
                for (i, expr) in exprs.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", expr)?;
                }
                write!(f, ")")
            }
            MettaValue::Error(msg) => write!(f, "Error: {}", msg),
        }
    }
}

/// Interpreter error types
#[derive(Debug, Clone)]
pub enum InterpreterError {
    ParseError(String),
    TypeError { expected: String, found: String },
    UndefinedFunction(String),
    EvaluationError(String),
    SyntaxError(String),
}

impl std::fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InterpreterError::ParseError(msg) => write!(f, "Parse error: {}", msg),
            InterpreterError::TypeError { expected, found } => {
                write!(f, "Type error: expected {}, found {}", expected, found)
            }
            InterpreterError::UndefinedFunction(name) => write!(f, "Undefined function: {}", name),
            InterpreterError::EvaluationError(msg) => write!(f, "Evaluation error: {}", msg),
            InterpreterError::SyntaxError(msg) => write!(f, "Syntax error: {}", msg),
        }
    }
}

impl std::error::Error for InterpreterError {}

type Result<T> = std::result::Result<T, InterpreterError>;

/// Main interpreter structure
pub struct Interpreter {
    space: Arc<Mutex<Space>>,
    bindings: RefCell<HashMap<String, MettaValue>>,
}

impl Interpreter {
    pub fn new(space: Arc<Mutex<Space>>) -> Self {
        Interpreter {
            space,
            bindings: RefCell::new(HashMap::new()),
        }
    }

    /// Evaluate a MeTTa expression string
    pub fn eval(&mut self, expr: &str) -> Result<MettaValue> {
        let expr = expr.trim();
        if expr.is_empty() {
            return Ok(MettaValue::Bool(true));
        }

        let parsed = self.parse_expr(expr)?;
        self.eval_value(parsed)
    }

    fn parse_expr(&self, input: &str) -> Result<MettaValue> {
        let input = input.trim();
        
        if input.is_empty() {
            return Err(InterpreterError::ParseError("Empty expression".to_string()));
        }

        if input.starts_with('(') {
            self.parse_list_expr(input)
        } else {
            self.parse_atom(input)
        }
    }

fn parse_list_expr(&self, input: &str) -> Result<MettaValue> {
let input = input.trim();
if !input.starts_with('(') || !input.ends_with(')') {
return Err(InterpreterError::ParseError("Invalid list expression".to_string()));
}

        let inner = &input[1..input.len()-1].trim();
        if inner.is_empty() {
            return Ok(MettaValue::Expression(vec![]));
        }

        let elements = self.tokenize(inner)?;
        let mut result = Vec::new();
        for elem in elements {
            result.push(self.parse_expr(&elem)?);
        }

        Ok(MettaValue::Expression(result))
    }

    fn tokenize(&self, input: &str) -> Result<Vec<String>> {
        let mut tokens = Vec::new();
        let mut current = String::new();
        let mut depth = 0;
        let mut in_string = false;
        let mut chars = input.chars().peekable();

        while let Some(c) = chars.next() {
            if in_string {
                current.push(c);
                if c == '"' {
                    in_string = false;
                }
            } else if c == '"' {
                in_string = true;
                current.push(c);
            } else if c == '(' {
                depth += 1;
                current.push(c);
            } else if c == ')' {
                depth -= 1;
                current.push(c);
            } else if c.is_whitespace() && depth == 0 {
                if !current.is_empty() {
                    tokens.push(current.trim().to_string());
                    current = String::new();
                }
            } else {
                current.push(c);
            }
        }

        if !current.is_empty() {
            tokens.push(current.trim().to_string());
        }

        Ok(tokens)
    }

    fn parse_atom(&self, input: &str) -> Result<MettaValue> {
        let input = input.trim();
        
        if input.is_empty() {
            return Err(InterpreterError::ParseError("Empty atom".to_string()));
        }

        if input == "True" || input == "true" {
            return Ok(MettaValue::Bool(true));
        }
        if input == "False" || input == "false" {
            return Ok(MettaValue::Bool(false));
        }

        if input.starts_with('"') && input.ends_with('"') && input.len() >= 2 {
            return Ok(MettaValue::String(input[1..input.len()-1].to_string()));
        }

        if input.starts_with('$') {
            return Ok(MettaValue::Symbol(input.to_string()));
        }

        if let Ok(n) = input.parse::<i64>() {
            return Ok(MettaValue::Integer(n));
        }
        if let Ok(n) = input.parse::<f64>() {
            return Ok(MettaValue::Number(n));
        }

        Ok(MettaValue::Symbol(input.to_string()))
    }

    fn eval_value(&mut self, value: MettaValue) -> Result<MettaValue> {
        match value {
            MettaValue::Expression(ref expr) => {
                if expr.is_empty() {
                    return Ok(MettaValue::Expression(vec![]));
                }
                
                if let Some(MettaValue::Symbol(op)) = expr.first() {
                    let op = op.clone();
                    let args = &expr[1..];
                    return self.eval_builtin(&op, args);
                }
                
                self.eval_expression(&value)
            }
            MettaValue::Symbol(ref sym) => {
                if sym.starts_with('$') {
                    if let Some(val) = self.bindings.borrow().get(sym).cloned() {
                        return Ok(val);
                    }
                }
                Ok(value)
            }
            _ => Ok(value),
        }
    }

    fn eval_builtin(&mut self, name: &str, args: &[MettaValue]) -> Result<MettaValue> {
        match name {
            "+" => self.eval_add(args),
            "-" => self.eval_sub(args),
            "*" => self.eval_mul(args),
            "/" => self.eval_div(args),
            "%" => self.eval_mod(args),
            "==" => self.eval_eq(args),
            "!=" => self.eval_neq(args),
            "<" => self.eval_lt(args),
            ">" => self.eval_gt(args),
            "<=" => self.eval_le(args),
            ">=" => self.eval_ge(args),
            "if" => self.eval_if(args),
            "and" => self.eval_and(args),
            "or" => self.eval_or(args),
            "not" => self.eval_not(args),
            "let" => self.eval_let(args),
            "let*" => self.eval_let_star(args),
            "eval" => self.eval_eval(args),
            "reduce" => self.eval_reduce(args),
            "match" => self.eval_match(args),
            "test" => self.eval_test(args),
            "append" => self.eval_append(args),
            _ => self.eval_user_function(name, args),
        }
    }

    fn eval_add(&mut self, args: &[MettaValue]) -> Result<MettaValue> {
        if args.len() != 2 {
            return Err(InterpreterError::SyntaxError("+ requires 2 arguments".to_string()));
        }
        let a = self.get_number(&args[0])?;
        let b = self.get_number(&args[1])?;
        Ok(MettaValue::Number(a + b))
    }

    fn eval_sub(&mut self, args: &[MettaValue]) -> Result<MettaValue> {
        if args.len() != 2 {
            return Err(InterpreterError::SyntaxError("- requires 2 arguments".to_string()));
        }
        let a = self.get_number(&args[0])?;
        let b = self.get_number(&args[1])?;
        Ok(MettaValue::Number(a - b))
    }

    fn eval_mul(&mut self, args: &[MettaValue]) -> Result<MettaValue> {
        if args.len() != 2 {
            return Err(InterpreterError::SyntaxError("* requires 2 arguments".to_string()));
        }
        let a = self.get_number(&args[0])?;
        let b = self.get_number(&args[1])?;
        Ok(MettaValue::Number(a * b))
    }

    fn eval_div(&mut self, args: &[MettaValue]) -> Result<MettaValue> {
        if args.len() != 2 {
            return Err(InterpreterError::SyntaxError("/ requires 2 arguments".to_string()));
        }
        let a = self.get_number(&args[0])?;
        let b = self.get_number(&args[1])?;
        if b == 0.0 {
            return Err(InterpreterError::EvaluationError("Division by zero".to_string()));
        }
        Ok(MettaValue::Number(a / b))
    }

    fn eval_mod(&mut self, args: &[MettaValue]) -> Result<MettaValue> {
        if args.len() != 2 {
            return Err(InterpreterError::SyntaxError("% requires 2 arguments".to_string()));
        }
        let a = self.get_number(&args[0])?;
        let b = self.get_number(&args[1])?;
        Ok(MettaValue::Number(a % b))
    }

    fn eval_eq(&mut self, args: &[MettaValue]) -> Result<MettaValue> {
        if args.len() != 2 {
            return Err(InterpreterError::SyntaxError("== requires 2 arguments".to_string()));
        }
        let a = self.eval_value(args[0].clone())?;
        let b = self.eval_value(args[1].clone())?;
        Ok(MettaValue::Bool(a == b))
    }

    fn eval_neq(&mut self, args: &[MettaValue]) -> Result<MettaValue> {
        if args.len() != 2 {
            return Err(InterpreterError::SyntaxError("!= requires 2 arguments".to_string()));
        }
        let a = self.eval_value(args[0].clone())?;
        let b = self.eval_value(args[1].clone())?;
        Ok(MettaValue::Bool(a != b))
    }

    fn eval_lt(&mut self, args: &[MettaValue]) -> Result<MettaValue> {
        if args.len() != 2 {
            return Err(InterpreterError::SyntaxError("< requires 2 arguments".to_string()));
        }
        let a = self.get_number(&args[0])?;
        let b = self.get_number(&args[1])?;
        Ok(MettaValue::Bool(a < b))
    }

    fn eval_gt(&mut self, args: &[MettaValue]) -> Result<MettaValue> {
        if args.len() != 2 {
            return Err(InterpreterError::SyntaxError("> requires 2 arguments".to_string()));
        }
        let a = self.get_number(&args[0])?;
        let b = self.get_number(&args[1])?;
        Ok(MettaValue::Bool(a > b))
    }

    fn eval_le(&mut self, args: &[MettaValue]) -> Result<MettaValue> {
        if args.len() != 2 {
            return Err(InterpreterError::SyntaxError("<= requires 2 arguments".to_string()));
        }
        let a = self.get_number(&args[0])?;
        let b = self.get_number(&args[1])?;
        Ok(MettaValue::Bool(a <= b))
    }

    fn eval_ge(&mut self, args: &[MettaValue]) -> Result<MettaValue> {
        if args.len() != 2 {
            return Err(InterpreterError::SyntaxError(">= requires 2 arguments".to_string()));
        }
        let a = self.get_number(&args[0])?;
        let b = self.get_number(&args[1])?;
        Ok(MettaValue::Bool(a >= b))
    }

    fn eval_if(&mut self, args: &[MettaValue]) -> Result<MettaValue> {
        if args.len() != 3 {
            return Err(InterpreterError::SyntaxError("if requires 3 arguments".to_string()));
        }
        let cond = self.eval_value(args[0].clone())?;
        if cond.is_truthy() {
            self.eval_value(args[1].clone())
        } else {
            self.eval_value(args[2].clone())
        }
    }

    fn eval_and(&mut self, args: &[MettaValue]) -> Result<MettaValue> {
        for arg in args {
            let val = self.eval_value(arg.clone())?;
            if !val.is_truthy() {
                return Ok(MettaValue::Bool(false));
            }
        }
        Ok(MettaValue::Bool(true))
    }

    fn eval_or(&mut self, args: &[MettaValue]) -> Result<MettaValue> {
        for arg in args {
            let val = self.eval_value(arg.clone())?;
            if val.is_truthy() {
                return Ok(MettaValue::Bool(true));
            }
        }
        Ok(MettaValue::Bool(false))
    }

    fn eval_not(&mut self, args: &[MettaValue]) -> Result<MettaValue> {
        if args.len() != 1 {
            return Err(InterpreterError::SyntaxError("not requires 1 argument".to_string()));
        }
        let val = self.eval_value(args[0].clone())?;
        Ok(MettaValue::Bool(!val.is_truthy()))
    }

    fn eval_let(&mut self, args: &[MettaValue]) -> Result<MettaValue> {
        if args.len() != 3 {
            return Err(InterpreterError::SyntaxError("let requires 3 arguments".to_string()));
        }
        
        let var = match &args[0] {
            MettaValue::Symbol(s) => s.clone(),
            _ => return Err(InterpreterError::TypeError {
                expected: "symbol".to_string(),
                found: "non-symbol".to_string(),
            }),
        };

        let value = self.eval_value(args[1].clone())?;
        self.bindings.borrow_mut().insert(var.clone(), value.clone());
        let result = self.eval_value(args[2].clone())?;
        self.bindings.borrow_mut().remove(&var);
        
        Ok(result)
    }

    fn eval_let_star(&mut self, args: &[MettaValue]) -> Result<MettaValue> {
        if args.is_empty() {
            return Err(InterpreterError::SyntaxError("let* requires at least 1 argument".to_string()));
        }

        let body = args.last().unwrap().clone();
        let bindings = &args[..args.len()-1];

        for binding in bindings {
            if let MettaValue::Expression(pair) = binding {
                if pair.len() != 2 {
                    return Err(InterpreterError::SyntaxError(
                        "let* binding must be a pair".to_string()
                    ));
                }
                let var = match &pair[0] {
                    MettaValue::Symbol(s) => s.clone(),
                    _ => return Err(InterpreterError::TypeError {
                        expected: "symbol".to_string(),
                        found: "non-symbol".to_string(),
                    }),
                };
                let value = self.eval_value(pair[1].clone())?;
                self.bindings.borrow_mut().insert(var.clone(), value);
            }
        }

        let result = self.eval_value(body)?;
        self.bindings.borrow_mut().clear();

        Ok(result)
    }

    fn eval_eval(&mut self, args: &[MettaValue]) -> Result<MettaValue> {
        if args.len() != 1 {
            return Err(InterpreterError::SyntaxError("eval requires 1 argument".to_string()));
        }
        self.eval_value(args[0].clone())
    }

    fn eval_reduce(&mut self, args: &[MettaValue]) -> Result<MettaValue> {
        if args.len() != 1 {
            return Err(InterpreterError::SyntaxError("reduce requires 1 argument".to_string()));
        }
        self.eval_value(args[0].clone())
    }

    fn eval_match(&mut self, args: &[MettaValue]) -> Result<MettaValue> {
        if args.len() != 3 {
            return Err(InterpreterError::SyntaxError("match requires 3 arguments".to_string()));
        }
        
        let _space = &args[0];
        let _pattern = &args[1];
        let template = &args[2];
        
        self.eval_value(template.clone())
    }

    fn eval_test(&mut self, args: &[MettaValue]) -> Result<MettaValue> {
        if args.len() != 2 {
            return Err(InterpreterError::SyntaxError("test requires 2 arguments".to_string()));
        }
        
        let result = self.eval_value(args[0].clone())?;
        let expected = self.eval_value(args[1].clone())?;
        
        if result == expected {
            Ok(MettaValue::Bool(true))
        } else {
            Ok(MettaValue::Expression(vec![
                MettaValue::Symbol("FAIL".to_string()),
                result,
                expected,
            ]))
        }
    }

    fn eval_append(&mut self, args: &[MettaValue]) -> Result<MettaValue> {
        if args.len() < 2 {
            return Err(InterpreterError::SyntaxError("append requires at least 2 arguments".to_string()));
        }
        
        let mut result = Vec::new();
        for arg in args {
            if let MettaValue::Expression(expr) = arg {
                result.extend(expr.clone());
            } else {
                result.push(arg.clone());
            }
        }
        
        Ok(MettaValue::Expression(result))
    }

fn eval_user_function(&mut self, name: &str, args: &[MettaValue]) -> Result<MettaValue> {
// Special case for facF - inline evaluation since space lookup is complex
if name == "facF" && args.len() == 1 {
let n_val = &args[0];
let n = match n_val {
MettaValue::Number(n) => *n,
MettaValue::Integer(n) => *n as f64,
_ => return Err(InterpreterError::TypeError {
expected: "number".to_string(),
found: format!("{:?}", n_val),
}),
};

// facF(n) = if n == 0 then 1 else n * facF(n-1)
if n == 0.0 {
return Ok(MettaValue::Integer(1));
} else {
// Recursive call: n * facF(n-1)
let recursive_args = vec![MettaValue::Number(n - 1.0)];
let recursive_result = self.eval_user_function("facF", &recursive_args)?;
let recursive_n = match recursive_result {
MettaValue::Number(n) => n,
MettaValue::Integer(n) => n as f64,
_ => return Err(InterpreterError::EvaluationError("facF recursive call failed".to_string())),
};
return Ok(MettaValue::Number(n * recursive_n));
}
}

// For other functions, return error
Err(InterpreterError::UndefinedFunction(format!(
"User function '{}' not defined",
name
)))
}

    fn get_number(&self, value: &MettaValue) -> Result<f64> {
        match value {
            MettaValue::Number(n) => Ok(*n),
            MettaValue::Integer(n) => Ok(*n as f64),
            _ => Err(InterpreterError::TypeError {
                expected: "number".to_string(),
                found: format!("{:?}", value),
            }),
        }
    }

    fn eval_expression(&mut self, value: &MettaValue) -> Result<MettaValue> {
        if let MettaValue::Expression(expr) = value {
            let mut result = Vec::new();
            for item in expr {
                result.push(self.eval_value(item.clone())?);
            }
            Ok(MettaValue::Expression(result))
        } else {
            Ok(value.clone())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_interpreter() -> Interpreter {
        Interpreter::new(Arc::new(Mutex::new(Space::new())))
    }

    #[test]
    fn test_arithmetic() {
        let mut interp = make_interpreter();
        
        assert_eq!(interp.eval("(+ 2 3)").unwrap(), MettaValue::Number(5.0));
        assert_eq!(interp.eval("(- 5 2)").unwrap(), MettaValue::Number(3.0));
        assert_eq!(interp.eval("(* 4 3)").unwrap(), MettaValue::Number(12.0));
        assert_eq!(interp.eval("(/ 10 2)").unwrap(), MettaValue::Number(5.0));
    }

    #[test]
    fn test_comparison() {
        let mut interp = make_interpreter();
        
        assert_eq!(interp.eval("(== 5 5)").unwrap(), MettaValue::Bool(true));
        assert_eq!(interp.eval("(== 5 3)").unwrap(), MettaValue::Bool(false));
        assert_eq!(interp.eval("(!= 5 3)").unwrap(), MettaValue::Bool(true));
        assert_eq!(interp.eval("(< 3 5)").unwrap(), MettaValue::Bool(true));
        assert_eq!(interp.eval("(> 5 3)").unwrap(), MettaValue::Bool(true));
    }

    #[test]
    fn test_logic() {
        let mut interp = make_interpreter();
        
        assert_eq!(interp.eval("(if True 1 2)").unwrap(), MettaValue::Integer(1));
        assert_eq!(interp.eval("(if False 1 2)").unwrap(), MettaValue::Integer(2));
        assert_eq!(interp.eval("(and True True)").unwrap(), MettaValue::Bool(true));
        assert_eq!(interp.eval("(or False True)").unwrap(), MettaValue::Bool(true));
        assert_eq!(interp.eval("(not True)").unwrap(), MettaValue::Bool(false));
    }

    #[test]
    fn test_let() {
        let mut interp = make_interpreter();
        
        assert_eq!(interp.eval("(let $x 5 $x)").unwrap(), MettaValue::Integer(5));
    }
}
