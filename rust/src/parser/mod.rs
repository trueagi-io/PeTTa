//! Native Rust MeTTa S-expression parser using `nom`.
//!
//! **This module is experimental.** It provides a pure-Rust parser for MeTTa
//! S-expressions, independent of the SWI-Prolog backend. It is not yet
//! integrated into the main execution pipeline — the engine currently sends
//! raw strings to Prolog for parsing.
//!
//! To enable this module publicly, use the `pure-rust` feature flag.
//!
//! # Design
//! The parser mirrors the structure of `prolog/parser.pl` but uses `nom`
//! combinators instead of DCG rules. It produces `MettaValue` AST nodes.

use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{char, multispace0},
    combinator::{map, opt, value},
    multi::many0,
    sequence::{delimited, preceded, terminated},
    IResult, Parser,
};

use crate::MettaValue;

/// Parse a MeTTa S-expression string into a `MettaValue`.
///
/// This is the main entry point for the native Rust parser.
/// It handles:
/// - Atoms: `foo`, `true`, `false`
/// - Numbers: `42`, `-3.14`
/// - Strings: `"hello world"`
/// - Variables: `$x`, `$foo`
/// - Lists/Expressions: `(a b c)`, `(+ 1 2)`
pub fn parse_metta(input: &str) -> Result<MettaValue, String> {
    match sexpr(input) {
        Ok((remaining, value)) => {
            if remaining.trim().is_empty() {
                Ok(value)
            } else {
                Err(format!(
                    "Unexpected trailing input: {:?}",
                    &remaining[..remaining.len().min(20)]
                ))
            }
        }
        Err(e) => Err(format!("Parse error: {}", e)),
    }
}

/// Parse an S-expression: atom, number, string, variable, or parenthesized list.
fn sexpr(input: &str) -> IResult<&str, MettaValue> {
    preceded(
        multispace0,
        alt((
            string_literal,
            parenthesized_list,
            variable,
            number,
            boolean_literal,
            atom,
        )),
    )
    .parse(input)
}

/// Parse a parenthesized list/expression: `(a b c)` or `()`
fn parenthesized_list(input: &str) -> IResult<&str, MettaValue> {
    let (rest, items) = delimited(
        terminated(char('('), multispace0),
        many0(terminated(sexpr, multispace0)),
        char(')'),
    )
    .parse(input)?;

    if items.is_empty() {
        Ok((rest, MettaValue::List(vec![])))
    } else {
        // Heuristic: if first item is a single-char operator-like atom (+, -, *, /, <, >, =, etc.)
        // and there are more items, treat as Expression (function application).
        // Otherwise treat as List.
        if items.len() > 1 {
            if let MettaValue::Atom(ref head) = items[0] {
                let is_operator = matches!(head.as_str(),
                    "+" | "-" | "*" | "/" | "%" | "<" | ">" | "=" | "!" | "?" | ":" |
                    "if" | "case" | "let" | "lambda" | "->" | "|->" | "superpose" |
                    "collapse" | "match" | "bind!" | "add-atom" | "remove-atom"
                );
                if is_operator {
                    let head = items[0].clone();
                    let args = items.into_iter().skip(1).collect();
                    return Ok((rest, MettaValue::Expression(
                        if let MettaValue::Atom(h) = head { h } else { unreachable!() },
                        args,
                    )));
                }
            }
        }
        Ok((rest, MettaValue::List(items)))
    }
}

/// Parse a string literal: `"hello"`
fn string_literal(input: &str) -> IResult<&str, MettaValue> {
    delimited(char('"'), parse_string_content, char('"'))
        .map(MettaValue::Atom)
        .parse(input)
}

fn parse_string_content(input: &str) -> IResult<&str, String> {
    let mut result = String::new();
    let mut chars = input.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            if let Some(&next) = chars.peek() {
                match next {
                    'n' => {
                        result.push('\n');
                        chars.next();
                    }
                    't' => {
                        result.push('\t');
                        chars.next();
                    }
                    'r' => {
                        result.push('\r');
                        chars.next();
                    }
                    '"' => {
                        result.push('"');
                        chars.next();
                    }
                    '\\' => {
                        result.push('\\');
                        chars.next();
                    }
                    _ => {
                        result.push(c);
                        result.push(next);
                        chars.next();
                    }
                }
            } else {
                result.push(c);
            }
        } else if c == '"' {
            break;
        } else {
            result.push(c);
        }
    }

    // Calculate how many bytes we consumed
    let mut byte_count = 0;
    for ch in input.chars() {
        if ch == '"' && byte_count > 0 {
            break;
        }
        byte_count += ch.len_utf8();
    }

    let rest = &input[byte_count..];
    Ok((rest, result))
}

/// Parse a MeTTa variable: `$x`, `$foo`
fn variable(input: &str) -> IResult<&str, MettaValue> {
    preceded(
        char('$'),
        map(take_while1(is_token_char), |name: &str| {
            MettaValue::Atom(format!("${}", name))
        }),
    )
    .parse(input)
}

/// Parse a number (integer or float, positive or negative).
fn number(input: &str) -> IResult<&str, MettaValue> {
    let (input, neg) = opt(tag("-")).parse(input)?;
    let (input, num_part) = take_while1(|c: char| c.is_ascii_digit() || c == '.').parse(input)?;

    if num_part.is_empty() {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::TakeWhile1,
        )));
    }

    let full = match neg {
        Some(_) => format!("-{}", num_part),
        None => num_part.to_string(),
    };

    // Try integer first
    if let Ok(i) = full.parse::<i64>() {
        return Ok((input, MettaValue::Integer(i.to_string())));
    }

    // Try float
    if let Ok(f) = full.parse::<f64>() {
        return Ok((input, MettaValue::Float(f)));
    }

    // Not a valid number
    Err(nom::Err::Error(nom::error::Error::new(
        input,
        nom::error::ErrorKind::Verify,
    )))
}

/// Parse boolean literals: `true` or `false`
fn boolean_literal(input: &str) -> IResult<&str, MettaValue> {
    alt((
        value(MettaValue::Bool(true), tag("true")),
        value(MettaValue::Bool(false), tag("false")),
    ))
    .parse(input)
}

/// Parse a MeTTa atom (identifier).
fn atom(input: &str) -> IResult<&str, MettaValue> {
    map(take_while1(is_token_char), |name: &str| match name {
        "True" => MettaValue::Bool(true),
        "False" => MettaValue::Bool(false),
        other => MettaValue::Atom(other.to_string()),
    })
    .parse(input)
}

/// Characters allowed in tokens (atoms, variable names, numbers).
fn is_token_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '-' || c == '+' || c == '*' || c == '/' || c == '.'
}

// ---------------------------------------------------------------------------
// Serialize MeTTaValue back to string (inverse of parse)
// ---------------------------------------------------------------------------

/// Serialize a `MettaValue` back to a MeTTa S-expression string.
pub fn serialize_metta(value: &MettaValue) -> String {
    match value {
        MettaValue::Integer(n) => n.clone(),
        MettaValue::Float(f) => {
            let s = f.to_string();
            if s.contains('.') {
                s.trim_end_matches('0').trim_end_matches('.').to_string()
            } else {
                s
            }
        }
        MettaValue::Bool(true) => "true".to_string(),
        MettaValue::Bool(false) => "false".to_string(),
        MettaValue::Atom(s) => s.clone(),
        MettaValue::List(items) => {
            if items.is_empty() {
                "()".to_string()
            } else {
                let inner: Vec<String> = items.iter().map(serialize_metta).collect();
                format!("({})", inner.join(" "))
            }
        }
        MettaValue::Expression(head, args) => {
            let mut parts = vec![head.clone()];
            parts.extend(args.iter().map(serialize_metta));
            format!("({})", parts.join(" "))
        }
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_atom() {
        let v = parse_metta("foo").unwrap();
        assert_eq!(v, MettaValue::Atom("foo".into()));
    }

    #[test]
    fn test_parse_number_int() {
        let v = parse_metta("42").unwrap();
        assert_eq!(v, MettaValue::Integer("42".into()));
    }

    #[test]
    fn test_parse_number_neg_int() {
        let v = parse_metta("-42").unwrap();
        assert_eq!(v, MettaValue::Integer("-42".into()));
    }

    #[test]
    fn test_parse_number_float() {
        let v = parse_metta("3.14").unwrap();
        assert!(matches!(v, MettaValue::Float(f) if (f - 3.14).abs() < 1e-10));
    }

    #[test]
    fn test_parse_string_literal() {
        let v = parse_metta(r#""hello world""#).unwrap();
        assert_eq!(v, MettaValue::Atom("hello world".into()));
    }

    #[test]
    fn test_parse_variable() {
        let v = parse_metta("$x").unwrap();
        assert_eq!(v, MettaValue::Atom("$x".into()));
    }

    #[test]
    fn test_parse_empty_list() {
        let v = parse_metta("()").unwrap();
        assert_eq!(v, MettaValue::List(vec![]));
    }

    #[test]
    fn test_parse_list() {
        let v = parse_metta("(a b c)").unwrap();
        match v {
            MettaValue::List(items) => {
                assert_eq!(items.len(), 3);
                assert_eq!(items[0], MettaValue::Atom("a".into()));
                assert_eq!(items[1], MettaValue::Atom("b".into()));
                assert_eq!(items[2], MettaValue::Atom("c".into()));
            }
            _ => panic!("Expected List"),
        }
    }

    #[test]
    fn test_parse_expression() {
        let v = parse_metta("(+ 1 2)").unwrap();
        match v {
            MettaValue::Expression(ref head, ref args) => {
                assert_eq!(head, "+");
                assert_eq!(args.len(), 2);
                assert_eq!(args[0], MettaValue::Integer("1".into()));
                assert_eq!(args[1], MettaValue::Integer("2".into()));
            }
            other => panic!("Expected Expression, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_nested() {
        let v = parse_metta("((a b) c)").unwrap();
        match v {
            MettaValue::List(items) => {
                assert_eq!(items.len(), 2);
                assert!(matches!(&items[0], MettaValue::List(l) if l.len() == 2));
                assert_eq!(items[1], MettaValue::Atom("c".into()));
            }
            _ => panic!("Expected List"),
        }
    }

    #[test]
    fn test_parse_bool_true() {
        let v = parse_metta("true").unwrap();
        assert_eq!(v, MettaValue::Bool(true));
    }

    #[test]
    fn test_parse_bool_false() {
        let v = parse_metta("false").unwrap();
        assert_eq!(v, MettaValue::Bool(false));
    }

    #[test]
    fn test_parse_true_false_mapping() {
        let v1 = parse_metta("True").unwrap();
        assert_eq!(v1, MettaValue::Bool(true));
        let v2 = parse_metta("False").unwrap();
        assert_eq!(v2, MettaValue::Bool(false));
    }

    #[test]
    fn test_parse_with_whitespace() {
        let v = parse_metta("  ( +  1  2 )  ").unwrap();
        match v {
            MettaValue::List(items) => {
                // + is an operator, so this should be Expression wrapped in List or just Expression
                assert!(!items.is_empty());
            }
            MettaValue::Expression(ref head, _) => assert_eq!(head, "+"),
            _ => panic!("Expected Expression or List"),
        }
    }

    #[test]
    fn test_serialize_roundtrip() {
        let input = "(+ 1 2)";
        let v = parse_metta(input).unwrap();
        let serialized = serialize_metta(&v);
        let v2 = parse_metta(&serialized).unwrap();
        assert_eq!(v, v2);
    }

    #[test]
    fn test_serialize_empty_list() {
        let v = MettaValue::List(vec![]);
        assert_eq!(serialize_metta(&v), "()");
    }

    #[test]
    fn test_parse_error() {
        let result = parse_metta("(");
        assert!(result.is_err());
    }
}
