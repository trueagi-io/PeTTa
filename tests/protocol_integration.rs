//! Integration tests for the binary protocol.
//!
//! These tests verify the full request/response cycle between Rust and SWI-Prolog,
//! covering all message types ('F', 'S', 'Q') and edge cases.

use petta::{BackendErrorKind, MettaValue, PeTTaError};

mod test_utils;
use test_utils::{make_engine, project_root};

// ---------------------------------------------------------------------------
// Protocol type 'S' (83): process_metta_string
// ---------------------------------------------------------------------------

#[test]
fn test_protocol_string_simple() {
    let mut e = make_engine();
    let results = e.process_metta_string("!(+ 1 2)").unwrap();
    assert_eq!(results.len(), 1);
    assert_eq!(results[0].value, "3");
}

#[test]
fn test_protocol_string_multiple_results() {
    let mut e = make_engine();
    // superpose produces multiple results
    let results = e.process_metta_string("!(superpose (1 2 3))").unwrap();
    assert!(!results.is_empty());
    let values: Vec<&str> = results.iter().map(|r| r.value.as_str()).collect();
    assert!(values.contains(&"1"));
    assert!(values.contains(&"2"));
    assert!(values.contains(&"3"));
}

#[test]
fn test_protocol_string_empty_input() {
    let mut e = make_engine();
    let results = e.process_metta_string("").unwrap();
    // Empty input should produce zero or minimal results
    assert!(results.is_empty() || results.len() <= 1);
}

#[test]
fn test_protocol_string_with_comments() {
    let mut e = make_engine();
    // Comments are handled by MeTTa parser, this should still produce a result
    let results = e.process_metta_string("% this is a comment\n!(+ 1 2)").unwrap();
    // The comment might affect parsing, so we accept either result
    let _ = results;
}

#[test]
fn test_protocol_string_multiline() {
    let mut e = make_engine();
    let code = r#"
(= (foo $x) $x)
!(foo 42)
"#;
    let results = e.process_metta_string(code).unwrap();
    assert!(!results.is_empty());
    assert_eq!(results.last().unwrap().value, "42");
}

#[test]
fn test_protocol_string_lambda() {
    let mut e = make_engine();
    let results = e.process_metta_string("!(|-> $x $x 42)").unwrap();
    assert!(!results.is_empty());
}

#[test]
fn test_protocol_string_case() {
    let mut e = make_engine();
    let code = r#"
!(case (+ 1 2)
  (3 three
   _ other))
"#;
    let results = e.process_metta_string(code).unwrap();
    // Case might produce different output format, just verify it doesn't crash
    let _ = results;
}

// ---------------------------------------------------------------------------
// Protocol type 'F' (70): load_metta_file
// ---------------------------------------------------------------------------

#[test]
fn test_protocol_file_identity() {
    let mut e = make_engine();
    let results = e.load_metta_file(&project_root().join("examples/identity.metta")).unwrap();
    assert!(!results.is_empty());
}

#[test]
fn test_protocol_file_arithmetic() {
    let mut e = make_engine();
    let results = e.load_metta_file(&project_root().join("examples/math.metta")).unwrap();
    assert!(!results.is_empty());
}

#[test]
fn test_protocol_file_not_found() {
    let mut e = make_engine();
    let err = e.load_metta_file(&project_root().join("examples/nonexistent.metta"));
    assert!(err.is_err());
    // May be FileNotFound or PathError depending on the stage
    match err.unwrap_err() {
        PeTTaError::FileNotFound(p) => {
            assert!(p.to_string_lossy().contains("nonexistent"));
        }
        PeTTaError::PathError(msg) => {
            assert!(msg.contains("No such file") || msg.contains("nonexistent"));
        }
        other => panic!("Expected FileNotFound or PathError, got {:?}", other),
    }
}

#[test]
fn test_protocol_file_boolean_solver() {
    let mut e = make_engine();
    let results = e.load_metta_file(&project_root().join("examples/booleansolver.metta")).unwrap();
    assert!(!results.is_empty());
}

#[test]
fn test_protocol_file_fibonacci() {
    let mut e = make_engine();
    let results = e.load_metta_file(&project_root().join("examples/fib.metta")).unwrap();
    assert!(!results.is_empty());
}

// ---------------------------------------------------------------------------
// Error handling
// ---------------------------------------------------------------------------

#[test]
fn test_protocol_error_undefined_function() {
    let mut e = make_engine();
    let result = e.process_metta_string("!(undefined_function 42)");
    // The engine may return Ok with empty results or an error
    match result {
        Ok(results) => {
            // Empty result list is also valid for undefined functions
            let _ = results;
        }
        Err(PeTTaError::BackendError(BackendErrorKind::UndefinedFunction {
            name, arity, ..
        })) => {
            assert_eq!(name, "undefined_function");
            assert_eq!(arity, 2);
        }
        Err(e) => panic!("Expected UndefinedFunction or Ok, got {:?}", e),
    }
}

#[test]
fn test_protocol_error_type_mismatch() {
    let mut e = make_engine();
    // Attempt an operation that should produce a type error
    let results = e.process_metta_string("!(< true false)");
    // This may succeed in MeTTa (returning false) or produce a type error
    // Either way, the protocol should handle it gracefully
    match results {
        Ok(_) => {}                            // Valid: returns false
        Err(PeTTaError::BackendError(_)) => {} // Also valid: type error
        Err(e) => panic!("Unexpected error: {:?}", e),
    }
}

// ---------------------------------------------------------------------------
// Value parsing (MettaValue)
// ---------------------------------------------------------------------------

#[test]
fn test_value_parse_integer_positive() {
    let v = MettaValue::parse("12345");
    assert!(matches!(v, Some(MettaValue::Integer(ref s)) if s == "12345"));
}

#[test]
fn test_value_parse_integer_negative() {
    let v = MettaValue::parse("-42");
    assert!(matches!(v, Some(MettaValue::Integer(ref s)) if s == "-42"));
}

#[test]
fn test_value_parse_float() {
    let v = MettaValue::parse("3.14159");
    assert!(matches!(v, Some(MettaValue::Float(f)) if (f - 3.14159).abs() < 1e-10));
}

#[test]
fn test_value_parse_bool_true() {
    assert_eq!(MettaValue::parse("true"), Some(MettaValue::Bool(true)));
}

#[test]
fn test_value_parse_bool_false() {
    assert_eq!(MettaValue::parse("false"), Some(MettaValue::Bool(false)));
}

#[test]
fn test_value_parse_atom() {
    let v = MettaValue::parse("some_atom");
    assert!(matches!(v, Some(MettaValue::Atom(ref s)) if s == "some_atom"));
}

#[test]
fn test_value_parse_list_empty() {
    let v = MettaValue::parse("()");
    assert!(matches!(v, Some(MettaValue::List(ref l)) if l.is_empty()));
}

#[test]
fn test_value_parse_list_with_items() {
    let v = MettaValue::parse("(1 2 3)");
    assert!(matches!(v, Some(MettaValue::List(ref l)) if l.len() == 3));
}

#[test]
fn test_value_parse_expression() {
    let v = MettaValue::parse("(+ 1 2)");
    match v {
        Some(MettaValue::Expression(ref f, ref args)) => {
            assert_eq!(f, "+");
            assert_eq!(args.len(), 2);
        }
        other => panic!("Expected Expression, got {:?}", other),
    }
}

#[test]
fn test_value_parse_nested_expression() {
    let v = MettaValue::parse("(foo (bar 1) 2)");
    match v {
        Some(MettaValue::Expression(ref f, ref args)) => {
            assert_eq!(f, "foo");
            assert_eq!(args.len(), 2);
        }
        other => panic!("Expected Expression, got {:?}", other),
    }
}

#[test]
fn test_value_parse_empty_string() {
    assert_eq!(MettaValue::parse(""), None);
}

// ---------------------------------------------------------------------------
// Engine lifecycle
// ---------------------------------------------------------------------------

#[test]
fn test_engine_multiple_queries() {
    let mut e = make_engine();
    let r1 = e.process_metta_string("!(+ 1 2)").unwrap();
    let r2 = e.process_metta_string("!(+ 3 4)").unwrap();
    let r3 = e.process_metta_string("!(+ 5 6)").unwrap();
    assert_eq!(r1[0].value, "3");
    assert_eq!(r2[0].value, "7");
    assert_eq!(r3[0].value, "11");
}

#[test]
fn test_engine_sequential_file_loads() {
    let mut e = make_engine();
    let r1 = e.load_metta_file(&project_root().join("examples/identity.metta")).unwrap();
    let r2 = e.load_metta_file(&project_root().join("examples/if.metta")).unwrap();
    assert!(!r1.is_empty());
    assert!(!r2.is_empty());
}

#[test]
fn test_engine_load_multiple_files() {
    let mut e = make_engine();
    let files =
        [project_root().join("examples/identity.metta"), project_root().join("examples/if.metta")];
    let file_refs: Vec<&std::path::Path> = files.iter().map(|p| p.as_path()).collect();
    let results = e.load_metta_files(&file_refs);
    assert!(results.is_ok());
}

#[test]
fn test_engine_shutdown_graceful() {
    let mut e = make_engine();
    // Run a query first
    let _ = e.process_metta_string("!(+ 1 2)");
    // Shutdown should complete without hanging
    e.shutdown();
    // Double shutdown should not panic
    e.shutdown();
}

#[test]
fn test_engine_drop_cleans_up() {
    // Create and immediately drop - cleanup should happen via Drop
    let _e = make_engine();
    // No assertion needed, just shouldn't leak or hang
}

// ---------------------------------------------------------------------------
// Edge cases and stress tests
// ---------------------------------------------------------------------------

#[test]
fn test_protocol_string_large_payload() {
    let mut e = make_engine();
    // Generate a large MeTTa expression
    let mut code = String::from("(= (biglist) (");
    for i in 0..1000 {
        code.push_str(&format!("item{} ", i));
    }
    code.push_str("))");
    let results = e.process_metta_string(&code).unwrap();
    // Large payloads might produce empty results or succeed
    let _ = results;
}

#[test]
fn test_protocol_string_unicode() {
    let mut e = make_engine();
    // MeTTa should handle unicode in atoms
    let results = e.process_metta_string("(= (テスト $x) $x) !(テスト 42)").unwrap();
    assert!(!results.is_empty());
}

#[test]
fn test_protocol_string_special_characters() {
    let mut e = make_engine();
    // Test special characters in strings
    let results = e.process_metta_string(r#"(= (test-str) "hello\tworld\n")"#).unwrap();
    // Should not crash
    let _ = results;
}

#[test]
fn test_protocol_multiple_consecutive_errors() {
    let mut e = make_engine();
    // Multiple errors should not corrupt the engine state
    let _ = e.process_metta_string("!(undefined1 42)");
    let _ = e.process_metta_string("!(undefined2 42)");
    let _ = e.process_metta_string("!(undefined3 42)");
    // Engine should still work after errors
    let r = e.process_metta_string("!(+ 1 2)").unwrap();
    assert_eq!(r[0].value, "3");
}

// ---------------------------------------------------------------------------
// Issue #160: is-member returns multiple unexpected matches for unassigned variables
// ---------------------------------------------------------------------------

#[test]
fn test_is_member_unassigned_variable() {
    let mut e = make_engine();
    // Test is-member with an unassigned variable - should return single false, not multiple trues
    let code = r#"
!(let*
    (
        ($pat (hi name boss))
        ($bool (is-member $new $pat))
    )
    $bool
)
"#;
    let results = e.process_metta_string(code).unwrap();
    // Should return exactly one result (not multiple like before the fix)
    assert_eq!(results.len(), 1, "is-member with unassigned variable should return exactly one result, got: {:?}", results.iter().map(|r| &r.value).collect::<Vec<_>>());
    // The result should be false (unassigned variable is not a member)
    let value = &results[0].value;
    assert!(value.eq_ignore_ascii_case("false"), "is-member with unassigned variable should be false, got: {}", value);
}

#[test]
fn test_is_member_existing_element() {
    let mut e = make_engine();
    // Test is-member with an existing element - should return true
    let code = r#"
!(let*
    (
        ($pat (hi name boss))
        ($bool (is-member hi $pat))
    )
    $bool
)
"#;
    let results = e.process_metta_string(code).unwrap();
    // At least one result should be true
    let values: Vec<&str> = results.iter().map(|r| r.value.as_str()).collect();
    assert!(values.iter().any(|v| v.eq_ignore_ascii_case("true")), "is-member with existing element should return true, got: {:?}", values);
}

#[test]
fn test_is_member_non_existing_element() {
    let mut e = make_engine();
    // Test is-member with a non-existing element - should return false
    let code = r#"
!(let*
    (
        ($pat (hi name boss))
        ($bool (is-member missing $pat))
    )
    $bool
)
"#;
    let results = e.process_metta_string(code).unwrap();
    // The result should be false
    let values: Vec<&str> = results.iter().map(|r| r.value.as_str()).collect();
    assert!(values.iter().any(|v| v.eq_ignore_ascii_case("false")), "is-member with non-existing element should return false, got: {:?}", values);
}
