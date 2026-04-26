use crate::engine::errors::{BackendErrorKind, parse_backend_error};

#[test]
fn parse_structured_undefined_function() {
    let json = r#"{"kind":"swipl","message":"ERROR","raw":"ERROR","formal":"existence_error(procedure, (/ foo 2))","context":""}"#;
    let kind = parse_backend_error(json);
    match kind {
        BackendErrorKind::UndefinedFunction { name, arity, .. } => {
            assert_eq!(arity, 2);
            assert!(name.contains("foo") || name.contains("/"));
        }
        _ => panic!("Expected UndefinedFunction, got: {:?}", kind),
    }
}

#[test]
fn parse_structured_generic() {
    let json = r#"{"kind":"swipl","message":"Something broke","raw":"Some raw text"}"#;
    let kind = parse_backend_error(json);
    match kind {
        BackendErrorKind::Generic(msg) => {
            assert!(msg.contains("Something broke") || msg.contains("Some raw text"))
        }
        _ => panic!("Expected Generic, got: {:?}", kind),
    }
}
