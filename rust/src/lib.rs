#![cfg_attr(all(test, feature = "mork"), allow(implicit_autoref))]
#![cfg_attr(feature = "mork", allow(internal_features))]
#![cfg_attr(feature = "mork", feature(core_intrinsics))]
#![cfg_attr(feature = "mork", feature(portable_simd))]
#![cfg_attr(feature = "mork", feature(allocator_api))]
#![cfg_attr(feature = "mork", feature(coroutine_trait))]
#![cfg_attr(feature = "mork", feature(coroutines))]
#![cfg_attr(feature = "mork", feature(stmt_expr_attributes))]
#![cfg_attr(feature = "mork", feature(gen_blocks))]
#![cfg_attr(feature = "mork", feature(yield_expr))]

pub mod parser;

#[cfg(feature = "profiling")]
pub mod profiler;

#[cfg(feature = "mork")]
pub mod mork;
#[cfg(not(feature = "mork"))]
mod mork;

pub mod pathmap;
pub mod gxhash;
pub mod utils;

pub mod engine;
pub use engine::{
    Backend, BackendErrorKind, EngineConfig, MettaResult, MettaValue, PeTTaEngine, PeTTaError,
    MIN_SWIPL_VERSION, swipl_available,
};

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::{Path, PathBuf};

    fn project_root() -> PathBuf {
        Path::new(env!("CARGO_MANIFEST_DIR")).to_path_buf()
    }
    fn make_engine() -> PeTTaEngine {
        PeTTaEngine::new(&project_root(), false).expect("Failed to create engine")
    }

    #[test]
    fn test_engine_creation() {
        let r = PeTTaEngine::new(&project_root(), false);
        if let Err(ref e) = r {
            eprintln!("Engine creation failed: {}", e);
        }
        assert!(r.is_ok());
    }

    #[test]
    fn test_booleansolver() {
        let mut e = make_engine();
        let r = e.load_metta_file(&project_root().join("examples/booleansolver.metta"));
        let stderr = e.stderr_output();
        if !stderr.is_empty() {
            eprintln!("STDERR: {}", stderr);
        }
        match &r {
            Ok(results) => {
                for res in results {
                    eprintln!("RESULT: {}", res.value);
                }
                assert!(!results.is_empty());
            }
            Err(e) => {
                eprintln!("ERROR: {:?}", e);
                panic!("booleansolver failed");
            }
        }
    }

    #[test]
    fn test_identity() {
        let mut e = make_engine();
        let r = e.process_metta_string("(= (myid $x) $x) !(myid 42)").unwrap();
        assert!(!r.is_empty());
        assert_eq!(r[0].value, "42");
    }

    #[test]
    fn test_arithmetic() {
        let mut e = make_engine();
        let r = e.process_metta_string("!(+ 1 2)").unwrap();
        assert!(!r.is_empty());
        assert_eq!(r[0].value, "3");
    }

    #[test]
    fn test_load_identity_file() {
        let mut e = make_engine();
        let r = e.load_metta_file(&project_root().join("examples/identity.metta")).unwrap();
        assert!(!r.is_empty());
    }

    #[test]
    fn test_boolean() {
        let mut e = make_engine();
        let r = e.process_metta_string("!(and true false)").unwrap();
        assert!(!r.is_empty());
        assert_eq!(r[0].value, "false");
    }

    #[test]
    fn test_comparison() {
        let mut e = make_engine();
        let r = e.process_metta_string("!(< 1 2)").unwrap();
        assert!(!r.is_empty());
        assert_eq!(r[0].value, "true");
    }

    #[test]
    fn test_fibonacci() {
        let mut e = make_engine();
        let r = e.load_metta_file(&project_root().join("examples/fib.metta")).unwrap();
        assert!(!r.is_empty());
    }

    #[test]
    fn test_state() {
        let mut e = make_engine();
        let r = e.load_metta_file(&project_root().join("examples/state.metta")).unwrap();
        assert!(!r.is_empty());
    }

    #[test]
    fn test_if() {
        let mut e = make_engine();
        let r = e.load_metta_file(&project_root().join("examples/if.metta")).unwrap();
        assert!(!r.is_empty());
    }

    #[test]
    fn test_math() {
        let mut e = make_engine();
        let r = e.load_metta_file(&project_root().join("examples/math.metta")).unwrap();
        assert!(!r.is_empty());
    }

    #[test]
    fn test_variable_renaming() {
        let mut e = make_engine();
        let r = e.process_metta_string("(= (fun ($a x)) ($b x)) !(fun (a x))").unwrap();
        assert!(!r.is_empty());
        let v = &r[0].value;
        assert!(v.contains("$") && v.contains('x'), "Expected variable pattern, got: {}", v);
    }

    #[test]
    fn test_file_imports() {
        let mut e = make_engine();
        let root = project_root();
        let id = std::fs::read_to_string(root.join("examples/identity.metta")).unwrap();
        let r = e.process_metta_string(&format!("{}\n!(f 5)", id)).unwrap();
        assert!(r.iter().any(|x| x.value == "25"), "Expected '25': {:?}", r);
    }

    #[test]
    fn test_verbose_mode() {
        let root = project_root();
        let mut e = PeTTaEngine::new(&root, true).unwrap();
        let r = e.process_metta_string("!(+ 1 2)").unwrap();
        assert!(!r.is_empty());
        assert_eq!(r[0].value, "3");
    }

    #[test]
    fn test_parse_int() {
        assert_eq!(MettaValue::parse("42"), Some(MettaValue::Integer("42".into())));
    }

    #[test]
    fn test_parse_bool() {
        assert_eq!(MettaValue::parse("true"), Some(MettaValue::Bool(true)));
        assert_eq!(MettaValue::parse("false"), Some(MettaValue::Bool(false)));
    }

    #[test]
    fn test_parse_float() {
        assert_eq!(MettaValue::parse("3.14"), Some(MettaValue::Float(3.14)));
    }

    #[test]
    fn test_parse_atom() {
        assert_eq!(MettaValue::parse("hello"), Some(MettaValue::Atom("hello".into())));
    }

    #[test]
    fn test_parse_list() {
        let v = MettaValue::parse("(1 2 3)");
        assert!(matches!(v, Some(MettaValue::List(ref items)) if items.len() == 3));
    }

    #[test]
    fn test_parse_expression() {
        let v = MettaValue::parse("(+ 1 2)");
        assert!(
            matches!(v, Some(MettaValue::Expression(ref f, ref args)) if f == "+" && args.len() == 2)
        );
    }

    #[test]
    fn test_parse_undefined_function_error() {
        assert!(matches!(
            engine::errors::parse_backend_error("ERROR: existence_error(procedure, (/ foo 2))"),
            BackendErrorKind::UndefinedFunction { name: _, arity: 2, .. }
        ));
    }

    #[test]
    fn test_parse_stack_overflow() {
        assert!(matches!(
            engine::errors::parse_backend_error("ERROR: Stack depth: 5000000"),
            BackendErrorKind::StackOverflow { .. }
        ));
    }

    #[test]
    fn test_result_parsed_value() {
        let r = MettaResult { value: "42".into() };
        assert_eq!(r.parsed_value(), Some(MettaValue::Integer("42".into())));
    }

    #[test]
    fn test_engine_config_builder() {
        let config = EngineConfig::new(&project_root()).verbose(true).max_restarts(3);
        assert!(config.verbose);
        assert_eq!(config.max_restarts, 3);
    }

    #[test]
    fn test_engine_with_config() {
        let config = EngineConfig::new(&project_root()).verbose(false).max_restarts(1);
        let engine = PeTTaEngine::with_config(&config);
        assert!(engine.is_ok());
    }

    #[test]
    fn test_is_alive() {
        let mut e = make_engine();
        assert!(e.is_alive(), "Engine should be alive after creation");
        let r = e.process_metta_string("!(+ 1 2)").unwrap();
        assert!(e.is_alive(), "Engine should be alive after query");
        assert_eq!(r[0].value, "3");
    }

    #[test]
    fn test_error_display() {
        let e = PeTTaError::FileNotFound(PathBuf::from("/nonexistent"));
        assert!(e.to_string().contains("/nonexistent"));
        let e = PeTTaError::ProtocolError("test error".into());
        assert!(e.to_string().contains("test error"));
        let e = BackendErrorKind::UndefinedFunction {
            name: "foo".into(),
            arity: 2,
            suggestion: Some("bar".into()),
        };
        let s = e.to_string();
        assert!(s.contains("foo/2"));
        assert!(s.contains("bar"));
    }

    #[test]
    fn test_config_defaults() {
        let config = EngineConfig::default();
        assert_eq!(config.swipl_path, PathBuf::from("swipl"));
        assert!(!config.verbose);
        assert!(config.query_timeout.is_none());
        assert_eq!(config.max_restarts, 0);
        assert_eq!(config.min_swipl_version, (9, 3));
    }

    #[test]
    fn test_swipl_available() {
        assert!(swipl_available(Path::new("swipl")));
    }
}