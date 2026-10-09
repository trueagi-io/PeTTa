; PeTTa's own bundled libraries must be loadable by a --strict program;
; lib_import declares its two nondet importers. From PeTTaChainer's
; typechecker_repros/strict_lib_import_missing_types.metta.
;; PeTTa's own lib_import.metta must be loadable by a strict program.
;; At 45b1b81 this fails before the caller can use static-import! because
;; import_prolog_functions_from_file/2 has no declared or inferable type.
!(import! &self (library lib_import))

!(test (== 1 1) true)
