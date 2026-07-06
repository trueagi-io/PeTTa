; Imported signatures must be available before local functions compile.
(= (uses-imported $x) (imported-inc $x))

!(import! &self imports/import_signature_preload_dep)

!(test (uses-imported 41) 42)
