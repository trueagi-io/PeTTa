; Type declarations of the MeTTa builtins as (: name type) atoms, for get-type
; and match. They are generated from the typing column of builtin_spec in
; src/typecheck/builtin_registry.pl, the single source of builtin signatures,
; which the checker reads whether or not this library is imported.
!(import_prolog_function add_builtin_signatures)
!(add_builtin_signatures &self)
