; A type error inside an imported module must abort the importing program:
; previously import! swallowed it, leaving a partially loaded module and a
; successful exit.
!(import! &self fail_import_type_error_module)

!(test (bad 1) 2)
