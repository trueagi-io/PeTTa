; A SYNTAX error in an imported file - unbalanced parentheses - is a compile
; error of the importing program, not a recoverable import problem: only a
; genuinely missing file fails quietly. Without this, the import silently
; loaded nothing and (imported-value) evaluated to itself as data, exit 0.
; From PeTTaChainer's malformed_import_missing_paren repro pair.
!(import! &self soundness/import_syntax_broken)
!(imported-value)
