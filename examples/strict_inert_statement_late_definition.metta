; A statement whose head is defined by a later import is not inert: the later
; definition recompiles the caller into a call (late_call_recompile.metta), so
; strict mode accepts it once loading is complete.
!(import! &self imports/late_definition/early_statement)
!(import! &self imports/late_definition/late)
!(bind! &effects (new-space))
!(test (early-checked-statement 7) True)
!(test (collapse (match &effects (effect $x) $x)) (7))
