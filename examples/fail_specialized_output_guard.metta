; Same program as fail_strict_closure_output_promise.metta, compiled WITHOUT
; --strict: the general clause of g keeps the runtime guard the certification
; could not discharge, so the type error is reported as a type error.
;
; The specializer is what made that guarantee conditional. A specialized copy
; (g_Spec_[h], with $f fixed to h) skipped the output certification on the
; grounds that it is an instance of an already-validated clause - so it also
; skipped the GUARD that validation produced, and the call site, which uses the
; specialized copy, ran unchecked into SWI's arithmetic.
;
; The certification now runs for specialized copies too. Being an instance, it
; can only make the result type more specific: here it sees String against the
; declared Number, and a typecheck error during specialization means "do not
; specialize" - the call falls back to the general, guarded clause.
(: g (-> (-> Number $b) Number))
(= (g $f) ($f 1))

(: h (-> Number String))
(= (h $x) "str")

(: consume (-> Number Number))
(= (consume $n) (+ $n 1))

!(consume (g h))
