; A user function every one of whose clauses provably results in a bound
; Bool carries a bound_bool output certificate, and its CALL is then a
; manifest boolean operand for and/or/not - the certificate, not the Bool
; declaration, is what qualifies. From PeTTaChainer's
; typechecker_repros/det_bool_call_operand.metta.
; A det function whose implementation always produces a bound Bool should be
; usable as an operand of and/or/not.  The checker recognizes bound Bool
; parameters and det builtin Bool results, but does not propagate the same
; guarantee through a user function call.
;
; Fails on PeTTa typecheck-v2 19c4671 in both modes:
;   run.sh det_bool_call_operand.metta --strict
;   run.sh det_bool_call_operand.metta --strict --strict-det
;   Determinism check failed for 'both-empty': call(and)

(: is-empty (-[det]-> (List Number) Bool))
(= (is-empty $values)
   (== $values ()))

(: both-empty (-[det]-> (List Number) (List Number) Bool))
(= (both-empty $left $right)
   (and (is-empty $left)
        (is-empty $right)))

!(test (both-empty () ()) true)
!(test (both-empty () (1)) false)
