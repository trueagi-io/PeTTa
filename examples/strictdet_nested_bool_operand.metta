; manifest_bool asks the CALL-SITE determinism verdict, not the flat
; table's worst case: (not (is-empty $r)) is det because its operand is a
; certified bound-Bool call, and a det sole-Bool-output call is itself a
; bound boolean - so nesting composes. From PeTTaChainer's
; typechecker_repros/det_nested_bool_operand.metta.
; Bound-Bool output certification now lets a det user-function call feed a
; boolean builtin directly.  The guarantee is not propagated recursively when
; that result first passes through another contextual boolean builtin.
;
; Fails on PeTTa typecheck-v2 e513a09 in both modes:
;   run.sh det_nested_bool_operand.metta --strict
;   run.sh det_nested_bool_operand.metta --strict --strict-det
;   Determinism check failed for 'left-only-empty': call(and)

(: is-empty (-[det]-> (List Number) Bool))
(= (is-empty $values)
   (== $values ()))

(: left-only-empty (-[det]-> (List Number) (List Number) Bool))
(= (left-only-empty $left $right)
   (and (is-empty $left)
        (not (is-empty $right))))

!(test (left-only-empty () (1)) true)
!(test (left-only-empty () ()) false)
