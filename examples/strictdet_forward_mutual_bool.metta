; The clause-BODY prepass mirrors the declaration prepass: every definition
; in a file is visible to the certificate prover while any of them is
; validated, so mutually recursive bound-Bool functions certify in source
; order - the first body may reference the second before it compiles. From
; PeTTaChainer's typechecker_repros/det_forward_mutual_bool_operand.metta.
; Demand-driven coinductive certification works once every mutually recursive
; body has been registered.  Validation of the first body still happens before
; the later body's clause metadata exists, so consuming the partner's bound-Bool
; certificate inside that first body fails.
;
; Fails on PeTTa typecheck-v2 3a2ad2c in both modes:
;   run.sh det_forward_mutual_bool_operand.metta --strict
;   run.sh det_forward_mutual_bool_operand.metta --strict --strict-det
;   Determinism check failed for 'left-even?': call(and)

(: left-even? (-[det]-> Number Bool))
(: right-even? (-[det]-> Number Bool))

(= (left-even? $number)
   (if (<= $number 0)
      true
      (and true
           (right-even? (- $number 1)))))

(= (right-even? $number)
   (if (<= $number 0)
      true
      (and true
           (left-even? (- $number 1)))))

!(test (left-even? 4) true)
