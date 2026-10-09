; The structural equality/unification builtins (=, =?, =alpha, =@=) take their
; operands as DATA PATTERNS. A var-headed operand like ($name $index $vars) is a
; pattern that is built and unified, never dispatched - reduce/2 leaves a
; var-headed term unevaluated - so it contributes no dynamic-head uncertainty and
; the whole test stays det. deterministic_expr reads = operands with
; unify_operand_determinism/2 rather than the generic call path.

(: Context (Newtype Expression))

(: has-context-shape (-[det]-> Context Bool))
(= (has-context-shape $value)
   (if (= $value ($name $index $vars))
      true
      false))

!(test
   (has-context-shape
      (brand Context (main 0 ())))
   true)

; A FUN-headed operand is still evaluated first and still contributes its own
; determinism, so a nondet call in operand position is NOT hidden by this
; reading (see fail_det_unify_nondet_operand.metta). Only the variable-head
; pattern case - the honest dynamic_head verdict was too weak for - is relaxed.
