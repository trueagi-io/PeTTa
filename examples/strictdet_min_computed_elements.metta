; A fun-headed compound is still a DATA head when the function's unique
; declared output type is a non-function type: the head of
; ((identity-number $a) ...) evaluates to a Number, and a Number cannot
; dispatch, so the spine is manifest and min-atom over it is det. A call
; returning a closure stays an application and stays excluded
; (fail_det_compound_head_apply.metta). From PeTTaChainer's
; typechecker_repros/det_min_computed_nonempty.metta.
(: identity-number (-[det]-> Number Number))
(= (identity-number $value) $value)

(: min-computed-three (-[det]-> Number Number Number Number))
(= (min-computed-three $a $b $c)
   (min-atom
      ((identity-number $a)
       (identity-number $b)
       (identity-number $c))))

!(test (min-computed-three 3 1 2) 1)
