; The fully annotated equivalent is valid: both the function and its
; higher-order parameter state their cardinality.
(: increment (-[det]-> Number Number))
(= (increment $x) (+ $x 1))

(: apply-explicit (-[det]-> (-[det]-> Number Number) Number Number))
(= (apply-explicit $f $x) ($f $x))

!(test (apply-explicit increment 4) 5)
