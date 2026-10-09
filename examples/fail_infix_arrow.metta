; Arrows are prefix like every MeTTa form: (-[det]-> Number Number).
; The abandoned infix form is rejected loudly instead of silently parsing
; as a value/tuple type and dropping the arrow.
(: f (Number -[det]-> Number))
(= (f $x) (+ $x 1))

!(f 1)
