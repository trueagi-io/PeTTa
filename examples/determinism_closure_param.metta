; An explicit -[det]-> closure parameter carries its commitment in every
; mode, not only under --strict-det: applying it inside a det body is det
; evidence, and det markers survive through partial application.
(: apply-det (-[det]-> (-[det]-> Number Number) Number Number))
(= (apply-det $f $x) ($f $x))

(: inc (-[det]-> Number Number))
(= (inc $x) (+ $x 1))

(: add (-[det]-> Number Number Number))
(= (add $a $b) (+ $a $b))

!(test (apply-det inc 1) 2)
!(test (apply-det (add 10) 1) 11)
