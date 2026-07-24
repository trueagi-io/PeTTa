; A -[nondet]-> function never fits a -[det]-> closure parameter,
; in normal mode just as under --strict-det.
(: apply-det (-[det]-> (-[det]-> Number Number) Number Number))
(= (apply-det $f $x) ($f $x))

(: pick (-[nondet]-> Number Number))
(= (pick $x) $x)
(= (pick $x) (+ $x 1))

!(apply-det pick 1)
