(= (only-pair 1 2) 3)

(: apply-future (-[det]-> (-[det]-> Number Number) Number))
(= (apply-future $function) ($function 3))

; The partial closure's remaining argument is not total merely because one
; fresh relational probe could bind it to the sole matching literal.
!(apply-future (only-pair 1))
