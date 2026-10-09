(= (only-one 1) 1)

(: apply-partial (-[det]-> (-[det]-> Number Number) Number))
(= (apply-partial $function) ($function 2))

!(apply-partial only-one)
