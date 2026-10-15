; A deterministic body does not make an uncommitted relation total.
(= (only-one 1) 1)

(: inferred-zero-consumer (-[det]-> Number))
(= (inferred-zero-consumer) (only-one 2))
