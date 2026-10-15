; Non-overlapping heads still enumerate when their argument is unbound.
(= (choose-head a) 1)
(= (choose-head b) 2)

(: inferred-many-consumer (-[det]-> Number))
(= (inferred-many-consumer) (choose-head $which))
