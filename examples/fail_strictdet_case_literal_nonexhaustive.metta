; Literal scrutinees use the ordinary value-typing relation, so True is known
; to be Bool and the missing True branch is a provable failure path.
(: impossible (-[det]-> Number))
(= (impossible) (case True ((False 1))))
