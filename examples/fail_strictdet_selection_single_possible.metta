; A lone merely-possible clause is at most one, not exactly one.
(= (only-true True) 1)

(: bad (-[det]-> Bool Number))
(= (bad $value) (only-true $value))
