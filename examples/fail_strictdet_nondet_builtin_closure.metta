; or/2 is nondet: bool/1 enumerates the boolean type on an operand it was not
; given. The builtin table is the authoritative effect source for `or`, both
; in direct calls and closure position, so this is rejected.

(: any-true (-[det]-> (List Bool) Bool))
(= (any-true $bs) (foldl-atom $bs False or))

!(any-true (True False))
