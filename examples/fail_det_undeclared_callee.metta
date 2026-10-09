; A deterministic function needs positive evidence about its callees: an
; undeclared callee whose clauses analyze as nondeterministic is rejected.
(= (pick $x) (superpose (1 2)))
(: wrap (-[det]-> Number Number))
(= (wrap $x) (pick $x))
!(test (wrap 5) 1)
