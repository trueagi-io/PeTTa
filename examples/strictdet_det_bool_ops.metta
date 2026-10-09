; -[det]->/-[semidet]-> BOUNDNESS enforcement makes "typed implies bound" hold
; at the boundary: a direct Bool parameter is checked nonvar on entry, so
; and/or/not/xor/implies find a value to TEST rather than a boolean to invent.
; Without enforcement bool/1 enumerates an unbound operand, which is why these
; wrappers were nondet (and rejected under a -[det]-> promise) before.
(: both-true (-[det]-> Bool Bool Bool))
(= (both-true $l $r) (and $l $r))

(: either-true (-[det]-> Bool Bool Bool))
(= (either-true $l $r) (or $l $r))

(: invert (-[det]-> Bool Bool))
(= (invert $b) (not $b))

(: differ (-[det]-> Bool Bool Bool))
(= (differ $l $r) (xor $l $r))

!(test (both-true true true) true)
!(test (either-true false true) true)
!(test (invert false) true)
!(test (differ true false) true)
