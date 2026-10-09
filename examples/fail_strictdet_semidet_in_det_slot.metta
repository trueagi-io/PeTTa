; A -[det]-> function promises exactly one result, so it may not call a
; -[semidet]-> one: (lookup 3) has no answer, and there is nothing wrap could
; return. Either handle the failure (collapse it, or fall back) or admit it in
; the arrow and declare wrap -[semidet]-> too - the same rule that rejects a
; bare (empty) in a -[det]-> body.
(: lookup (-[semidet]-> Number Number))
(= (lookup 1) 10)

(: wrap (-[det]-> Number Number))
(= (wrap $n) (+ 1 (lookup $n)))

!(test (wrap 1) 11)
