; A caller compiled while its callee has one clause may copy that clause.
; Later source clauses of the callee must still reach the caller.
(: Shape Type)
(: Circle (-[det]-> Number Shape))
(: Square (-[det]-> Number Shape))

(: area (-[det]-> Shape Number))
(= (area (Circle $r)) (* 3 (* $r $r)))

(: use-area (-[det]-> Shape Number))
(= (use-area $s) (+ 1 (area $s)))

(= (area (Square $s)) (* $s $s))

(: pick (-[semidet]-> Number Number Number))
(= (pick 0 $y) (+ $y 10))

(: use-pick (-[semidet]-> Number Number))
(= (use-pick $x) (pick $x 1))

(= (pick 1 $y) (+ $y 20))

!(test (use-area (Circle 2)) 13)
!(test (use-area (Square 2)) 5)
!(test (use-pick 0) 11)
!(test (use-pick 1) 21)
