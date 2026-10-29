; A callee with one clause and a caller that may copy it. inline_extended.metta
; imports this file and adds a second clause to the callee.
(: pick (-[semidet]-> Number Number Number))
(= (pick 0 $y) (+ $y 10))

(: use-pick (-[semidet]-> Number Number))
(= (use-pick $x) (pick $x 1))

!(test (use-pick 0) 11)
