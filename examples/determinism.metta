(: choose (-[nondet]-> Number Number))
(= (choose $x)
   (superpose ($x (+ $x 1))))

(: choose-once (-[det]-> Number Number))
(= (choose-once $x)
   (once (choose $x)))

!(test (collapse (choose 10)) (10 11))
!(test (choose-once 10) 10)
