(: default-effect-map
   (-[$e]-> (-[$e]-> $a $b) (List $a) (List $b)))
(= (default-effect-map $f ()) ())
(= (default-effect-map $f (cons $x $xs))
   (cons ($f $x) (default-effect-map $f $xs)))

(: default-effect-inc (-[det]-> Number Number))
(= (default-effect-inc $x) (+ $x 1))

(: default-effect-choice (-[nondet]-> Number Number))
(= (default-effect-choice $x) (superpose ($x (+ $x 10))))

!(test (default-effect-map default-effect-inc (1 2)) (2 3))
!(test
   (collapse (default-effect-map default-effect-choice (1 2)))
   ((1 2) (1 12) (11 2) (11 12)))
