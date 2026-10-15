(: bad-effect-map
   (-[$e]-> (-[$e]-> Number Number) (List Number) (List Number)))
(= (bad-effect-map $f ()) ())
(= (bad-effect-map $f (cons $x $xs))
   (cons ($f $x) (bad-effect-map $f $xs)))

(: branching-effect (-[nondet]-> Number Number))
(= (branching-effect $x) (superpose ($x (+ $x 1))))

(: wrongly-det-effect-use (-[det]-> (List Number) (List Number)))
(= (wrongly-det-effect-use $xs)
   (bad-effect-map branching-effect $xs))
