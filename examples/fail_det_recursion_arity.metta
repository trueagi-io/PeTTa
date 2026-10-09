; f/1 calling f/2 is not recursive evidence about the same relation.
(= (arity-hop $x) (arity-hop $x $x))
(= (arity-hop $x $y) (superpose (1 2)))

(: arity-hop-consumer (-[det]-> Number))
(= (arity-hop-consumer) (arity-hop 0))
