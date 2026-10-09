; Calls inside a data literal are evaluated: (1 (one)) is the list (1 1).
(: one (-[det]-> Number))
(= (one) 1)
(: has-one? (-[det]-> Number Bool))
(= (has-one? $x) (is-member $x (1 (one))))
!(has-one? 1)
