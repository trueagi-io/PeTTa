; (cons-atom 1 (1)) has no repeated subterm, but it builds (1 1): the literal
; that the duplicate check must inspect is the runtime list, not its source.
(: has-one? (-[det]-> Number Bool))
(= (has-one? $x) (is-member $x (cons-atom 1 (1))))
!(has-one? 1)
