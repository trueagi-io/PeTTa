; is-member is det only against a list whose runtime value has no duplicate.
; (twins) is a call: its source syntax is one ground symbol, but it evaluates
; to (1 1), so a bound probe still answers true twice.
(: twins (-[det]-> Expression))
(= (twins) (1 1))
(: has-one? (-[det]-> Number Bool))
(= (has-one? $x) (is-member $x (twins)))
!(has-one? 1)
