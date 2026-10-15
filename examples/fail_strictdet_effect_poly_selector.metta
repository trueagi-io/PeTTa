(: select-effect
   (-[$effect]-> (-[$effect]-> Number Number) Bool Number))
(= (select-effect $function True) ($function 1))
(= (select-effect $function False) ($function 2))

(: increment (-[det]-> Number Number))
(= (increment $value) (+ $value 1))

(: unbound-effect-selection (-[det]-> Number))
(= (unbound-effect-selection) (select-effect increment $selector))
