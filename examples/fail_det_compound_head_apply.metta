; A compound-headed argument - ((foo) 2 3) - is an APPLICATION: the translator
; compiles it through apply_fn/reduce, and its result is whatever the applied
; closure returns, possibly (). No part of its spine is built at this call
; site, so it must not count as a manifest list (data_headed/1), and min-atom
; must stay semidet over it: a -[det]-> body calling it is rejected.
; Before, data_headed/1's catch-all read every compound head as data; this
; program compiled clean in every mode and (bad) produced ZERO solutions at
; runtime - min-atom failed on the empty result and the commit cut sealed it.
(: empties (-[det]-> Number Number (List Number)))
(= (empties $a $b) ())
(: foo (-> (-[det]-> Number Number (List Number))))
(= (foo) empties)
(: bad (-[det]-> Number))
(= (bad) (min-atom ((foo) 2 3)))
!(bad)
