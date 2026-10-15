; Removing a committed declaration recompiles existing clauses against the
; declarations that remain, withdrawing the stale clause-entry cut.
(: f (-[det]-> Atom Number))
(= (f a) 1)

!(remove-atom &self (: f (-[det]-> Atom Number)))
!(add-atom &self (= (f a) 2))

!(test (collapse (f a)) (1 2))
