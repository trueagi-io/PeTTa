; A quoted comparison is syntax, so it cannot certify a Bool result.
(: held_comparison (-> Number Atom))
(= (held_comparison $x) (== $x 5))
(: uses_comparison (-[det]-> Number Bool))
(= (uses_comparison $x) (and (held_comparison $x) true))
