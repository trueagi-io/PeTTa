; A ground call is not a bound probe: (field (mk)) is well typed and det, yet
; it returns an unbound Atom, so is-member enumerates the list.
(: Boxed Type)
(: B (-[det]-> Atom Boxed))
(: mk (-[det]-> Boxed))
(= (mk) (B $u))
(: field (-[det]-> Boxed Atom))
(= (field (B $b)) $b)
(: anything-known? (-[det]-> Bool))
(= (anything-known?) (is-member (field (mk)) (alpha beta)))
!(anything-known?)
