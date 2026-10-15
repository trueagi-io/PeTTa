; The inferred determinism of a lambda is a PROOF, not a rubber stamp: a body
; that superposes analyzes as nondet, so the closure has no det commitment to
; offer and must not fit a -[det]-> parameter - under plain --strict, exactly
; as under --strict-det.

(: apply-det (-[det]-> (-[det]-> Number Number Number) Number Number Number))
(= (apply-det $f $a $b) ($f $a $b))

!(apply-det (|-> ($a $b) (superpose ($a $b))) 1 2)
