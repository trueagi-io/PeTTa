; spread is undeclared, and its clause calls the nondeterministic (get-atoms
; ...), so nothing proves it deterministic. Passing it where a -[det]->
; closure is required is rejected at compile time: a runtime type check cannot
; count a closure's solutions, so a determinism commitment can never be
; deferred to one - the same reason a conflicting newtype brand rejects
; instead of guarding.
(= (spread $x) (let $a (get-atoms &self) $x))

(: apply-det (-[det]-> (-[det]-> Number Number) Number Number))
(= (apply-det $f $x) ($f $x))

!(test (collapse (apply-det spread 3)) (3))
