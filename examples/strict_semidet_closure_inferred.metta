; The inferred arrow of an inline lambda carries whatever level the clause-set
; analysis commits to, not just det. A body that may produce ZERO results but
; never two analyzes as semidet, and a -[semidet]-> parameter accepts it under
; plain --strict - no --strict-det needed, and no residual runtime check.

(: apply-semi (-[semidet]-> (-[semidet]-> Number Number) Number Number))
(= (apply-semi $f $x) ($f $x))

; (empty) is the canonical semidet body: zero results is never two.
!(test (apply-semi (|-> ($x) (if (== $x 0) (empty) (/ 10 $x))) 5) 2)
!(test (collapse (apply-semi (|-> ($x) (if (== $x 0) (empty) (/ 10 $x))) 0)) ())

; Failure inherited from a semidet callee is still only failure.
(: lookup (-[semidet]-> Number Number))
(= (lookup 1) 10)
(= (lookup 2) 20)

!(test (apply-semi (|-> ($k) (+ 1 (lookup $k))) 2) 21)
!(test (collapse (apply-semi (|-> ($k) (+ 1 (lookup $k))) 9)) ())

; A det lambda fits the semidet slot too (fewer results always fit), so the
; semidet head is a ceiling, not a requirement.
!(test (apply-semi (|-> ($x) (+ $x 1)) 4) 5)
