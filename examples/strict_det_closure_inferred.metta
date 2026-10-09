; A determinism claim written in a declaration is checkable in EVERY mode.
; --strict-det exists to force you to make the claim, not to be a precondition
; for checking one you already wrote: so an inline lambda whose body the
; clause-set analysis PROVES deterministic carries a real -[det]-> arrow under
; plain --strict too, and fits a -[det]-> parameter at compile time - with no
; residual runtime type check for --strict to reject.

(: apply-det (-[det]-> (-[det]-> Number Number Number) Number Number Number))
(= (apply-det $f $a $b) ($f $a $b))

!(test (apply-det (|-> ($a $b) (+ $a $b)) 1 2) 3)
!(test (apply-det (|-> ($a $b) (* $a $b)) 3 4) 12)

; The proof is transitive: the lambda body calls an undeclared helper, whose
; own clauses have to analyze deterministic for the closure to fit.
(= (double $x) (+ $x $x))

!(test (apply-det (|-> ($a $b) (+ (double $a) $b)) 5 1) 11)

; A closure over a declared -[det]-> function fits the same slot; the inferred
; head is not doing anything the declared one would not.
(: scale (-[det]-> Number Number Number))
(= (scale $a $b) (* $a $b))

!(test (apply-det scale 6 7) 42)

; The same lambda also fits weaker slots - det is the strongest commitment, so
; it satisfies -[semidet]-> and -[nondet]-> requirements as well.
(: apply-semi (-[semidet]-> (-[semidet]-> Number Number Number) Number Number Number))
(= (apply-semi $f $a $b) ($f $a $b))

!(test (apply-semi (|-> ($a $b) (+ $a $b)) 10 5) 15)
