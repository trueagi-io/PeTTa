(: choose (-[nondet]-> Number Number))
(= (choose $x)
   (superpose ($x (+ $x 1))))

; once caps the solution count at one - it never manufactures one. Nothing
; here proves (choose $x) produces anything at all (a superpose over an empty
; list produces nothing, and the declaration -[nondet]-> says only "zero or
; more"), so the honest verdict for the wrapped call is "at most one" -
; -[semidet]->, not -[det]->. semidet commits and costs exactly like det; the
; difference is only in what it promises the caller.
(: choose-once (-[semidet]-> Number Number))
(= (choose-once $x)
   (once (choose $x)))

; A once around something already deterministic stays deterministic - once is
; a no-op there, and the analysis says so:
(: bump (-[det]-> Number Number))
(= (bump $x) (+ $x 1))

(: bump-once (-[det]-> Number Number))
(= (bump-once $x)
   (once (bump $x)))

!(test (collapse (choose 10)) (10 11))
!(test (choose-once 10) 10)
!(test (bump-once 10) 11)
