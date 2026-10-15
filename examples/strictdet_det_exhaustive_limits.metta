; The exhaustiveness check for -[det]-> is deliberately ONE-SIDED: it errors
; only where incompleteness is PROVEN, and stays silent everywhere else. It is
; not GHC's "warn unless proven exhaustive" - PeTTa's nominal types are open
; (a constructor can be declared in a later file), so "cannot prove" is the
; normal case, and erroring on it would reject working code with no escape.
; Every function below is accepted; this file is the regression guard against
; the check turning noisy.

; Exhaustive only because of an arithmetic condition the checker never looks
; at: a variable in the clause head covers the domain, and that is enough.
(: sign (-[det]-> Number Number))
(= (sign $n) (if (< $n 0) -1 (if (> $n 0) 1 0)))

; Literal heads over an argument whose type is a wildcard: Atom has no domain
; to enumerate, so nothing is provable here even though (pick-branch c) does
; fail. Silence, not a guess.
(: pick-branch (-[det]-> Atom Number))
(= (pick-branch a) 1)
(= (pick-branch b) 2)

; Bool IS closed, and these clauses do cover it - a closed type fully matched
; must never be reported.
(: flag (-[det]-> Bool Bool))
(= (flag true) false)
(= (flag false) true)

; A nominal type whose constructors are all matched: same story one level up.
(: Light Type)
(: Red Light)
(: Green Light)

(: go (-[det]-> Light Bool))
(= (go Red) false)
(= (go Green) true)

; A constructor argument the checker cannot enumerate at all (the field is a
; Number): the column consumes every declared constructor of Box, and what the
; fields do is not its business.
(: Box Type)
(: Boxed (-[det]-> Number Box))

(: unbox (-[det]-> Box Number))
(= (unbox (Boxed $n)) $n)

!(test (sign -3) -1)
!(test (sign 0) 0)
!(test (pick-branch b) 2)
!(test (flag true) false)
!(test (go Green) true)
!(test (unbox (Boxed 4)) 4)
