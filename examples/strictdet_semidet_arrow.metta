; -[semidet]-> means ZERO OR ONE result: the function is allowed to fail, but
; never to produce two answers. Cardinality is a total order,
; det (exactly one) < semidet (zero or one) < nondet (any), and semidet is the
; answer to "I want a partial function": -[det]-> would be a lie and
; -[nondet]-> throws away the commit (and with it the choicepoint-free
; execution) for a nondeterminism that is not there.

; A partial lookup. No fallthrough clause: a catch-all would overlap the
; literal heads, and there is nothing to fall through to - failing IS the
; result for an unmatched key.
(: lookup (-[semidet]-> Number Number))
(= (lookup 1) 10)
(= (lookup 2) 20)

!(test (lookup 1) 10)
!(test (collapse (lookup 2)) (20))
!(test (collapse (lookup 3)) ())

; (empty) is semidet, not nondet: zero results is never two. It is rejected in
; a -[det]-> body and allowed here, so a semidet function can say "no answer"
; explicitly, in the middle of a computation.
(: safe-div (-[semidet]-> Number Number Number))
(= (safe-div $a $b) (if (== $b 0) (empty) (/ $a $b)))

!(test (safe-div 6 3) 2)
!(test (collapse (safe-div 6 0)) ())

; Failure propagates: a semidet caller may call a semidet callee (a -[det]->
; one may not - see fail_strictdet_semidet_in_det_slot.metta).
(: shifted (-[semidet]-> Number Number))
(= (shifted $k) (+ 1 (lookup $k)))

!(test (shifted 1) 11)
!(test (collapse (shifted 7)) ())

; Closure parameters carry the same order: a -[semidet]-> parameter accepts a
; det closure as well as a semidet one (fewer results always fit), and applying
; it inside a semidet body is fine.
(: inc (-[det]-> Number Number))
(= (inc $x) (+ $x 1))

(: apply-semi (-[semidet]-> (-[semidet]-> Number Number) Number Number))
(= (apply-semi $f $x) ($f $x))

!(test (apply-semi inc 1) 2)
!(test (apply-semi lookup 2) 20)
!(test (collapse (apply-semi lookup 9)) ())
