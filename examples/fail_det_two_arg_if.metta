; A -[det]-> function whose body is a two-argument if.
;
; (if $b 1) has no else branch, so it produces nothing when $b is false. That
; failure path belongs to the CONSTRUCT, not to any of its subexpressions, so
; combining the determinism of the parts - which is all the analysis used to
; do - never noticed it: a -[det]-> promise was satisfied by an expression
; with zero solutions half the time.
;
; A two-argument if is now may_fail unconditionally. Write the else branch, or
; declare the function -[semidet]->.
(: g (-[det]-> Bool Number))
(= (g $b) (if $b 1))
!(test (collapse (g false)) ())
