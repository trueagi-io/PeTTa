; A parameter the body tests with is-var is EXEMPT from the -[det]->
; boundary boundness check: the author wrote exactly the branch the check
; would preempt, so the compiler does not overrule them. The body must then
; prove itself deterministic WITH a possibly-unbound value, which the is-var
; branch structure does. Runs in the default mode - the det commitment is
; checked in every mode.
(: opt-inc (-[det]-> Number Number))
(= (opt-inc $x) (if (is-var $x) 0 (+ $x 1)))

!(test (opt-inc 4) 5)

; passing a genuinely unbound argument takes the handled branch instead of
; throwing at the boundary:
(= (probe) (opt-inc $fresh))
!(test (probe) 0)
