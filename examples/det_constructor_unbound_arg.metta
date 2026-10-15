; Need-based -[det]-> boundness enforcement: the boundary check is emitted ONLY
; for a parameter whose boundness the determinism proof CONSUMED. A pure data
; constructor consumes none - ($x $y) is a var-headed tuple, built as data, det
; with its arguments unbound - so pair-up gets NO boundary check and may be
; called with an unbound argument. The var comes back in place.
(: pair-up (-[det]-> Number Number (Number Number)))
(= (pair-up $x $y) ($x $y))

; called with an unbound second argument: no boundary throw, the result is a
; two-element tuple carrying the unbound var in its second position.
(= (mk-open) (pair-up 1 $u))
!(test (size-atom (mk-open)) 2)

; and the ordinary fully-bound call constructs the pair as expected:
!(test (pair-up 1 2) (1 2))
