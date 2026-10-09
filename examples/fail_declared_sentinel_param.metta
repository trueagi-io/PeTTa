; The counterpart to inferred_sentinel_param.metta: the very same shape, but
; with a DECLARED parameter type. A declaration is a promise, not an inference,
; so the call site is checked and this is rejected at compile time - dropping
; the demand for inferred types must not weaken declared ones.
(: score-d (-> Number Number Number))
(= (score-d $current $cand) (if (== $current 0) $cand (max $cand $current)))

!(score-d "none" 0.42)
