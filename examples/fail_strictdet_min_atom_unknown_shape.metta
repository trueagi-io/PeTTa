; The argument-aware verdicts are provable-only. A list that arrives as a
; parameter may be () - min_list/2 has no answer for that - or open, in which
; case foldl/4 enumerates its lengths. Neither is ruled out by the (List
; Number) type, so min-atom stays semidet here and a -[det]-> body may not
; call it.

(: min-of (-[det]-> (List Number) Number))
(= (min-of $xs) (min-atom $xs))

!(min-of (1 2))
