; decons succeeds once for a nonempty expression and fails for an empty one,
; so a wrapper may promise semidet but not det.
(: split-expression (-[semidet]-> Expression Expression))
(= (split-expression $value) (decons $value))

!(test (split-expression (head tail)) (head (tail)))
!(test (collapse (split-expression ())) ())
