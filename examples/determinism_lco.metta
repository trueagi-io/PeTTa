; Deterministic functions commit to their first matching clause: no residual
; choicepoints, so deep recursion runs in constant memory via last-call
; optimization even when clause indexing cannot discriminate the heads.
(: rot (Atom Atom Atom Number -[det]-> Number))
(= (rot a $x b $n) (if (== $n 0) $n (rot $x b a (- $n 1))))
(= (rot $x b a $n) (if (== $n 0) $n (rot b a $x (- $n 1))))
(= (rot b a $x $n) (if (== $n 0) $n (rot a $x b (- $n 1))))

!(test (rot a a b 1000000) 0)
