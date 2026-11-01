(: a A)
(: only-a (-> A %Undefined%))
(= (only-a $x) (ok $x))

; The unused field stays a wildcard in (pair A %Undefined%). The requirement
; on $a survives at callers, so fa needs no inline guard or declaration.
(= (fa (pair $a $unused)) (only-a $a))
!(test (fa (pair a 1)) (ok a))
!(test (fa (pair a arbitrary)) (ok a))

; Partial shapes compose through nested tagged patterns.
(= (nested-fa (box (pair $a $unused))) (only-a $a))
!(test (nested-fa (box (pair a 1))) (ok a))

; Forwarding a partial shape does not make its wildcard field an obligation.
(= (fa-through (pair $a $unused)) (fa (pair $a $unused)))
!(test (fa-through (pair a arbitrary)) (ok a))
!(test (fa-through (pair a $unbound)) (ok a))
