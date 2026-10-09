; -[semidet]-> COMMITS exactly like -[det]->: the right to fail costs no
; choicepoint, so a semidet function still compiles to a clause-entry cut and
; still recurses in constant memory via last-call optimization. Same shape as
; examples/determinism_lco.metta - clause indexing cannot discriminate these
; heads, so without the commit every level would leave a choicepoint behind -
; only the arrow differs.
(: rot (-[semidet]-> Atom Atom Atom Number Number))
(= (rot a $x b $n) (if (== $n 0) $n (rot $x b a (- $n 1))))
(= (rot $x b a $n) (if (== $n 0) $n (rot b a $x (- $n 1))))
(= (rot b a $x $n) (if (== $n 0) $n (rot a $x b (- $n 1))))

!(test (rot a a b 1000000) 0)

; And the failure semantics survive the commit: no clause matches (rot c c c),
; so the call fails instead of returning a wrong answer.
!(test (collapse (rot c c c 3)) ())
