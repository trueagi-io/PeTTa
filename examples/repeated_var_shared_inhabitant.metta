; Open nominal types can overlap. In default mode the repeated argument keeps
; both deferred requirements, and the shared inhabitant satisfies both.
(: shared A)
(: shared B)

(: both (-[det]-> A B Bool))
(= (both $left $right) True)

(: use-shared (-[det]-> A Bool))
(= (use-shared $x) (both $x $x))

!(test (use-shared shared) True)
