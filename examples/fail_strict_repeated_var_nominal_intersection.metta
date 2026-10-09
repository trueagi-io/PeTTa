; Strict typing cannot express the unresolved A ∩ B obligation created when
; one runtime value fills two differently nominal positions.
(: shared A)
(: shared B)

(: both (-[det]-> A B Bool))
(= (both $left $right) True)

(: use-shared (-[det]-> A Bool))
(= (use-shared $x) (both $x $x))
