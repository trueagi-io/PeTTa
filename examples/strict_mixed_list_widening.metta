; A heterogeneous cons chain is still a proper list - of the WIDENED element
; type, the union of what the tail holds and what the head is (the same
; merge collapse uses for disagreeing branches). Both members fit Atom, so
; the declared (List Atom) discharges with no residual guard; against
; (List Number) the non-fitting member is a compile-time conflict instead
; (fail_strict_mixed_list_not_numbers.metta). From PeTTaChainer's
; typechecker_repros/mixed_list_atom_widening.metta.
(: Item Type)
(: item (-> Number Item))

(: mixed-values (-> (List Atom)))
(= (mixed-values)
   (cons ()
      (cons (item 1) ())))

!(test (mixed-values) (() (item 1)))

; append widens the same way:
(: more-values (-> (List Atom)))
(= (more-values) (union-atom ((item 2)) (7 8)))
!(test (more-values) ((item 2) 7 8))
