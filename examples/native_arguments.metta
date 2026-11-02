; A native (Prolog-implemented) function takes its arguments by the same rule
; as a MeTTa function with the same declaration: evaluated, except for
; Atom-typed parameters, which receive the source expression unevaluated.
; This pins existing behaviour; the late-import difference is covered by
; late_call_recompile.metta.
!(assertaPredicate (Predicate (native-id $x $x)))
!(assertaPredicate (Predicate (native-atom $x $x)))
!(import_prolog_function native-id)
!(import_prolog_function native-atom)
(: native-atom (-> Atom Atom))
(= (metta-id $x) $x)
(: metta-atom (-> Atom Atom))
(= (metta-atom $x) $x)

!(test (native-id (car-atom (1 2))) 1)
!(test (metta-id (car-atom (1 2))) 1)
(= (native-of-head $xs) (native-id (car-atom $xs)))
!(test (native-of-head (7 8)) 7)

!(test (native-atom (car-atom (1 2))) (quote (car-atom (1 2))))
!(test (metta-atom (car-atom (1 2))) (quote (car-atom (1 2))))

; An Atom parameter receives a (data ...) argument as the literal source term,
; for a native and a MeTTa function alike; an evaluated one gets the tuple.
!(test (native-id (data 1 2)) (1 2))
!(test (native-atom (data 1 2)) (quote (data 1 2)))
!(test (metta-atom (data 1 2)) (quote (data 1 2)))
