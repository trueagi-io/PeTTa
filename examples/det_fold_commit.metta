; A call to an effect-polymorphic library function commits (once/1) where the
; checker proves the instance det: its closure argument is det and its clause
; selection is det, so the fold leaves no choice point behind. A
; nondeterministic step keeps every answer, and a step that becomes
; nondeterministic at runtime recompiles the committed caller.
!(import! &self (library lib_roman))

(: add-step (-[det]-> Number Number Number))
(= (add-step $acc $item) (+ $acc $item))

(: sum-list (-[det]-> (List Number) Number))
(= (sum-list $items) (fold-flat add-step 0 $items))

!(test (sum-list (1 2 3)) 6)

(: inc (-[det]-> Number Number))
(= (inc $x) (+ $x 1))

(: incs (-[det]-> (List Number) (List Number)))
(= (incs $xs) (map-flat inc $xs))

!(test (incs (1 2 3)) (2 3 4))

(: either-step (-[nondet]-> Number Number Number))
(= (either-step $acc $item) (superpose ((+ $acc $item) (* $acc $item))))

(= (either-sums $items) (fold-flat either-step 1 $items))

!(test (collapse (either-sums (2 3))) (6 9 5 6))

(= (step $acc $item) (+ $acc $item))

(= (fixed-fold) (fold-flat step 1 (2 3)))

!(test (collapse (fixed-fold)) (6))

!(add-atom &self (= (step $acc $item) (* $acc $item)))

!(test (collapse (fixed-fold)) (6 9 5 6))
