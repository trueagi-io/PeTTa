; The argument-aware transitive determinism gate must NOT over-accept: a
; -[det]-> function that folds a -[nondet]-> closure through fold-flat is
; genuinely nondeterministic and must still be rejected. The closure
; argument (nd-step) carries no det evidence, so det_closure_args_ok fails
; and the call stays undetermined - a determinism check failure.
!(import! &self (library lib_roman))

(: nd-step (-[nondet]-> Number Number Number))
(= (nd-step $acc $item)
   (superpose ($acc $item)))

(: sum-nd (-[det]-> (List Number) Number))
(= (sum-nd $items)
   (fold-flat nd-step 0 $items))

!(test (sum-nd (1 2 3)) 1)
