; A -[det]-> function whose body folds with a det closure argument through a
; higher-order library function (fold-flat, declared with plain -> arrows)
; is itself deterministic. fold-flat's det-ness is CONDITIONAL on its closure
; argument, so the transitive analysis re-checks the callee's clauses under
; the assumption that the arrow-typed parameter is det - and add-step, the
; actual argument here, is det. Passes in normal mode, --strict, and
; --strict-det.
!(import! &self (library lib_roman))

(: add-step (-[det]-> Number Number Number))
(= (add-step $acc $item)
   (+ $acc $item))

(: sum-list (-[det]-> (List Number) Number))
(= (sum-list $items)
   (fold-flat add-step 0 $items))

!(test (sum-list (1 2 3)) 6)
