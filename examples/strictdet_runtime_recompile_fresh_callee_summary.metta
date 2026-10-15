; A runtime clause addition invalidates the file-analysis scope and recompiles
; dependents.  The unchanged consumer still needs the callee's proper-Bool
; result fact: a bare Bool type is deliberately not enough to call and/2 in a
; semidet body.  Recompilation must therefore solve fresh summaries from the
; current stored clauses instead of analyzing this consumer in isolation.
(: bool-from-runtime-case (-[semidet]-> Bool Bool))
(= (bool-from-runtime-case true)
   (case true ((true true) (false false))))

(: consume-runtime-case-bool (-[semidet]-> Bool Bool))
(= (consume-runtime-case-bool $choice)
   (and (bool-from-runtime-case $choice) true))

!(add-atom &self
   (= (bool-from-runtime-case false)
      (case false ((true true) (false false)))))

!(test (consume-runtime-case-bool true) true)
!(test (consume-runtime-case-bool false) false)
