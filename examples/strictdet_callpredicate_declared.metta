; A determinism arrow declared for a PROLOG predicate - a symbol with no
; MeTTa clauses - is a trusted foreign promise, and callPredicate reads it
; off a manifest (Predicate (g ...)) goal: assertz/2 is declared -[det]->
; in lib_builtin_types, so this wrapper proves det. From PeTTaChainer's
; typechecker_repros/det_callpredicate_assertz.metta.
; callPredicate is nondet in general, but this concrete Prolog goal is
; assertz/2: it succeeds exactly once and binds its clause-reference output.
; A deterministic wrapper should be able to rely on that whitelisted mode
; without hiding the call inside collapse.
;
; Fails on PeTTa typecheck-v2 418a0c5 in both modes:
;   run.sh det_callpredicate_assertz.metta --strict
;   run.sh det_callpredicate_assertz.metta --strict --strict-det
;   Determinism check failed for 'assert-one': call(callPredicate)

(: assert-one (-[det]-> Atom Bool))
(= (assert-one $value)
   (let*
      (($term (Predicate (remembered $value)))
       ($_asserted
          (callPredicate
             (Predicate (assertz $term $_clause-ref)))))
      true))

!(test (assert-one hello) true)
