; wrap's proof initially infers helper as det. Adding a nondet helper clause
; recompiles recorded callers transitively, so the stale proof is rejected
; instead of letting wrap return more than once.
(: helper (-> Atom Atom))
(= (helper $x) $x)

(: wrap (-[det]-> Atom Atom))
(= (wrap $x) (helper $x))

!(add-atom &self (= (helper $x) (superpose ($x extra))))
