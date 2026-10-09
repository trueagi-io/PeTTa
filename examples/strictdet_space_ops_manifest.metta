; add-atom/remove-atom are keyed semidet (builtin_call_determinism/3): their
; second clause reads the atom as a list and FAILS on a non-list argument, so a
; call whose argument shape is unknown may fail. But when the argument is a
; MANIFEST non-empty expression written at the call site, the =.. succeeds,
; assertz/retractall are det, and the function-equation first clause (guarded on
; [=,_,_]) is equally det - so exactly one clause commits and the call is det.
; call_site_determinism/4 recognises this via manifest_nonempty_list/1.

(: add-one-fact (-[det]-> Bool))
(= (add-one-fact)
   (add-atom &self (fact 1)))

!(test (add-one-fact) true)

; remove-atom over a manifest literal is det for the same reason: retractall/1
; over a fully-built term answers exactly once.
(: drop-one-fact (-[det]-> Bool))
(= (drop-one-fact)
   (remove-atom &self (fact 1)))

!(add-atom &self (fact 2))
!(test (drop-one-fact) true)

; A DECLARED type never qualifies as manifest - only a literal spine built here
; does - so a typed *variable* argument (its field may arrive unbound) still
; costs the semidet verdict; that is why remove-entry over a declared Entry
; stays out of scope. This file only asserts the manifest-literal case.
