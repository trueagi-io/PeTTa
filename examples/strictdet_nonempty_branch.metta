; Flow-sensitive nonemptiness (checker feature 1). Adapted from the
; PeTTaChainer repro det_nonempty_semidet_branch.metta.
;
; In (if (== $values ()) Then Else) the Else branch runs exactly when $values
; is a NONEMPTY list, so a -[semidet]-> accessor whose ONLY incompleteness is
; the empty case is deterministic there. The upgrade is proven, not assumed:
; the cons-pattern head covers every nonempty list (coverage), and the single
; clause body is a bare variable, which cannot fail (no-fail body). Both legs
; hold, so first-number is det AT THIS SITE and first-or-zero is a valid
; -[det]-> function.

(: first-number (-[semidet]-> (List Number) Number))
(= (first-number (cons $head $_tail)) $head)

(: first-or-zero (-[det]-> (List Number) Number))
(= (first-or-zero $values)
   (if (== $values ())
      0
      (first-number $values)))

!(test (first-or-zero (4 5)) 4)
!(test (first-or-zero ()) 0)

; The reversed condition (== () $values) narrows the same way.
(: head-or-zero (-[det]-> (List Number) Number))
(= (head-or-zero $values)
   (if (== () $values)
      0
      (first-number $values)))

!(test (head-or-zero (7 8 9)) 7)
!(test (head-or-zero ()) 0)
