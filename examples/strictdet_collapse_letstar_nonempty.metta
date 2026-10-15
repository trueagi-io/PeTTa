; let* is judged as the nested let it is (binds_and_body_determinism
; desugars onto let_determinism), so the collapse properness guarantee and
; the nonemptiness narrowing reach let*-introduced variables too. From
; PeTTaChainer's typechecker_repros/det_nonempty_collapse_let_star.metta.
; The collapse-derived proper-list guarantee now survives a plain let, but it
; is still lost when the same binding appears after another binding in let*.
;
; Passes:
;   run.sh det_nonempty_collapse_let_star.metta --strict
; Fails on PeTTa typecheck-v2 d429c10:
;   run.sh det_nonempty_collapse_let_star.metta --strict --strict-det
;   Determinism check failed for 'collect-first-or-empty': call('first-list')

(: first-list (-[semidet]-> (List (List Number)) (List Number)))
(= (first-list (cons $head $_tail)) $head)

(: collect-first-or-empty (-[det]-> Number (List Number)))
(= (collect-first-or-empty $value)
   (let*
      (($ignored (quote ($value)))
       ($values (collapse (collapse $value))))
      (if (== $values ())
         ()
         (first-list $values))))

!(test (collect-first-or-empty 7) (7))
