; The nonemptiness narrowing fires on a LET-introduced variable when the let
; value is guaranteed a proper list - a collapse form (findall output) or a
; call to a proper_list_output-certified function. The guarantee is what
; makes the narrowing's coverage leg sound: a cons head cannot match a
; non-list, and collapse rules non-lists out unconditionally. From
; PeTTaChainer's typechecker_repros/det_nonempty_collapse_binding.metta.
(: first-list (-[semidet]-> (List (List Number)) (List Number)))
(= (first-list (cons $head $_tail)) $head)

(: collect-first-or-empty (-[det]-> Number (List Number)))
(= (collect-first-or-empty $value)
   (let $values
      (collapse (collapse $value))
      (if (== $values ())
         ()
         (first-list $values))))

!(test (collect-first-or-empty 7) (7))
