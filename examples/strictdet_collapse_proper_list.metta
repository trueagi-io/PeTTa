; Output-properness certificate (checker feature 2). Adapted from the
; PeTTaChainer repro det_collapse_union_bound_list.metta.
;
; collapse compiles to findall/3, whose result is ALWAYS a bound proper list -
; but a DECLARED (List Number) output type never proves that (a det function
; may still return an unbound variable), so the fact used to die at the clause
; boundary. collect-one's single clause result is a collapse form, so the
; checker certifies collect-one as producing a proper list; union-atom is then
; provably det on (collect-one $left), and collect-and-extend is a valid
; -[det]-> function.

(: collect-one (-[det]-> Number (List Number)))
(= (collect-one $value)
   (collapse $value))

(: collect-and-extend (-[det]-> Number Number (List Number)))
(= (collect-and-extend $left $right)
   (union-atom
      (collect-one $left)
      ($right)))

!(test (collect-and-extend 1 2) (1 2))

; The certificate is transitive within the file: recollect's clause result is a
; call to the already-certified collect-one, so recollect is certified too and
; the union-atom call in extend-twice is likewise det.
(: recollect (-[det]-> Number (List Number)))
(= (recollect $value)
   (collect-one $value))

(: extend-twice (-[det]-> Number Number (List Number)))
(= (extend-twice $left $right)
   (union-atom
      (recollect $left)
      ($right)))

!(test (extend-twice 3 4) (3 4))
