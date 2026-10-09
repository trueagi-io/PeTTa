!(import! &self (library lib_builtin_types))

(: &labels (SpaceOf (label-row Number String)))
(: &offsets (SpaceOf (offset-row Number Number)))

!(add-atom &labels (label-row 1 "one"))
!(add-atom &offsets (offset-row 1 10))

; A match body receives the declared positional result expectation.
(: matched-row (-> Number (Number String)))
(= (matched-row $wanted)
   (match &labels (label-row $wanted $label)
          (data (+ $wanted 1) $label)))

!(test (matched-row 1) (2 "one"))

; The same expectation crosses nested match result positions.
(: nested-matched-row (-> Number (Number String)))
(= (nested-matched-row $wanted)
   (match &labels (label-row $wanted $label)
          (match &offsets (offset-row $wanted $offset)
                 (data (+ $wanted $offset) $label))))

!(test (nested-matched-row 1) (11 "one"))

; List expectations also reach explicit list construction in a match body.
(: matched-list (-> Number (List Number)))
(= (matched-list $wanted)
   (match &offsets (offset-row $wanted $offset)
          (make-list $wanted $offset)))

!(test (matched-list 1) (1 10))
