!(import! &self (library lib_builtin_types))

(: &labels (SpaceOf (label-row Number String)))
!(add-atom &labels (label-row 1 "one"))

; The String literal contradicts the declared second Number result field.
(: bad-matched-row (-> Number (Number Number)))
(= (bad-matched-row $wanted)
   (match &labels (label-row $wanted $_label)
          (data $wanted "not-a-number")))
