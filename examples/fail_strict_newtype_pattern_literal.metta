!(import! &self (library lib_builtin_types))

(: Bundle (Newtype (Number String)))
(: &bundles (SpaceOf (row Number Bundle)))

!(add-atom &bundles (row 1 (7 "valid")))

; The raw pattern contradicts Bundle's Number field.
(: bad-bundle (-> Number Number))
(= (bad-bundle $wanted)
   (match &bundles (row $wanted ("not-a-number" $_label)) 0))
