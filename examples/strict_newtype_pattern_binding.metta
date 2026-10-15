!(import! &self (library lib_builtin_types))

; Structural match patterns see through product-representation newtypes.
(: Bundle (Newtype (Number String)))
(: &bundles (SpaceOf (row Number Bundle)))

!(add-atom &bundles (row 1 (7 "plain")))

(: bundle-first (-> Number Number))
(= (bundle-first $wanted)
   (match &bundles (row $wanted ($a $_label)) $a))

!(test (bundle-first 1) 7)

; Pattern binding also follows a transitive newtype representation. Loading
; this declaration under strict mode proves that $a reaches Number without a
; residual runtime check.
(: NestedBundle (Newtype Bundle))
(: &nested-bundles (SpaceOf (row Number NestedBundle)))

(: nested-bundle-first (-> Number Number))
(= (nested-bundle-first $wanted)
   (match &nested-bundles (row $wanted ($a $_label)) $a))
