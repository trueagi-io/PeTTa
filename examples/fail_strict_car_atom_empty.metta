; car-atom's element-type certification (list_elem_out_type/2) says the result
; of (car-atom $xs) on a (List Number) IS a Number - so the runtime has no
; licence to answer () for the empty list, which has no head. It raises
; instead, which keeps both the certification and the det verdict sound (an
; exception is not a solution). Before, this program compiled clean under
; --strict and died inside is/2 with the certified () - a wrong answer where
; a proven Number was expected, the exact shape --strict promises away.
(: f (-> (List Number) Number))
(= (f $xs) (+ 1 (car-atom $xs)))
!(f ())
