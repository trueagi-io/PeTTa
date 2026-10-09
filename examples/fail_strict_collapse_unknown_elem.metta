; (collapse ...) over alternatives of mixed knowledge: 0.1 is a Number, $x is
; arbitrary data. The element type is therefore unknown, and a (List Number)
; result may not be certified from the one alternative that happens to be
; typed - nor from an open element type variable, which unifies with any
; requirement at all.
(: f (-> Expression (List Number)))
(= (f $x) (collapse (superpose ($x 0.1))))

!(test (f "str") (1))
