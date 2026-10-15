; collapse of heterogeneous alternatives is a union element type, which a
; concrete (List Number) declaration cannot absorb.
(: mixed (-> (List Number)))
(= (mixed) (collapse (superpose (1 "x"))))
!(test (mixed) (1))
