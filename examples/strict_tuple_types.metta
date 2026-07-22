!(import! &self (library lib_builtin_types))

; Structural tuple types: (STV Number Number) describes the tagged expression
; (STV 0.5 0.8). Pattern parameters bind their field types, constructed
; values check structurally, and case patterns narrow - all under --strict.

(: tuple-confidence (-> (STV Number Number) Number))
(= (tuple-confidence (STV $strength $confidence))
   $confidence)

!(test (tuple-confidence (STV 0.5 0.8)) 0.8)

(: make-tv (-> (STV Number Number)))
(= (make-tv) (STV 0.5 0.8))

!(test (tuple-confidence (make-tv)) 0.8)

(: scale-tv (-> (STV Number Number) Number (STV Number Number)))
(= (scale-tv (STV $s $c) $k) (STV (* $s $k) $c))

!(test (scale-tv (make-tv) 2) (STV 1.0 0.8))

(: particle-id (-> (ParticleDist Number Number) Number))
(= (particle-id $distribution)
   (case $distribution
      (((ParticleDist $id $scale) $id))))

!(test (particle-id (ParticleDist 7 0.5)) 7)
