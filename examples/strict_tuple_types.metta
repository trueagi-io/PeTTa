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

; Untagged tuple element types propagate through cons patterns in heads:
(: pair-weight-sum (-> (List ($value Number)) Number))
(= (pair-weight-sum ()) 0.0)
(= (pair-weight-sum (cons ($val $weight) $tail))
   (+ $weight (pair-weight-sum $tail)))

!(test (pair-weight-sum ((a 1.5) (b 2.5))) 4.0)

; Destructuring let binds pattern fields from the value's known type:
(: make-stats (-> (Stats Number Number Number)))
(= (make-stats) (Stats 1.0 2.0 3))

(: stats-total (-> Number))
(= (stats-total)
   (let (Stats $sum $sumsq $count) (make-stats)
      (+ $sum 1.0)))

!(test (stats-total) 2.0)

; A type variable in tuple head position unified to a primitive stays an
; untagged pair: ($first Number) -> (Number Number) is not a tag named Number.
(: numeric-pairs (-> (List ($first Number))))
(= (numeric-pairs)
   ((1.0 2.0) (3.0 4.0)))

!(test (numeric-pairs) ((1.0 2.0) (3.0 4.0)))
