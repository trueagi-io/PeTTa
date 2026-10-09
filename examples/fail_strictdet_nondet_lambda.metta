; A lambda whose body superposes is nondeterministic, so it must not fit
; a det fold accumulator position.
(: pick-sum (-[det]-> (List Number) Number))
(= (pick-sum $items)
   (foldl-atom $items 0
      (|-> ($acc $item) (superpose ($acc (+ $acc $item))))))

!(pick-sum (1 2))
