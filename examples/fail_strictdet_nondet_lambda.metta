!(import! &self (library lib_roman))

; A lambda whose body superposes is nondeterministic, so it must not fit
; fold-flat's plain (det-committed) accumulator arrow under --strict-det.
(: pick-sum (-> (List Number) Number))
(= (pick-sum $items)
   (fold-flat (|-> ($acc $item) (superpose ($acc (+ $acc $item)))) 0 $items))

!(pick-sum (1 2))
