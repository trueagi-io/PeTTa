!(import! &self (library lib_roman))

(: add-offset (-> Number Number Number Number))
(= (add-offset $offset $acc $item)
   (+ $acc (+ $offset $item)))

; Underapplying a det function constructs a det closure; passing it where
; fold-flat requires a det accumulator function must be accepted.
(: sum-offset (-> Number (List Number) Number))
(= (sum-offset $offset $items)
   (fold-flat (add-offset $offset) 0 $items))

; A lambda with a deterministic body gets a det arrow from clause analysis.
(: sum-offset-lambda (-> Number (List Number) Number))
(= (sum-offset-lambda $offset $items)
   (fold-flat (|-> ($acc $item) (+ $acc (+ $offset $item))) 0 $items))

!(test (sum-offset 1 (2 3)) 7)
!(test (sum-offset-lambda 1 (2 3)) 7)
