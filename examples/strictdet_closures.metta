(: add-offset (-[det]-> Number Number Number Number))
(= (add-offset $offset $acc $item)
   (+ $acc (+ $offset $item)))

(: fold-flat (-[det]-> (-[det]-> $acc $item $acc) $acc (List $item) $acc))
(= (fold-flat $f $init ()) $init)
(= (fold-flat $f $init (cons $x $xs))
   (fold-flat $f ($f $init $x) $xs))

; Underapplying a det function constructs a det closure; passing it where
; fold-flat requires a det accumulator function must be accepted.
(: sum-offset (-[det]-> Number (List Number) Number))
(= (sum-offset $offset $items)
   (fold-flat (add-offset $offset) 0 $items))

; A lambda with a deterministic body gets a det arrow from clause analysis.
(: sum-offset-lambda (-[det]-> Number (List Number) Number))
(= (sum-offset-lambda $offset $items)
   (fold-flat (|-> ($acc $item) (+ $acc (+ $offset $item))) 0 $items))

!(test (sum-offset 1 (2 3)) 7)
!(test (sum-offset-lambda 1 (2 3)) 7)
