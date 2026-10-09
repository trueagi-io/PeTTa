; The widened element type of a heterogeneous cons chain keeps every member,
; so a list that definitely contains an Item is a definite COMPILE-TIME
; conflict against (List Number) - not a discharged obligation, and not a
; deferred runtime guard. The value cannot honour the declaration.
(: Item Type)
(: item (-> Number Item))

(: nums (-> (List Number)))
(= (nums) (cons (item 1) (cons 2 ())))

!(nums)
