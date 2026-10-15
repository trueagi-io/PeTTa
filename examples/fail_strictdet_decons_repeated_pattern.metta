; Even with a nonempty input, decons can return different head/tail fields.
; A repeated positional binder therefore remains a real failure path.
(: split-repeated-det (-[det]-> Expression Bool))
(= (split-repeated-det $term)
   (if (and (is-expr $term) (not (== $term ())))
      (let ($field $field) (decons $term) true)
      false))
