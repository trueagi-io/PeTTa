; A parameter reused inside the positional pattern is an equality constraint,
; not a fresh binder. The exact-width decons result does not make it total.
(: split-existing-det (-[det]-> Expression Bool))
(= (split-existing-det $term)
   (if (and (is-expr $term) (not (== $term ())))
      (let ($head $term) (decons $term) true)
      false))
