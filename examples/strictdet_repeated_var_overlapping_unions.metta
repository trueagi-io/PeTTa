(: accepts-overlap
   (-[det]-> (| Number String) (| Number Bool) Bool))
(= (accepts-overlap $left $right) True)

(: overlap-at-same-variable (-[det]-> Number Bool))
(= (overlap-at-same-variable $value)
   (accepts-overlap $value $value))

!(test (overlap-at-same-variable 5) True)
