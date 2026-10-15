; An if/case/let merges several branches into one value, and EVERY branch has
; to fit the type the merged value is used at. A branch whose type the checker
; cannot determine ($x below is Expression, i.e. arbitrary data) is not one
; that fits: one typed branch must not discharge the obligation for all of
; them. The argument position is the dangerous one - no output certification
; covers it, so the unsound discharge here used to reach the runtime as a raw
; Prolog error inside (+ $n 1) rather than a type error.
(: need (-> Number Number))
(= (need $n) (+ $n 1))

(: f (-> Expression Bool Number))
(= (f $x $c) (need (if $c 0.1 $x)))

!(test (f "str" false) 1)
