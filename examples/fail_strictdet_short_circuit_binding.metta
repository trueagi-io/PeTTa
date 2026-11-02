; A binding made only in a short-circuited right operand is not a binding:
; and False skips (let $x 1 True), so $x may be unbound afterwards.
(: f (-[det]-> Number))
(= (f)
   (let $_ (and False (let $x 1 True))
        (+ $x 1)))
!(collapse (f))
