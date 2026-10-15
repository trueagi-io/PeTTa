; The same promise, one level deeper: $a is the INPUT type of a closure
; parameter, so (: g2 (-> (-> $a Number) Number)) claims g2 accepts a function
; from anything to Number. The body then applies that function to a string,
; which pins $a to String for this clause only.
;
; A caller handing g2 an (-> Number Number) is checked against a fresh copy of
; the declaration and passes, and the string reaches the callee's arithmetic
; unguarded. If the body needs a String there, the declaration must say
; (-> (-> String Number) Number).
(: g2 (-> (-> $a Number) Number))
(= (g2 $f) ($f "hello"))

(: h2 (-> Number Number))
(= (h2 $x) (+ $x 1))

!(g2 h2)
