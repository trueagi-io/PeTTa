; A quoted literal is just that literal: (quote "oops") is a String and
; cannot satisfy a declared Number output. (Quoted CODE stays exempt.)
(: bad-quote (-> Number))
(= (bad-quote) (quote "oops"))
!(test (bad-quote) 0)
