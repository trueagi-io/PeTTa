; A quoted atomic literal is still that literal: (quote "oops") is a String
; and cannot satisfy a declared Number output.
(: bad-quote (-> Number))
(= (bad-quote) (quote "oops"))
!(test (bad-quote) 0)
