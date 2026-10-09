; Quoted code is an Expression value; it does not inherit the output type of
; the call syntax inside the quote.
(: bad-quote (-[det]-> Number))
(= (bad-quote) (quote (+ 1 2)))
