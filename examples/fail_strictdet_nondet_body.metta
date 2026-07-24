; Under --strict-det a plain -> claims determinism; a superpose body
; contradicts it. Either fix the function or declare it -[nondet]->.
(: sneaky (-> Number Number))
(= (sneaky $n) (superpose (1 $n)))
!(test (collapse (sneaky 5)) (1 5))
