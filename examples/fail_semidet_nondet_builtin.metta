; -[semidet]-> allows zero or one result, never several: the same unanalysed
; builtin must not certify it either.
(: pick (-[semidet]-> Number))
(= (pick) (let $a (get-atoms &self) 5))

!(test (collapse (pick)) (5))
