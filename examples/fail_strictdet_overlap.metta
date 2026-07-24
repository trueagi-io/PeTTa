; Under --strict-det overlapping clause heads on a -> function are a compile
; error: in MeTTa every matching clause fires, so (f 0) would yield two
; results. Disambiguate the heads or declare the function -[nondet]->.
(: f (-> Number Number))
(= (f 0) 100)
(= (f $n) $n)
!(test (f 0) 100)
