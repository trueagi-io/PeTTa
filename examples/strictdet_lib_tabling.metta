!(import! &self (library lib_tabling))

!(tabled (strictdet_tabled $x))
(= (strictdet_tabled a) 1)
!(test (strictdet_tabled a) 1)
