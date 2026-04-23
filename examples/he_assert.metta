!(import! &self lib/lib_he)

!(test (assertEqual (+ 1 2) (- 6 3)) True)
!(test (assertAlphaEqual (h $x $y) (h $a $b)) True)
!(test (assertAlphaEqual (quote (+ $x $y)) (quote (+ $a $b))) True)

;TODO:
;!(assertEqualToResult (+ 1 2) 3)
;(= (adder) $x)
;!(assertAlphaEqualToResult (adder) ($y))
