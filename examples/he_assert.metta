!(import! &self ../lib/lib_he)

!(test (assertEqual (+ 1 2) (- 6 3)) True)
!(test (assertAlphaEqual (h $x $y) (h $a $b)) True)
!(test (assertAlphaEqual (quote (+ $x $y)) (quote (+ $a $b))) True)

!(test (assertEqualToResult (+ 1 2) (3)) True)
(= (adder) ($x))
!(test (assertAlphaEqualToResult (adder) ($y)) True)

; Compare the complete set of results.
(a 1)
(a 2)
!(test (assertEqualToResult (match &self (a $x) $x) (1 2)) True)

; A single result is compared as a one-element result set.
!(test (assertEqualToResult (+ 1 2) (3)) True)

; Result ordering should not affect comparison, if that matches Hyperon semantics.
!(test (assertEqualToResult (match &self (a $x) $x) (2 1)) True)
