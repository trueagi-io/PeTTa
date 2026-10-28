!(import! &self ../lib/lib_he)

!(test (assertEqual (+ 1 2) (- 6 3)) True)
!(test (assertAlphaEqual (h $x $y) (h $a $b)) True)
!(test (assertAlphaEqual (quote (+ $x $y)) (quote (+ $a $b))) True)

!(test (assertEqualToResult (+ 1 2) (3)) True)

; Compare all answers as a multiset, preserving duplicate answers.
(assert-answer 1)
(assert-answer 2)
!(test (assertEqualToResult (match &self (assert-answer $x) $x) (2 1)) True)
!(test (assertEqualToResult (superpose (1 1 2)) (2 1 1)) True)
!(test (assertEqualToResult (empty) ()) True)

; Expected expressions are literal data, including calls with side effects.
!(test (assertEqualToResult (quote (+ 1 2)) ((+ 1 2))) True)
!(test (assertEqualToResult (quote (println! "literal")) ((println! "literal"))) True)

(= (adder) ($x))
!(test (assertAlphaEqualToResult (adder) ($y)) True)
