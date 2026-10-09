!(import! &self (library lib_roman))

(: roman-inc (-[det]-> Number Number))
(= (roman-inc $x) (+ $x 1))

(: roman-choice (-[nondet]-> Number Number))
(= (roman-choice $x) (superpose ($x (+ $x 10))))

(: roman-add (-[det]-> Number Number Number))
(= (roman-add $acc $x) (+ $acc $x))

(: roman-choice-add (-[nondet]-> Number Number Number))
(= (roman-choice-add $acc $x) (superpose ($acc (+ $acc $x))))

(: roman-choice-radd (-[nondet]-> Number Number Number))
(= (roman-choice-radd $x $acc) (superpose ($acc (+ $x $acc))))

(: roman-pair-sum (-[det]-> Number Number Number))
(= (roman-pair-sum $x $y) (+ $x $y))

(: roman-choice-pair-sum (-[nondet]-> Number Number Number))
(= (roman-choice-pair-sum $x $y) (superpose ($x (+ $x $y))))

(: roman-choice-bool (-[nondet]-> Number Number Bool))
(= (roman-choice-bool $_x $_y) (superpose (True False)))

!(test (map-flat roman-inc (1 2 3)) (2 3 4))
!(test (collapse (map-flat roman-choice (1 2)))
       ((1 2) (1 12) (11 2) (11 12)))

!(test (map-nested roman-inc (1 (2))) (2 (3)))
!(test (collapse (map-nested roman-choice (1)))
       ((1) (11)))

!(test (fold-flat roman-add 0 (1 2 3)) 6)
!(test (collapse (fold-flat roman-choice-add 0 (1)))
       (0 1))

!(test (foldr-flat roman-add 0 (1 2 3)) 6)
!(test (collapse (foldr-flat roman-choice-radd 0 (1)))
       (0 1))

!(test (fold-nested roman-add 0 ((1))) 1)
!(test (collapse (fold-nested roman-choice-add 0 ((1))))
       (0 1))

!(test (/?\ = (1 2) (2 3)) (2))
!(test (collapse (/?\ roman-choice-bool (1) (2))) ((1) ()))
!(test (\? = (1 2) (2 3)) (1))
!(test (collapse (\? roman-choice-bool (1) (2))) (() (1)))
!(test (\?/ = (1) (2)) (1 2))
!(test (collapse (\?/ roman-choice-bool (1) (2))) ((2) (1 2)))

!(test (. roman-inc roman-inc 1) 3)
!(test (collapse (. roman-choice roman-inc 1)) (2 12))
!(test (.. roman-inc roman-inc 1) 3)
!(test (collapse (.. roman-choice roman-inc 1)) (2 12))
!(test (.: roman-inc roman-pair-sum 1 2) 4)
!(test (collapse (.: roman-inc roman-choice-pair-sum 1 2)) (2 4))
!(test (&&& roman-inc roman-inc 1) (2 2))
!(test (collapse (&&& roman-choice roman-inc 1)) ((1 2) (11 2)))
