!(import! &self (library lib_he))

!(test (if-equal 1 1 yes no) yes)

(: he-inc (-[det]-> Number Number))
(= (he-inc $x) (+ $x 1))

(: he-choice (-[nondet]-> Number Number))
(= (he-choice $x) (superpose ($x (+ $x 10))))

(: he-each-det (-[det]-> (List Number) %Undefined%))
(= (he-each-det $xs) (for-each-in-atom $xs he-inc))

(: he-each-nondet (-[nondet]-> (List Number) %Undefined%))
(= (he-each-nondet $xs) (for-each-in-atom $xs he-choice))

!(test (he-each-det (1 2)) (2 3))
!(test (collapse (he-each-nondet (1 2)))
       ((1 2) (1 12) (11 2) (11 12)))
