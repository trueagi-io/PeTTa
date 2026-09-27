; == compares values and never binds: numbers by exact value at any depth,
; every other term as a term.
(= (from-above) (* 1.0e-200 1.0e-200))
(= (from-below) (* -1.0e-200 1.0e-200))
!(test (== (from-above) (from-below)) true)
!(test (== 0.0 -0.0) true)
!(test (== 1 1.0) true)
!(test (== 9007199254740992 9007199254740992.0) true)
!(test (== 9007199254740993 9007199254740992.0) false)
!(test (!= 1 2) true)
!(test (== 2.5 2.5) true)
!(test (== "1" 1) false)
!(test (== a a) true)

; Inside expressions the same rule applies to each element.
!(test (== (a 0.0) (a -0.0)) true)
!(test (== (1 2 3) (1.0 2.0 3.0)) true)
!(test (== (a 1) (a 2)) false)
!(test (== (a 1) (b 1)) false)
!(test (== (a 1 2) (a 1)) false)
(= (row $n $x) (if (== $n 0) () (cons-atom $x (row (- $n 1) $x))))
!(test (== (row 2000 1) (row 2000 1.0)) true)

; Variables compare by identity and are never bound.
!(test (== $x $x) true)
!(test (== $x $y) false)
!(test (let $_ (== (a 1) $x) (let $_ (translatePredicate (var $x)) true)) true)
!(test (let $_ (== 0.5 $x) (let $_ (translatePredicate (var $x)) true)) true)
!(test (== (a 1) ()) false)
!(test (== () (a 1)) false)
!(test (== 0.5 0.75) false)
!(test (!= 0.5 0.75) true)
!(test (let* (($_ (translatePredicate (put_attr $x same_value 42)))
               ($_ (== (a $x 1) (a $x 1.0))))
          (let $_ (translatePredicate (get_attr $x same_value 42)) true)) true)

; A NaN equals itself, as in SWI's ==, bare or inside an expression, and nothing else.
!(test (let $_ (translatePredicate (is $n nan)) (== $n $n)) true)
!(test (let $_ (translatePredicate (is $n nan)) (== (a $n) (a $n))) true)
!(test (let $_ (translatePredicate (is $n nan)) (== $n 1.0)) false)
!(test (let $_ (translatePredicate (is $n nan)) (!= $n $n)) false)

; Rationals compare exactly with floats.
!(test (let $_ (translatePredicate (set_prolog_flag prefer_rationals true)) (== (/ 1 2) 0.5)) true)
!(test (== (/ 1 3) 0.3333333333333333) false)
!(test (let $_ (translatePredicate (set_prolog_flag prefer_rationals false)) (/ 1 2)) 0.5)

; Cyclic terms compare by their unfolding, with the same rules, and the comparison ends.
!(test (let $_ (translatePredicate (= $c (f $c))) (== $c $c)) true)
!(test (let* (($_ (translatePredicate (= $c (f 1 $c)))) ($_ (translatePredicate (= $d (f 1.0 $d))))) (== $c $d)) true)
!(test (let* (($_ (translatePredicate (= $c (f 1 $c)))) ($_ (translatePredicate (= $d (f 1 (f 1 $d)))))) (== $c $d)) true)
!(test (let* (($_ (translatePredicate (= $c (f 1 $c)))) ($_ (translatePredicate (= $d (f 2 $d))))) (== $c $d)) false)
!(test (let* (($_ (translatePredicate (is $n nan))) ($_ (translatePredicate (= $c (f $n $c))))) (== $c $c)) true)
!(test (let* (($_ (translatePredicate (= $c (f (g $c))))) ($_ (translatePredicate (= $h (g (f $h))))) ($_ (translatePredicate (= $d (f $h))))) (== $c $d)) true)

; Terms that share subterms compare without unfolding the sharing.
(= (dag $n $leaf) (if (== $n 0) $leaf (let $t (dag (- $n 1) $leaf) (node $t $t))))
!(test (== (dag 40 1) (dag 40 1.0)) true)
!(test (== (dag 40 1) (dag 40 2)) false)
!(test (let $_ (translatePredicate (is $n nan)) (let $d (dag 40 $n) (== $d $d))) true)
!(test (let $_ (translatePredicate (is $n nan)) (== (pair (dag 40 1) $n) (pair (dag 40 1.0) $n))) true)

; The occurs_check flag does not change the answer: comparing creates no cycle by unification.
!(test (let* (($_ (translatePredicate (= $c (f 1 $c)))) ($_ (translatePredicate (= $d (f 1.0 $d)))) ($_ (translatePredicate (set_prolog_flag occurs_check true))) ($r (== $c $d)) ($_ (translatePredicate (set_prolog_flag occurs_check false)))) $r) true)
!(test (let* (($_ (translatePredicate (= $c (f 1 $c)))) ($_ (translatePredicate (set_prolog_flag occurs_check true))) ($r (== $c $c)) ($_ (translatePredicate (set_prolog_flag occurs_check false)))) $r) true)
!(test (let* (($_ (translatePredicate (= $c (f 1 $c)))) ($_ (translatePredicate (= $d (f 1.0 $d)))) ($_ (translatePredicate (set_prolog_flag occurs_check error))) ($r (== $c $d)) ($_ (translatePredicate (set_prolog_flag occurs_check false)))) $r) true)
!(test (let* (($_ (translatePredicate (= $c (f 1 $c)))) ($_ (translatePredicate (= $d (f 2 $d)))) ($_ (translatePredicate (set_prolog_flag occurs_check error))) ($r (== $c $d)) ($_ (translatePredicate (set_prolog_flag occurs_check false)))) $r) false)

; A zero-argument Prolog compound is a leaf, including in cyclic and shared terms.
!(test (let* (($_ (translatePredicate (compound_name_arguments $z z ()))) ($_ (translatePredicate (= $c (f $z $c))))) (== $c $c)) true)
!(test (let* (($_ (translatePredicate (compound_name_arguments $z z ()))) ($_ (translatePredicate (compound_name_arguments $q q ()))) ($_ (translatePredicate (= $c (f $z $c)))) ($_ (translatePredicate (= $d (f $q $d))))) (== $c $d)) false)
!(test (let $_ (translatePredicate (compound_name_arguments $z z ())) (let $xs (row 2000 $z) (== $xs $xs))) true)
