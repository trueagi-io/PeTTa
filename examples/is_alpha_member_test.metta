;;; Test file for member_alpha and is-alpha-member predicates  
;;; This file tests edge cases for alpha-equivalence membership checking  
  
;;; Test 1: Empty list should return false  
!(test (is-alpha-member x ()) false)  
  
;;; Test 2: Variable in list of atoms should not match  
!(test (is-alpha-member $x (a b c)) false)  
  
;;; Test 3: Ground term membership  
!(test (is-alpha-member a (a b c)) true)  
!(test (is-alpha-member d (a b c)) false)  
  
;;; Test 4: Alpha-equivalence with variables  
!(test (is-alpha-member (f $x) ((f $y) (g $z)) ) true)  
!(test (is-alpha-member (f $x) ((f $y) (f $y)) ) true)  
  
;;; Test 5: Complex nested structures  
!(test (is-alpha-member (f (g $x) $y) ((f (g $a) $b) (h $c $d)) ) true)  
!(test (is-alpha-member (f (g $x) $x) ((f (g $a) $b) (f (g $c) $c)) ) true)  
  
;;; Test 6: Different arities should fail  
!(test (is-alpha-member (f $x) ((f $x $y) (g $z)) ) false)  
  
;;; Test 7: Numbers and atoms  
!(test (is-alpha-member 42 (1 2 42 3) ) true)  
!(test (is-alpha-member 99 (1 2 42 3) ) false)  
  
;;; Test 8: Nested lists  
!(test (is-alpha-member (1 $x) ((1 2) (3 4)) ) true)  
!(test (is-alpha-member (1 $x) ((2 3) (4 5)) ) false)  
  
;;; Test 9: Multiple occurrences  
!(test (is-alpha-member a (a b a c)) true)  
  
;;; Test 10: Complex terms with same structure but different variables  
!(test (is-alpha-member (f $x $y) ((f $a $b) (f $c $d)) ) true)  
  
;;; Test 11: Single element list  
!(test (is-alpha-member a (a)) true)  
!(test (is-alpha-member b (a)) false)  
  
;;; Test 12: List with variables as elements  
!(test (is-alpha-member $x ($y $z $w)) true)  
  
;;; Test 13: Deeply nested structures  
!(test (is-alpha-member (a (b (c $x))) ((a (b (c $d))) (e $f)) ) true)  
  
;;; Test 14: Terms with different functors  
!(test (is-alpha-member (f $x) ((g $y) (h $z)) ) false)  
  
;;; Test 15: Empty complex terms  
!(test (is-alpha-member () (() a b) ) true)  
!(test (is-alpha-member () (a b c) ) false)

!(let*
    (
        ($pat (hi name boss))
        ($dummy1 (println! (pattern:- $pat)))
        ($bool (is-alpha-member $new $pat))
        ($dummy2 (println! (is member:- $bool in pattern:- $pat)))
    )
    ()
)