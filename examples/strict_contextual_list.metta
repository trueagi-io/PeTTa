!(import! &self (library lib_builtin_types))
!(import! &self (library lib_roman))

; Explicit list construction receives the declared element expectation.
(: nums (-> (List Number)))
(= (nums) (make-list 1 2 3))

!(test (nums) (1 2 3))

; Product elements receive their own positional expectations.
(: rows (-> (List (Number String))))
(= (rows)
   (make-list (data 1 "a")
              (data 2 "b")))

!(test (rows) ((1 "a") (2 "b")))

; The expectation also follows a cons chain and its empty tail.
(: cons-rows (-> (List (Number String))))
(= (cons-rows)
   (cons (data 3 "c")
         (cons (data 4 "d") ())))

!(test (cons-rows) ((3 "c") (4 "d")))

; A variable-headed expression is data, never contextual list syntax. In
; particular, its source head must survive rather than unify with make-list.
(: wrap-one (-> Number ((List Number) String)))
(= (wrap-one $n)
   (data ($n) "wrapped"))

!(test (wrap-one 7) ((7) "wrapped"))

; The same rule protects longer variable-headed products from unifying with
; contextual control-flow forms such as a three-element if.
(: wrap-three (-> Number Number Number ((Number Number Number) String)))
(= (wrap-three $a $b $c)
   (data ($a $b $c) "triple"))

!(test (wrap-three 1 2 3) ((1 2 3) "triple"))

; Atom elements stay as unevaluated source terms.
(: exprs (-> (List Atom)))
(= (exprs)
   (make-list (+ 1 2) foo ($x bar)))

!(test (exprs) (quote ((+ 1 2) foo ($x bar))))

; Result-position propagation through control flow is unchanged.
(: choose-nums (-> Bool (List Number)))
(= (choose-nums $first)
   (if $first
       (make-list 5 6)
       (cons 7 ())))

!(test (choose-nums True) (5 6))
!(test (choose-nums False) (7))

; A staged initializer learns (List Number) from fold-flat after the closure
; resolves its shared item type.
(: staged-sum (-> Number))
(= (staged-sum)
   (let* (($init (make-list 8 9))
          ($sum (fold-flat + 0 $init)))
     $sum))

!(test (staged-sum) 17)

; Bottom-up homogeneous inference works without a surrounding expectation.
!(test (make-list 10 11) (10 11))
