!(import! &self (library lib_patrick))

(: fib-step (-[det]-> Number (Number Number) (Number Number)))
(= (fib-step $i ($a $b))
   ($b (+ $a $b)))

(= (fib $n)
   (first (iterate 0 $n (0 1) fib-step)))

!(test (fib 100) 354224848179261915075)
