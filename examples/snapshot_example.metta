!(import! &self (library lib_snapshot))

;Functions as usual:
(= (fib-tr $n $a $b)
   (if (== $n 0)
       $a
       (fib-tr (- $n 1) $b (+ $a $b))))

(= (fib $n)
   (fib-tr $n 0 1))

;Start computation:
!(CallWithSnapshots (fib 1000) 300)
