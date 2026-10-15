(= (fib $N)
   (if (< $N 2)
       $N
       (+ (fib (- $N 1))
          (fib (- $N 3)))))

!(test (fib 10) 55)
