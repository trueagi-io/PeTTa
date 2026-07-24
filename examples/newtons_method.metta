!(import! &self (library lib_memo))

!(config-memoize (strategy wtinylfu) (unique-limit 100))

!(memoize energy)
(= (energy $x $n)
   (if (<= $n 0)
       (* $x $x)
       (+ (energy (+ (* 0.5 $x) 0.4) (- $n 1))
          (energy (+ (* 0.5 $x) 0.4) (- $n 1)))))

; Base case: n=0 returns x*x = 4.0
!(test (energy 2.0 0) 4.0)

; Recursive case: energy(2.0, 1) = 1.4*1.4 + 1.4*1.4 = 1.96 + 1.96 = 3.92
!(test (energy 2.0 1) 3.9199999999999995)
