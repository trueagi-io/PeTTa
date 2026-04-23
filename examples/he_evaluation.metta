!(import! &self lib/lib_he)

(= (double $x) (+ $x $x))

!(test (eval (double 5)) 10)

!(test (evalc (+ 5 5) &self) 10)

!(test (chain (+ 2 3) $x (* $x 2)) 10)

!(test (for-each-in-atom (1 3 5 62 2 5) println!)
       (true true true true true true))
