(: f (-> Number %Undefined%))
(= (f $x) (+ $x 42))

(: g (-> Number Atom))
(= (g $x) (+ $x 42))

(: h (-> Atom Atom))
(= (h $x) (+ $x 42))

!(test (f (+ 1 1)) 44)
!(test (g (+ 1 1)) (quote (+ 2 42)))
!(test (h (+ 1 1)) (quote (+ (+ 1 1) 42)))
