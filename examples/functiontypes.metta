(: wu1 (-> Number Atom Atom))
(= (wu1 $a $b)
   (42 $a $b))

(: wu2 (-> Number Number Number))
(= (wu2 $a $b)
   (+ $a $b))

(: wu3 (-> Number Number %Undefined%))
(= (wu3 $a $b)
   (if (< $a 10)
       (+ $a $b)
       (a list not a number)))

!(test (wu1 (+ 2 4) (+ 4 2)) (quote (42 6 (+ 4 2))))

!(test (wu2 (+ 2 4) (+ 4 2)) 12)

!(test (wu3 42 0) (a list not a number))

!(test (wu3 2 0) 2)
