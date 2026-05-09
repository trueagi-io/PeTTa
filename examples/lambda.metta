;fake lambda (works in H-E MeTTa too):
(: apply (-> Atom %Undefined% %Undefined%))
(= (apply (lambda $var $body) $arg)
   (eval (let $var $arg $body)))

(= (applyL1)
   (apply (lambda $x (+ $x 1)) 2))

(= (applyL2)
   (apply (lambda ($x $y) (+ $x $y)) (2 7)))

!(test (applyL1) 3)
!(test (applyL2) 9)

;Proper lambdas that act as first-class compiled functions:
!(test (maplist (|-> ($a) (+ 1 $a)) (1 2 3))
       (2 3 4))

!(test ((|-> ($acc $e) (or (== 1 $e) $acc)) False 1)
       True)

(= (myfunc $a $b) (cons $a $b))

!(test (let $f (myfunc 42)
            ((|-> ($x) ($f ($x 2 3))) 43))
       (42 43 2 3))

!(test (((|-> ($x $y) (42 $x $y)) 43) 44)
       (42 43 44))

(= (myfunc2 $mylambda)
   ($mylambda 43 44))

!(test (let* (($k 45)
              ($lambda (|-> ($x $y) (42 $x $y $k))))
             (myfunc2 $lambda))
       (42 43 44 45))
