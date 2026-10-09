;Range number generator, separate non-det return of each item:
(: range (-[nondet]-> Number Number Number))
(= (range $K $N)
   (if (< $K $N)
       (superpose ($K (range (+ $K 1) $N)))
       (empty)))

;Combine two elements of $L uniquely e.g. (a b) but not (b a):
(: choose2 (-[nondet]-> (List $a) ($a $a)))
(= (choose2 $L)
   (let $j (range 0 (length $L))
           (let $i (range 0 $j)
                (data (index-atom $L $i) (index-atom $L $j)))))
(: choose2l (-[det]-> (List $a) (List ($a $a))))
(= (choose2l $L)
   (collapse (choose2 $L)))

; choose exactly k elements, unordered
(: chooseKl (-[nondet]-> (List $a) Number (List (List $a))))
(= (chooseKl $L 0) (let $temp (cut) (())))
(= (chooseKl () $k) (let $temp (cut) ()))
(= (chooseKl (cons $h $t) $k)
   (the (List (List $a))
        (append (map-atom (chooseKl $t (- $k 1)) (|-> ($c) (cons $h $c)))
                (chooseKl $t $k))))
(: chooseK (-[nondet]-> (List $a) Number (List $a)))
(= (chooseK $L $k)
   (let $options (chooseKl $L $k) (superpose $options)))


;Take the first K items from the list:
(: takeK (-[det]-> Number (List $a) (List $a)))
(= (takeK $k ()) ())
(= (takeK $k (cons $head $tail))
   (if (> $k 0)
       (cons $head (takeK (- $k 1) $tail))
       ()))
