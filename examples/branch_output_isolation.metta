; An alternative's result is connected to the caller only when it runs.
!(test (msort (collapse (superpose
         (1 (if (> 2 4) (let $v0 4 $v0) (let $v0 1 3))))))
       (1 3))
!(test (msort (collapse (superpose
         (1 (if (> 2 4) (let $v0 4 $v0) (let $v1 1 3))))))
       (1 3))

!(test (if False (let $x 4 $x) (let $x 1 3)) 3)
!(test (if True (let $x 1 3) (let $x 4 $x)) 3)
!(test (collapse (superpose
         ((let $x 1 3) (if False (let $x 4 $x)))))
       (3))
!(test (msort (collapse (superpose ((let $x 1 3) (let $x 4 $x)))))
       (3 4))
!(test (case b ((a (let $x 4 $x)) (b (let $x 1 3)))) 3)
!(test (case a ((a (let $x 1 3)) (Empty (let $x 4 $x)))) 3)
!(test (case (empty) ((a (let $x 4 $x)) (Empty (let $x 1 3)))) 3)

; Selected branches still share their bindings with the surrounding call.
!(test (let $answer (if False (let $x 4 $x) (let $x 1 3))
         (pair $answer $x))
       (pair 3 1))
!(test (msort (collapse (let $x (superpose (1 2))
         (if (== $x 1) (let $_ 0 $x) (+ $x 10)))))
       (1 12))

(= (branch-effect $which)
   (if $which
       (progn (add-atom &branch-output-effects (then)) (let $x 4 $x))
       (progn (add-atom &branch-output-effects (else)) (let $x 1 3))))
!(test (branch-effect False) 3)
!(test (collapse (match &branch-output-effects $row $row)) ((else)))
