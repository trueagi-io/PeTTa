; An expression head has already been evaluated, including any quote in it.
!(test (qh_data ((quote (> 2 1)))) (quote (qh_data ((> 2 1)))))
!(test ((quote (> 2 1)) (+ 1 2)) (quote ((> 2 1) 3)))
!(test (((quote (+ 1 2)))) (quote (((+ 1 2)))))
!(test ((quote ()) (+ 1 2)) (quote (() 3)))
!(test ((quote 9) (+ 1 2)) (quote (9 3)))

; Ordinary nested expressions and callable heads still evaluate once.
!(test ((qh_tag (+ 1 2)) (+ 3 4)) (quote ((qh_tag 3) 7)))
!(test ((quote +) 2 3) 5)
!(test (let $h (quote (> 2 1)) (qh_data ($h))) (quote (qh_data ((> 2 1)))))
!(test (let $code (quote (> 2 1)) (eval $code)) True)

; Quoted code does not run effects or enumerate alternatives as data heads.
(= (qh_touch) (let $_ (add-atom &self (qh_ran)) 42))
!(test (qh_data ((quote (qh_touch)))) (quote (qh_data ((qh_touch)))))
!(test (collapse (match &self (qh_ran) ran)) ())
!(test (qh_data ((quote (superpose (1 2)))))
       (quote (qh_data ((superpose (1 2))))))
!(test (qh_data ((quote (/ 1 0)))) (quote (qh_data ((/ 1 0)))))

; Quotation retains logic-variable identity for later bindings.
!(test (let $r ((quote (qh_pair $x))) (let $x 5 $r)) (quote ((qh_pair 5))))

; Malformed case syntax keeps main's existing data fallback and warning.
!(test (case (+ 1 2)) (quote (case 3)))
