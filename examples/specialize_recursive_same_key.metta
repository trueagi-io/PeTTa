; A recursive call that passes its higher-order argument on unchanged keeps
; calling the specialization, so the function passed in stays a direct call
; and its declaration is followed on every step, not only on the first.
(: a A)
(: b B)
(: only-a (-> A %Undefined%))
(= (only-a $x) (ok $x))
(= (each $f ()) ())
(= (each $f ($x $rest)) (($f $x) (each $f $rest)))
!(test (collapse (each only-a (a (a ())))) (((ok a) ((ok a) ()))))
!(test (collapse (each only-a (b (a ())))) ())
!(test (collapse (each only-a (a (b ())))) ())
; The same when the recursion goes through a second function.
(= (evens $f ()) ())
(= (evens $f ($x $rest)) (($f $x) (odds $f $rest)))
(= (odds $f ()) ())
(= (odds $f ($x $rest)) ((skip $x) (evens $f $rest)))
!(test (collapse (evens only-a (a (b (b ()))))) ())

; Reusing compiled recursion preserves the order and multiplicity of answers.
(= (fork $x) (+ $x 1))
(= (fork $x) (+ $x 1))
(= (fork $x) (+ $x 10))
(= (repeat $f $n $x) (if (== $n 0) $x (repeat $f (- $n 1) ($f $x))))
!(test (collapse (repeat fork 2 0)) (2 2 11 2 2 11 11 11 20))

; A failed outer specialization must also discard a nested specialization
; that calls it. Both calls below must fall back to the original equations.
(: broken Bad)
(= (broken $x) $x)
(= (outer $f $x) (pair (inner $f $x) ($f $x)))
(= (inner $f $x) (if (== $x 0) done (outer $f 0)))
!(test (collapse (outer broken 1)) ((pair (pair done 0) 1)))
!(test (collapse (inner broken 1)) ((pair done 0)))
