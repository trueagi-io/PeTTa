; A recursive call that passes its higher-order argument on unchanged keeps
; calling the specialization, so the function passed in stays a direct call
; and declared argument checks are preserved on every recursive step.
(: a A)
(: b B)
(: only-a (-> A %Undefined%))
(= (only-a $x) (ok $x))
(= (each $f ()) ())
(= (each $f ($x $rest)) (($f $x) (each $f $rest)))
!(test (collapse (each only-a (a (a ())))) (((ok a) ((ok a) ()))))
; Since #209, declared calls use static checks and throwing residual checks,
; rather than the old failing get-type guards. car-atom leaves the argument
; unknown here, so the direct inc call needs a residual Number check.
(: inc (-> Number Number))
(= (inc $x) (+ $x 1))
(= (mymap $f ()) ())
(= (mymap $f ($x $r)) (($f (car-atom ($x))) (mymap $f $r)))
!(test (collapse (mymap inc (1 (2 ())))) ((2 (3 ()))))
; Error details are Prolog compounds; repr compares the full MeTTa form.
!(test (repr (catch (collapse (mymap inc (x (1 ()))))))
       "(Error (literal_type_mismatch x Number) typecheck)")
; On main without recursive specialization reuse, the second step falls back
; to generic mymap and reports a raw type_error evaluable instead.
!(test (repr (catch (collapse (mymap inc (1 (x ()))))))
       "(Error (literal_type_mismatch x Number) typecheck)")

; Reusing compiled recursion preserves the order and multiplicity of answers.
(= (fork $x) (+ $x 1))
(= (fork $x) (+ $x 1))
(= (fork $x) (+ $x 10))
(= (repeat $f $n $x) (if (== $n 0) $x (repeat $f (- $n 1) ($f $x))))
!(test (collapse (repeat fork 2 0)) (2 2 11 2 2 11 11 11 20))

; Rollback of failed specialization cycles is covered by
; tests/test_specializer_recursion.pl with an injected translation error.
