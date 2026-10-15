(: a A)
(: b B)
(: only-a (-> A %Undefined%))
(= (only-a $x) (ok $x))

; An untyped positional head pattern gives no evidence about its fields.
(= (h ($x)) (only-a $x))
!(test (collapse (h (a))) ((ok a)))
!(test (repr (catch (h (b)))) "(Error (literal_type_mismatch b A) typecheck)")

; The same obligation survives nested destructuring.
(= (nested (($x))) (only-a $x))
!(test (collapse (nested ((a)))) ((ok a)))
!(test (repr (catch (nested ((b))))) "(Error (literal_type_mismatch b A) typecheck)")

; Plain-parameter inference moves the requirement to the caller; unresolved
; values there use the same residual error as destructured values.
(= (g $x) (only-a $x))
!(test (g a) (ok a))
!(test (repr (catch (g (car-atom (b))))) "(Error (literal_type_mismatch b A) typecheck)")

; A lost shape's guard stays in the branch that consumes the field. Moving it
; to clause entry would incorrectly reject the sentinel branch below.
(= (maybe-h ($x)) (if (== $x b) skipped (only-a $x)))
!(test (maybe-h (b)) skipped)
!(test (maybe-h (a)) (ok a))

; A required list element is still checked when its head shape is lost.
(: list-a (-> (List A) %Undefined%))
(= (list-a $xs) (ok-list $xs))
(= (list-h ($x)) (list-a ($x)))
!(test (list-h (a)) (ok-list (a)))
!(test (repr (catch (list-h (x))))
       "(Error (literal_type_mismatch (x) (List A)) typecheck)")
