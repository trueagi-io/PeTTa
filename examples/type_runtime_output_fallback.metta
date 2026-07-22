!(import! &self (library lib_builtin_types))
!(import! &self (library lib_he))

(: a A)
(: b B)
(: aa (-> A A))
(= (aa a) a)

; Dynamic form: compile-time cannot guarantee output type, runtime check must enforce it.
(: bad-catch (-> A B))
(= (bad-catch $x) (catch (aa $x)))

(: bad-filter (-> A B))
(= (bad-filter $x) (filter-atom (1) $y (aa $x)))

(: bad-foldl (-> A B))
(= (bad-foldl $x) (foldl-atom (1) b $acc $y (aa $x)))

; the accumulator function is unknown here, so the output stays a runtime check
; (foldall with a known accumulator is typed statically - see
; fail_foldall_output_mismatch.metta)
(: bad-foldall (-> A B))
(= (bad-foldall $x) (foldall $x (superpose (1 2)) 0))

!(test
   (let* (($expr (quote (bad-catch a)))
          ($result (catch (eval $expr))))
     (car-atom $result))
   Error)

!(test
   (let* (($expr (quote (bad-filter a)))
          ($result (catch (eval $expr))))
     (car-atom $result))
   Error)

!(test
   (let* (($expr (quote (bad-foldl a)))
          ($result (catch (eval $expr))))
     (car-atom $result))
   Error)

!(test
   (let* (($expr (quote (bad-foldall a)))
          ($result (catch (eval $expr))))
     (car-atom $result))
   Error)
