!(import! &self (library lib_he))
!(import! &self (library lib_builtin_types))

; Metatype vs concrete behavior
(: expect-expression (-> Expression Bool))
(= (expect-expression $x) True)

(: to-num (-> Number Number))
(= (to-num $x) (+ $x 1))

!(test (expect-expression (+ 1 2)) True)
!(test (to-num (+ 1 2)) 4)

; Expression checks should not duplicate success branches
!(test (collapse (expect-expression (+ 1 2))) (True))

; Partial function values should still enforce typed argument checks

!(test
   (let* (($expr (quote ((+ 2) "oops")))
          ($result (catch (eval $expr))))
         (car-atom $result))
   Error)

!(test
   (let* (($expr (quote ((+ 2) "oops")))
          ($result (catch (eval $expr)))
          ($type (car-atom (cdr-atom $result))))
         (repr $type))
   "(literal_type_mismatch \"oops\" Number)")

!(test
   (let* (($expr (quote ((+ 2) "oops")))
          ($result (catch (eval $expr)))
          ($ctx (car-atom (cdr-atom (cdr-atom $result)))))
         $ctx)
   typecheck)

; Explicit variable-context conflict should surface rich type_conflict
(: both (-> Number String Bool))
(= (both $n $s) True)

!(test (collapse (map-atom (1) $x (both $x $x))) ())
