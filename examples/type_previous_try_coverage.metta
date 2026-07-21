!(import! &self (library lib_he))
!(import! &self (library lib_builtin_types))

(: mp (-> Number Number Number))
(= (mp $a $b) (+ $a $b))

(: double (-> Number Number))
(= (double $x) (mp $x $x))

(: quadruple (-> Number Number))
(= (quadruple $x) (double (double $x)))

(: mid (-> $x $x))
(= (mid $x) $x)

(: doOp (-> (-> Number Number Number) Number Number Number))
(= (doOp $op $x $y) ($op $x $y))

(= (noType $x $y) (mp $x $y))

(: sid (-> String String))
(= (sid $x) $x)

(: both (-> Number String Bool))
(= (both $n $s) True)

; Successful chains/composition from previous try
!(test (double 7) 14)
!(test (quadruple 3) 12)
!(test (double (mid 7)) 14)
!(test (doOp + (mid 7) 8) 15)

; Rich typed mismatch errors
!(test
   (let* (($expr (quote (mp 1 "oops")))
          ($result (catch (eval $expr))))
         (car-atom $result))
   Error)
!(test
   (let* (($expr (quote (mp 1 "oops")))
          ($result (catch (eval $expr)))
          ($type (car-atom (cdr-atom $result))))
         (repr $type))
   "(literal_type_mismatch \"oops\" Number)")
!(test
   (let* (($expr (quote (double (mid "oops"))))
          ($result (catch (eval $expr))))
         (car-atom $result))
   Error)
!(test
   (let* (($expr (quote (double (mid "oops"))))
          ($result (catch (eval $expr)))
          ($type (car-atom (cdr-atom $result))))
         (repr $type))
   "(literal_type_mismatch \"oops\" Number)")
!(test
   (let* (($expr (quote (sid (noType 7 8))))
          ($result (catch (eval $expr))))
         (car-atom $result))
   Error)
!(test
   (let* (($expr (quote (sid (noType 7 8))))
          ($result (catch (eval $expr)))
          ($type (car-atom (cdr-atom $result))))
         (repr $type))
   "(literal_type_mismatch 15 String)")
!(test
   (collapse (map-atom (1) $x (both $x $x)))
   ())

; Explicit error-path checks (real runtime exceptions)
!(test (if-error (catch (/ 1 0)) True False) True)
!(test (let* (($expr (quote (/ 1 0)))
              ($result (catch (eval $expr))))
         (if-error $result True False))
       True)
