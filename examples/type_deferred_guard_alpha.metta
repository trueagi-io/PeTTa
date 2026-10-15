!(import! &self (library lib_he))

(: guarded-list-id (-> (List Number) (List Number)))
(= (guarded-list-id $xs) $xs)

; A nested variable retains the element requirement until it is bound.
!(test
   (let* (($expr
             (quote
               (let $result (guarded-list-id ($x))
                    (let "not-a-number" $x $result))))
          ($caught (catch (eval $expr))))
         (car-atom $caught))
   Error)

; A later binding of the required type still succeeds.
!(test
   (let $result (guarded-list-id ($x))
        (let 7 $x $result))
   (7))

; Checker attributes are metadata, not part of MeTTa alpha-identity.
!(test
   (let $typed (guarded-list-id ($x))
        (=alpha $typed ($fresh)))
   True)
