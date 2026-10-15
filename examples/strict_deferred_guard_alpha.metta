; Strict mode permits an explicit `the` guard. Its nested requirements remain
; live until binding, while checker metadata stays outside MeTTa identity.
!(test
   (let* (($expr
             (quote
               (let $typed (the (List Number) ($x))
                    (let "not-a-number" $x $typed))))
          ($caught (catch (eval $expr))))
         (car-atom $caught))
   Error)

!(test
   (let $typed (the (List Number) ($x))
        (let 7 $x $typed))
   (7))

!(test
   (let $typed (the (List Number) ($x))
        (=alpha $typed ($fresh)))
   True)
