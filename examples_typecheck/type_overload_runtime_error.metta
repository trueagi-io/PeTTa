!(import! &self (library lib_builtin_types))

(: f (-> Number Number))
(: f (-> String String))
(= (f $x) $x)

!(test
   (let* (($expr (quote (f True)))
          ($result (catch (eval $expr))))
     (car-atom $result))
   Error)

!(test
   (let* (($expr (quote (f True)))
          ($result (catch (eval $expr)))
          ($type (car-atom (cdr-atom $result))))
     (repr $type))
   "(no_matching_overload f)")
