!(import! &self (library lib_builtin_types))

(: f (-> Number Number))
(: f (-> String String))
(= (f $x) $x)

!(test (f 1) 1)
