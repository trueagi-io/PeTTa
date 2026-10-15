!(import! &self (library lib_builtin_types))

(: inc (-> Number Number))
(= (inc $x) (+ $x 1))

!(test (inc 2) 3)
