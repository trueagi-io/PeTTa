!(import! &self (library lib_builtin_types))
!(import! &self (library lib_roman))

(: takes-number-list (-> (List Number) Bool))
(= (takes-number-list $xs) True)

!(test (takes-number-list (1 2 3)) True)
!(test (map-flat (+ 1) (1 2 3)) (2 3 4))
