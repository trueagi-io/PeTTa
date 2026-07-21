!(import! &self (library lib_roman))

(: takes-number-list (-> (List Number) Bool))
(= (takes-number-list $xs) True)

!(takes-number-list (1 "oops" 3))
