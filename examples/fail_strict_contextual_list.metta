!(import! &self (library lib_builtin_types))

(: bad (-> (List Number)))
(= (bad) (make-list 1 "two"))
