!(import! &self (library lib_builtin_types))

(: a A)
(: b B)

(: f (-> A B))
(= (f a) b)

(: f2 (-> A A))
(= (f2 $a) (f $a))
