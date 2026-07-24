!(import! &self (library lib_builtin_types))

(: a A)
(: aa (-> A A))
(= (aa a) a)

(: bad-reduce (-> A B))
(= (bad-reduce $x) (reduce (aa $x)))
