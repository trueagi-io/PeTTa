!(import! &self (library lib_builtin_types))

(: a A)
(: aa (-> A A))
(= (aa a) a)

(: bad-mutex (-> A B))
(= (bad-mutex $x) (with_mutex m (aa $x)))
