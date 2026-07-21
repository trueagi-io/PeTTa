!(import! &self (library lib_builtin_types))

(: a A)
(: aa (-> A A))
(= (aa a) a)

(: bad-once (-> A B))
(= (bad-once $x) (once (aa $x)))
