!(import! &self (library lib_builtin_types))

(: a A)
(: aa (-> A A))
(= (aa a) a)

(: bad-call (-> A B))
(= (bad-call $x) (call (aa $x)))
