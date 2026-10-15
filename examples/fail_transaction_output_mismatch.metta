!(import! &self (library lib_builtin_types))

(: a A)
(: aa (-> A A))
(= (aa a) a)

(: bad-transaction (-> A B))
(= (bad-transaction $x) (transaction (aa $x)))
