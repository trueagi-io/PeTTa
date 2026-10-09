!(import! &self (library lib_builtin_types))

(: a A)
(: aa (-> A A))
(= (aa a) a)

(: bad-sealed (-> A B))
(= (bad-sealed $x) (sealed ($x) (aa $x)))
