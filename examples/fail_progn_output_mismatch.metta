!(import! &self (library lib_builtin_types))

(: a A)
(: b B)

(: aa (-> A A))
(= (aa a) a)

(: bad-progn (-> A B))
(= (bad-progn $x) (progn (aa $x) (aa $x)))
