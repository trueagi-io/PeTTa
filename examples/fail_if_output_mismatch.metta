!(import! &self (library lib_builtin_types))

(: a A)
(: b B)

(: aa (-> A A))
(= (aa a) a)

(: bad-if (-> A B))
(= (bad-if $x) (if True (aa $x) b))
