!(import! &self (library lib_builtin_types))

(: a A)
(: aa (-> A A))
(= (aa a) a)

(: bad-match (-> A B))
(= (bad-match $x) (match &self (: aa (-> A A)) (aa $x)))
