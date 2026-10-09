!(import! &self (library lib_builtin_types))

(: a A)
(: aa (-> A A))
(= (aa a) a)

(: bad-map (-> A B))
(= (bad-map $x) (map-atom (1) $y (aa $x)))
