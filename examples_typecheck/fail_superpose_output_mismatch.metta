!(import! &self (library lib_builtin_types))

(: a A)
(: b B)
(: aa (-> A A))
(= (aa a) a)

(: bad-super (-> A B))
(= (bad-super $x) (superpose ((aa $x) b)))
