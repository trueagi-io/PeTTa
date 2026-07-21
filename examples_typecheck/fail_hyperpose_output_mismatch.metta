!(import! &self (library lib_builtin_types))

(: a A)
(: aa (-> A A))
(= (aa a) a)

(: bad-hyper (-> A B))
(= (bad-hyper $x) (hyperpose ((aa $x))))
