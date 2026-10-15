!(import! &self (library lib_builtin_types))

(: a A)
(: aa (-> A A))
(= (aa a) a)

(: bad-forall (-> A B))
(= (bad-forall $x) (forall (superpose ((aa $x))) (aa $x)))
