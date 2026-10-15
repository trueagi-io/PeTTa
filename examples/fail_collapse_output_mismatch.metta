!(import! &self (library lib_builtin_types))

(: a A)
(: aa (-> A A))
(= (aa a) a)

(: bad-collapse (-> A B))
(= (bad-collapse $x) (collapse (superpose ((aa $x)))))
