!(import! &self (library lib_builtin_types))

(: a A)
(: aa (-> A A))
(= (aa a) a)

(: bad-evalq (-> A B))
(= (bad-evalq $x) (eval (quote (aa a))))
