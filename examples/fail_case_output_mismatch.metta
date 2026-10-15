!(import! &self (library lib_builtin_types))

(: a A)
(: b B)

(: aa (-> A A))
(= (aa a) a)

(: bad-case (-> A B))
(= (bad-case $x)
   (case $x
     ((a (aa $x))
      (Empty b))))
