!(import! &self (library lib_builtin_types))

; Pattern variables acquire types from the declared relation schema, so match
; results are typed and this file compiles under --strict with no guards.
(: age (-> String Number Atom))
(age "bob" 42)
(age "alice" 37)

(: next-age (-> String Number))
(= (next-age $who) (match &self (age $who $n) (+ $n 1)))

!(test (next-age "bob") 43)

; Direct type queries bind the variable's type from the pattern itself,
; and the result type flows through collapse to the declared output.
(: Fruit Type)
(: apple Fruit)
(: banana Fruit)

(: fruits (-> (List Fruit)))
(= (fruits) (collapse (match &self (: $x Fruit) $x)))

!(test (fruits) (apple banana))
