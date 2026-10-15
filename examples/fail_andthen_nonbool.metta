; and-then/or-else are Boolean operators: a literal non-Bool operand is a
; compile error rather than a mistyped Bool-certified result.
(: bad-and (-> Bool))
(= (bad-and) (and-then True 42))
!(test (bad-and) True)
