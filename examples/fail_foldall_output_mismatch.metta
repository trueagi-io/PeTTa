!(import! &self (library lib_builtin_types))

(: a A)
(: b B)

; foldall's result carries the accumulator function's output type, so a
; declared output that can never hold is a compile-time error.
(: bad-foldall (-> A B))
(= (bad-foldall $x) (foldall + (superpose (1 2)) 0))
