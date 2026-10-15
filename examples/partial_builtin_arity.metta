; A body that is a partial application of a builtin completes it with the
; builtin's arity. library(yall) also defines `/` with arities 4 to 9, so the
; arity used must not depend on the order the arities were registered in.

(= (div10) (/ 10))
(= (inc) (+ 1))

!(test (div10 2)
       5)
!(test (inc 2)
       3)
