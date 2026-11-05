; _ is the checking alias of %Undefined%, including for computed expressions.
(: underscore-template (-> Number _))
(= (underscore-template $x) (quote (+ $x 1)))
(: undefined-template (-> Number %Undefined%))
(= (undefined-template $x) (quote (+ $x 1)))
!(test (underscore-template 4) (quote (+ 4 1)))
!(test (underscore-template 4) (undefined-template 4))

; It imposes no membership requirement on an input, but evaluates that input.
(: underscore-input (-> _ Number))
(= (underscore-input $x) 7)
!(test (underscore-input (quote (data 1))) 7)
(: underscore-identity (-> _ _))
(= (underscore-identity $x) $x)
!(test (underscore-identity (+ 1 2)) 3)

; A committed function can compute a result without promising a narrower type.
(: underscore-committed ( -[det]-> Number _))
(= (underscore-committed $x) (quote (result $x)))
!(test (underscore-committed 5) (quote (result 5)))

; Ordinary explicit output requirements remain enforced.
(: underscore-number (-> Number Number))
(= (underscore-number $x) (+ $x 1))
!(test (underscore-number 4) 5)

; Equivalent spellings share a declaration, rather than multiplying answers.
(: underscore-both (-> _ Number))
(: underscore-both (-> %Undefined% Number))
(= (underscore-both $x) 7)
!(test (collapse (underscore-both "ok")) (7))
