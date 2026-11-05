; With one fixed signature, a partial call checks inputs, not the eventual result.
(: part-first (-> Number String Number))
(= (part-first $n $s) $n)
!(test ((part-first 1) "ok") 1)
!(test (collapse ((part-first 1) 2)) ())
!(test (collapse ((part-first "bad") "ok")) ())

; Complete parametric calls retain their existing checks.
(: part-same (-> $a $a Bool))
(= (part-same $x $y) True)
!(test (part-same 1 2) True)
!(test (collapse (part-same 1 "bad")) ())

; Captured evaluated values must not run again when the call is completed.
(= (part-syntax) (quote (/ 1 0)))
(: part-echo (-> %Undefined% Number %Undefined%))
(= (part-echo $x $n) $x)
!(test ((part-echo (part-syntax)) 1) (quote (/ 1 0)))

; Held inputs stay held, including inputs supplied in the final group.
(: part-held (-> Atom Number %Undefined%))
(= (part-held $x $n) $x)
!(test ((part-held (/ 1 0)) 1) (quote (/ 1 0)))
(: part-last-held (-> Number Atom %Undefined%))
(= (part-last-held $n $x) $x)
!(test ((part-last-held 1) (/ 1 0)) (quote (/ 1 0)))

; Partial values can be completed in several groups or stored for dispatch.
(: part-three (-> Number String Number Number))
(= (part-three $a $s $b) (+ $a $b))
!(test (((part-three 1) "ok") 2) 3)
; This checks closure construction, not type enforcement by variable-headed reduce.
!(test (let $f (part-first 7) ($f "ok")) 7)

; The result is still checked when the call is complete.
(: part-bad-result (-> Number String Number))
(= (part-bad-result $n $s) "bad")
!(test (collapse ((part-bad-result 1) "ok")) ())

; Evaluating an argument while creating the closure happens only once.
(= (part-once)
   (let $n (get-state &part-count)
      (let $_ (change-state! &part-count (+ $n 1)) 4)))
!(test (change-state! &part-count 0) True)
!(test ((part-first (part-once)) "ok") 4)
!(test (get-state &part-count) 1)

; Unrelated parametric calls keep their existing answer/effect multiplicity.
(: part-value part-A)
(: part-value part-B)
(: part-poly (-> $a Number %Undefined%))
(= (part-poly $x $n) (let $_ (add-atom &part-poly-effects ran) 7))
!(test (collapse ((part-poly part-value) 1)) (7 7))
!(test (collapse (match &part-poly-effects ran counted)) (counted counted))

; Overloaded calls retain their existing signature-selection behavior.
(: part-over (-> Number Number %Undefined%))
(: part-over (-> String String %Undefined%))
(= (part-over $x $y) $y)
!(test (collapse ((part-over 1) 2)) (2))
!(test (collapse ((part-over "a") "b")) ("b"))

; Empty argument groups do not force the result check either.
!(test ((part-first) 1 "ok") 1)

; Repeating the same signature is still one fixed signature.
(: part-duplicate (-> Number String Number))
(: part-duplicate (-> Number String Number))
(= (part-duplicate $n $s) $n)
!(test (collapse ((part-duplicate 1) "ok")) (1))

; Atom-result templates keep their existing meaning.
(: part-template (-> Number String Atom))
(= (part-template $n $s) (pair $n $s))
!(test ((part-template 1) "ok") (quote (pair 1 "ok")))
(: part-template-add (-> Number Atom))
(= (part-template-add $x) (+ $x 1))
!(test ((part-template-add) 4) (quote (+ 4 1)))

; The body starts only on completion and runs once.
(: part-body (-> Number String Number))
(= (part-body $n $s)
   (let $count (get-state &part-body-count)
      (let $_ (change-state! &part-body-count (+ $count 1)) $n)))
!(test (change-state! &part-body-count 0) True)
!(test (let $f (part-body 1) (get-state &part-body-count)) 0)
!(test (((part-body) 1) "ok") 1)
!(test (get-state &part-body-count) 1)

; A rejected final result does not suppress the body's side effects.
(: part-effect-result (-> Number String Number))
(= (part-effect-result $n $s)
   (let $_ (change-state! &part-effect-result-count 1) "bad"))
!(test (change-state! &part-effect-result-count 0) True)
!(test (collapse ((part-effect-result 1) "ok")) ())
!(test (get-state &part-effect-result-count) 1)
