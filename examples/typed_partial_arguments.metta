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

; Multiple type witnesses do not multiply the body answers or effects.
(: part-value part-A)
(: part-value part-B)
(: part-poly (-> $a Number %Undefined%))
(= (part-poly $x $n) (let $_ (add-atom &part-poly-effects ran) 7))
!(test (collapse ((part-poly part-value) 1)) (7))
!(test (collapse (match &part-poly-effects ran counted)) (counted))

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

; Polymorphic and overloaded closures resume at the first new input position.
!(test ((part-same 1) 2) True)
!(test (((part-same) 1) 2) True)
!(test (collapse ((part-same 1) "bad")) ())
(: part-pick (-> Number String Number))
(: part-pick (-> String Number String))
(= (part-pick $x $y) $x)
!(test ((part-pick 7) "ok") 7)
!(test (collapse ((part-pick 7) 2)) ())
(: part-three-same (-> $a $a $a Bool))
(= (part-three-same $x $y $z) True)
!(test (((part-three-same 1) 2) 3) True)
!(test (collapse (((part-three-same 1) 2) "bad")) ())

; Completing a closure rechecks values, not their evaluating expressions.
!(test (change-state! &part-count 0) True)
!(test ((part-same (part-once)) 4) True)
!(test (get-state &part-count) 1)
(: part-pick-effect (-> Number String Number))
(: part-pick-effect (-> String Number String))
(= (part-pick-effect $x $y) (let $_ (add-atom &part-pick-effects ran) $x))
!(test ((part-pick-effect 7) "ok") 7)
!(test (collapse (match &part-pick-effects ran counted)) (counted))

; Distinct captured bindings remain distinct when the closure is completed.
(: (part-box good) PartBox)
(: (part-box other) PartBox)
(: part-open (-> PartBox PartBox %Undefined%))
(= (part-open $x $y) $x)
!(test (collapse ((part-open (part-box $free)) (part-box good))) ((part-box good) (part-box other)))

; Output checking is deferred, not dropped, and does not repeat the body.
(: part-poly-bad (-> $a $a Bool))
(= (part-poly-bad $x $y) (let $_ (add-atom &part-poly-bad-effects ran) "bad"))
!(test (collapse ((part-poly-bad 1) 2)) ())
!(test (collapse (match &part-poly-bad-effects ran counted)) (counted))

; Type evidence keeps its lifetime across argument-group boundaries.
(: part-mark A)
(: part-witness A)
(: part-evidence-three (-> $t $t Number Number))
(= (part-evidence-three $x $y $n) 9)
(= (part-erase-evidence) (let $_ (remove-atom &self (: part-mark A)) part-witness))
!(test (collapse (part-evidence-three part-mark (part-erase-evidence) 0)) (9))
!(test (add-atom &self (: part-mark A)) True)
!(test (collapse ((part-evidence-three part-mark (part-erase-evidence)) 0)) (9))
!(test (add-atom &self (: part-mark A)) True)
!(test (collapse (((part-evidence-three part-mark) (part-erase-evidence)) 0)) (9))
!(test (add-atom &self (: part-mark A)) True)
!(test (collapse (((progn (part-evidence-three part-mark)) (part-erase-evidence)) 0)) (9))
!(test (add-atom &self (: part-mark A)) True)
!(test (collapse (((let $unused True (part-evidence-three part-mark)) (part-erase-evidence)) 0)) (9))
!(test (add-atom &self (: part-mark A)) True)
!(test (collapse (((prog1 (part-evidence-three part-mark) True) (part-erase-evidence)) 0)) (9))
(= (part-add-late-evidence) (let $_ (add-atom &self (: part-late A)) part-witness))
!(test (collapse (part-evidence-three part-late (part-add-late-evidence) 0)) ())
!(test (remove-atom &self (: part-late A)) True)
!(test (collapse ((part-evidence-three part-late (part-add-late-evidence)) 0)) ())

; Partial closures created or resumed through eval terminate and recheck captured inputs across multiple groups.
!(test (let $f (eval (part-same 1)) ($f 2)) True)
!(test (collapse (let $p (part-three-same 1) (eval (($p "bad") "bad")))) ())
!(test (collapse (let $p (part-three-same 1) (eval (($p 2) 3)))) (True))
!(test (collapse (let $p (part-three-same 1) (eval (($p 2) "bad")))) ())

; Evidence belongs to the returned partial, never to a discarded expression.
(: part-owner-source (-> Number String Number))
(: part-owner-source (-> Bool String Number))
(= (part-owner-source $x $y) 5)
(: part-owner-target (-> Number Number))
(: part-owner-target (-> Number Bool))
(= (part-owner-target $x) 7)
!(test (collapse (part-owner-target "bad")) ())
!(test (collapse ((progn (part-owner-source 1) part-owner-target) "bad")) ())
!(test (collapse ((let $discarded (part-owner-source 1) part-owner-target) "bad")) ())
(: part-owner-stored (-> Number Number Number))
(: part-owner-stored (-> Bool Number Number))
(= (part-owner-stored $x $y) 7)
!(test (collapse (let $saved (part-owner-stored 1) (eval ($saved "bad")))) ())
!(test (collapse (let $saved (part-owner-stored 1)
                   (eval ((progn (part-owner-source 1) $saved) "bad")))) ())
!(test (collapse ((progn (part-owner-target 1) (part-owner-source 1)) "ok")) (5))

; Saved evidence is narrowed to the declaration arity being completed.
(: part-arity (-> $a $a Bool))
(: part-arity (-> $a $b $c Bool))
(= (part-arity $x $y) True)
(= (part-arity $x $y $z) True)
!(test (collapse (part-arity 1 "bad")) ())
!(test (collapse ((part-arity 1) "bad")) ())
!(test (collapse ((part-arity 1) 2)) (True))
(: part-ground-arity (-> Number String Number))
(: part-ground-arity (-> Number String Bool Number))
(= (part-ground-arity $x $s) $x)
(= (part-ground-arity $x $s $b) $x)
!(test (collapse (part-ground-arity 1 "ok")) (1))
!(test (collapse ((part-ground-arity 1) "ok")) (1))
!(test (collapse ((part-ground-arity 1) 2)) ())
!(test (collapse ((part-ground-arity 1) "ok" True)) (1))
(: part-ground-cross (-> Number String Number))
(: part-ground-cross (-> Bool Bool Bool Number))
(= (part-ground-cross $x $y) 1)
(= (part-ground-cross $x $y $z) 2)
!(test (collapse (part-ground-cross 1 True True)) ())
!(test (collapse ((part-ground-cross 1) True True)) ())
!(test (collapse (let $p (part-ground-cross 1) (eval ($p True True)))) ())
(= (part-ground-late $x $y) $y)
!(test (collapse (let $p (part-ground-late "bad")
                   (let $_ (add-atom &self (: part-ground-late (-> Number Number Number)))
                     (eval ($p 2))))) ())
!(test (collapse (part-ground-cross 1 (add-atom &self part-ground-boom) True)) ())
!(test (collapse ((part-ground-cross 1) (add-atom &self part-ground-boom) True)) ())
!(test (collapse (match &self part-ground-boom found)) ())

; Transparent result paths preserve the inner value's evidence without importing discarded evidence.
!(test (add-atom &self (: part-mark A)) True)
!(test (collapse ((once (part-evidence-three part-mark (part-erase-evidence))) 0)) (9))
!(test (add-atom &self (: part-mark A)) True)
!(test (collapse ((with_mutex &partial-evidence-lock (part-evidence-three part-mark (part-erase-evidence))) 0)) (9))
!(test (add-atom &self (: part-mark A)) True)
!(test (collapse ((transaction (part-evidence-three part-mark (part-erase-evidence))) 0)) (9))
!(test (add-atom &self (: part-mark A)) True)
!(test (collapse ((match &self (: part-witness A) (part-evidence-three part-mark (part-erase-evidence))) 0)) (9))
!(test (collapse ((once (progn (part-owner-source 1) part-owner-target)) "bad")) ())
!(test (collapse ((match &self (: part-witness A)
                        (progn (part-owner-source 1) part-owner-target)) "bad")) ())

; Compiler-internal evidence must not reserve a MeTTa function name.
(= (with_type_evidence $x) $x)
!(test (with_type_evidence 7) 7)
