; A typed call runs its body once for each tuple of argument values.
; Declarations decide whether it runs and which results are kept; they never
; multiply answers or effects.

; Two declarations of the same arity that both accept the call.
(: either (-> Atom %Undefined%))
(: either (-> Number %Undefined%))
(= (either $x) (called $x))
!(test (collapse (either 7)) ((called 7)))
; An argument stays unevaluated only when every declaration takes it as Atom.
!(test (collapse (either (+ 1 2))) ((called 3)))

; The body's effects happen once.
!(bind! &once-count (new-state 0))
(: count-call (-> Atom %Undefined%))
(: count-call (-> Symbol %Undefined%))
(= (count-call $x) (change-state! &once-count (+ 1 (get-state &once-count))))
!(count-call a)
!(test (get-state &once-count) 1)

; A value with two types under one parametric declaration.
(: two-typed A)
(: two-typed B)
(: one-typed B)
(: poly (-> $a Number))
(= (poly $x) 1)
!(test (collapse (poly two-typed)) (1))
; The type variable must agree across arguments: only B fits both.
(: same-type (-> $a $a Number))
(= (same-type $x $y) 2)
!(test (collapse (same-type two-typed one-typed)) (2))
(: echo (-> $a $a))
(= (echo $x) $x)
!(test (collapse (echo two-typed)) (two-typed))

; Overloads still select by type, and each result is kept once.
(: pick (-> Number Number))
(: pick (-> String String))
(= (pick $x) $x)
!(test (collapse (pick 1)) (1))
!(test (collapse (pick "s")) ("s"))
!(test (collapse (pick (superpose (1 "a")))) (1 "a"))
; Results are checked against every declaration that accepted the arguments.
(: both-results (-> Number Number))
(: both-results (-> Number String))
(= (both-results $x) $x)
(= (both-results $x) "s")
!(test (collapse (both-results 1)) (1 "s"))

; Unbound arguments keep their delayed guard across overloads without retaining
; constraints from declarations that rejected the result.
(: free-over (-> (List $a) %Undefined%))
(: free-over (-> String %Undefined%))
(= (free-over $y) ok)
!(test (let $x $free (let $r (free-over $x) (let $x (make-list 1) $r))) ok)
!(test (let $x $free (let $r (free-over $x) (let $x "hello" $r))) ok)
!(test (let (Error $_ $kind) (catch (let $x $free (let $r (free-over $x) (let $x 42 $r)))) $kind) typecheck)
(: late (-> (List $a) Number))
(: late (-> String String))
(= (late $x) "ok")
!(test (collapse (let $x $free (let $r (late $x) (let $x "hello" $r)))) ("ok"))
!(test (let (Error $_ $kind) (catch (let $x $free (let $r (late $x) (let $x (make-list 1) $r)))) $kind) typecheck)

; Open elements under a parametric list declaration keep their type variable unbound.
(: consume (-> (List $a) %Undefined%))
(= (consume $xs) ok)
!(test (let $x $free (let $r (consume (make-list $x)) (let $x 1 $r))) ok)

; Input overload obligations are active while the body runs.
!(bind! &body-ticks (new-state 0))
(: bind-bad (-> Number %Undefined%))
(: bind-bad (-> String %Undefined%))
(= (bind-bad $x) (let $x True (change-state! &body-ticks 1)))
!(test (let (Error $_ $kind) (catch (bind-bad $open)) $kind) typecheck)
!(test (get-state &body-ticks) 0)
(: bind-bad-out (-> Number Number))
(: bind-bad-out (-> String String))
(= (bind-bad-out $x) (let $x True (let $_ (change-state! &body-ticks 1) 1)))
!(test (let (Error $_ $kind) (catch (bind-bad-out $open)) $kind) typecheck)
!(test (get-state &body-ticks) 0)

; Overload obligations follow open subterms, variable aliasing, and copied results.
(: nested (-> (List Number) %Undefined%))
(: nested (-> String %Undefined%))
(= (nested $x) ok)
!(test (let $x $free (let $r (nested $x) (let $x (make-list $y) (let $y 1 $r)))) ok)
!(test (let (Error $_ $kind) (catch (let $x $free (let $r (nested $x) (let $x (make-list $y) (let $y "bad" $r))))) $kind) typecheck)
(: nested-both (-> (List Number) %Undefined%))
(: nested-both (-> (List String) %Undefined%))
(= (nested-both $x) ok)
!(test (let $x $free (let $r (nested-both $x) (let $x (make-list $y) (let $y 1 $r)))) ok)
!(test (let $x $free (let $r (nested-both $x) (let $x (make-list $y) (let $y "ok" $r)))) ok)
!(test (let (Error $_ $kind) (catch (let $x $free (let $r (nested-both $x) (let $x (make-list $y) (let $y True $r))))) $kind) typecheck)
(: pair-same (-> Number Number %Undefined%))
(: pair-same (-> String String %Undefined%))
(= (pair-same $x $y) ok)
!(test (let $x $f1 (let $y $f2 (let $r (pair-same $x $y) (let $x $y (let $y 1 $r))))) ok)
!(test (let (Error $_ $kind) (catch (let $x $f1 (let $y $f2 (let $r (pair-same $x $y) (let $x $y (let $y True $r)))))) $kind) typecheck)
(: echo-ov (-> Number Number))
(: echo-ov (-> String String))
(= (echo-ov $x) $x)
!(test (let $rs (collapse (echo-ov $free)) (let $e (car-atom $rs) (let $e 7 $e))) 7)
!(test (let (Error $_ $kind) (catch (let $rs (collapse (echo-ov $free)) (let $e (car-atom $rs) (let $e True $e)))) $kind) typecheck)
; Repeated input-only overloaded calls on the same open variables coalesce equivalent obligations.
!(test (let $x $f1 (let $y $f2 (let $_ (pair-same $x $y) (let $r (pair-same $x $y) (let $x 1 (let $y 2 $r)))))) ok)
!(test (let (Error $_ $kind) (catch (let $x $f1 (let $y $f2 (let $_ (pair-same $x $y) (let $r (pair-same $x $y) (let $x 1 (let $y "bad" $r))))))) $kind) typecheck)
