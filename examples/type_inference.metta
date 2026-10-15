; Pins the local type inference behavior (see AGENTS.md): inference only adds
; knowledge - it eliminates guards and types outputs, but never rejects code.
(: sid2 (-> String String))
(= (sid2 $s) $s)

; Fully inferable: fi : (-> Number Number), compiles guard-free with fused
; arithmetic (the dispatch matrix asserts the generated code).
(= (fi $x) (+ $x 1))
!(test (fi 41) 42)

; Heterogeneous use taints the parameter: the clause still compiles, and the
; conflicting use checks at runtime.
(= (tainted $x) (progn (+ $x 1) (sid2 $x)))
!(test (let* (($r (catch (eval (quote (tainted 5)))))) (car-atom $r)) Error)

; Untyped higher-order function: applying $f infers its arrow type, so the
; body dispatches through the closure fast path (apply_fn1).
(= (mapl $f ()) ())
(= (mapl $f (cons $x $xs)) (cons ($f $x) (mapl $f $xs)))
!(test (mapl fi (1 2 3)) (2 3 4))

; A call that statically mismatches an inferred type never fails compilation -
; it degrades to the runtime guard the call would have had anyway.
!(test (let* (($r (catch (eval (quote (fi "oops")))))) (car-atom $r)) Error)
