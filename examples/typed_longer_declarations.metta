; A declaration with more inputs than a complete call has arguments does not
; describe that call, so the call's body runs once.
(: two-arity (-> Number Number))
(: two-arity (-> Number Number Number))
(= (two-arity $x) (+ $x 1))
(= (two-arity $x $y) (+ $x $y))
!(test (collapse (two-arity 1)) (2))

; The shorter call is still checked against its own declaration.
!(test (collapse (two-arity "s")) ())

; An effect in the body happens once.
!(bind! &longer-count (new-state 0))
(: count-once (-> Atom %Undefined%))
(: count-once (-> Atom Atom %Undefined%))
(= (count-once $x) (change-state! &longer-count (+ 1 (get-state &longer-count))))
!(count-once a)
!(test (get-state &longer-count) 1)

