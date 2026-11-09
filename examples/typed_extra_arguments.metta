; A declaration describes calls with at most as many arguments as it has inputs.
; A function whose body is a partial application takes its further arguments directly.
(: pu (-> Number %Undefined%))
(= (pu $x) (+ $x))

(: cur (-> Number (-> Number Number)))
(= (cur $x) (+ $x))

; With declarations of two arities, each call uses the declarations that fit it.
(: md (-> Number Number))
(: md (-> Number Number Number))
(= (md $x) $x)
(= (md $x $y) $y)

!(test (pu 1 2) 3)
!(test (cur 1 2) 3)
!(test (md 1 2) 2)
!(test (collapse (md 1 "s")) ())

; Partial calls and an exact single-arity signature keep their existing checks.
(: partial-extra (-> Number Number %Undefined%))
(= (partial-extra $a $b) (+ $a $b))
!(test ((partial-extra 1) 2) 3)
(: exact-extra (-> Number Number))
(= (exact-extra $x) (+ $x 1))
!(test (exact-extra 1) 2)
!(test (collapse (exact-extra "bad")) ())
