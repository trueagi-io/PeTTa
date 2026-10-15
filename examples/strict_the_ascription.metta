!(import! &self (library lib_builtin_types))

; A dynamically typed value (a match on a relation with no declared schema)
; cannot satisfy strict mode by itself. The author states the assumed type
; with (the Type Expr): the checker gains the knowledge, and an explicit
; runtime check is emitted - allowed even under --strict, which only forbids
; implicit residual checks.
(name "bob")

(: greet (-> String))
(= (greet) (the String (match &self (name $n) $n)))

!(test (greet) "bob")

; The ascription is checked: a wrong assumption raises a runtime type error.
(= (bad-age) (+ (the Number (match &self (name $n) $n)) 1))

!(test (let* (($r (catch (eval (quote (bad-age)))))) (car-atom $r)) Error)
