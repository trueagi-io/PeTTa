; The OUTPUT half of the same promise. $b is the result type of g's closure
; parameter, so it is whatever the CALLER's function returns - g knows nothing
; about it. The clause body's result is therefore of type $b, and g declares it
; returns Number.
;
; That certification used to be discharged statically, because comparing the
; unbound $b against Number succeeded by unification: "I don't know" read as
; "compatible with everything", the same defect the unknown-branch marker was
; introduced to fix for merged branches. --strict then reported zero residual
; runtime checks for a program that hands a String to arithmetic.
;
; An unbound type variable the declaration promised to its callers is not
; evidence, so the certification falls back to a runtime guard - which is
; exactly what --strict forbids. (Without --strict the program compiles with
; that guard and reports a proper type error instead of a silent wrong answer.)
(: g (-> (-> Number $b) Number))
(= (g $f) ($f 1))

(: h (-> Number String))
(= (h $x) "str")

(: consume (-> Number Number))
(= (consume $n) (+ $n 1))

!(consume (g h))
