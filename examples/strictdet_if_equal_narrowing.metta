!(import! &self (library lib_builtin_types))

; (= $value Pattern) in an if condition is a binding unification test. It is
; lowered through case, so the success branch receives constructor field types
; and the committed fallthrough subtracts the consumed union member.
(: Goal Type)
(: CPU (-[det]-> Symbol Expression Expression Goal))
(: Statement (Newtype Expression))

(: use-symbol (-[det]-> Symbol Number))
(= (use-symbol $_symbol) 1)

(: use-statement (-[det]-> Statement Number))
(= (use-statement $_statement) 2)

(: classify (-[det]-> (| Goal Statement) Number))
(= (classify $value)
   (if (= $value (CPU $function $arguments $result))
       (use-symbol $function)
       (use-statement $value)))

!(test (classify (CPU op args result)) 1)
!(test (classify (brand Statement statement)) 2)

; == is identity testing and must not acquire binding/case semantics. A ground
; CPU value is not identical to a source pattern containing fresh variables.
(: identity-only (-[det]-> Goal Number))
(= (identity-only $value)
   (if (== $value (CPU $_function $_arguments $_result))
       10
       20))

!(test (identity-only (CPU op args result)) 20)

; Only a variable LEFT operand is lowered. The existing reverse unification
; remains an ordinary eager equality call.
(: reverse-unify (-[det]-> Goal Number))
(= (reverse-unify $value)
   (if (= (CPU $_function $_arguments $_result) $value)
       30
       40))

!(test (reverse-unify (CPU op args result)) 30)
