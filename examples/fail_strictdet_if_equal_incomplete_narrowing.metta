!(import! &self (library lib_builtin_types))

; CPU does not consume Goal: GPU is a second constructor. The equality-if
; fallthrough must therefore retain (| Goal Statement), so use-statement needs
; a residual guard and strict-det rejects the declaration.
(: Goal Type)
(: CPU (-[det]-> Symbol Expression Expression Goal))
(: GPU (-[det]-> Symbol Goal))
(: Statement (Newtype Expression))

(: use-symbol (-[det]-> Symbol Number))
(= (use-symbol $_symbol) 1)

(: use-statement (-[det]-> Statement Number))
(= (use-statement $_statement) 2)

(: classify (-[det]-> (| Goal Statement) Number))
(= (classify $value)
   (if (= $value (CPU $function $_arguments $_result))
       (use-symbol $function)
       (use-statement $value)))
