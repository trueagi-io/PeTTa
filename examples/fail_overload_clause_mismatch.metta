!(import! &self (library lib_builtin_types))

; The clause head (overloaded 0) selects the Number overload, whose output
; the body then violates - caught at compile time (previously unchecked,
; issue 2 of the PeTTaChainer review).
(: overloaded (-> Number Number))
(: overloaded (-> String String))

(= (overloaded 0) "wrong")
