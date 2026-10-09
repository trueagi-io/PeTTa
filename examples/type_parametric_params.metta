!(import! &self (library lib_builtin_types))

; Positive pinning for the parametricity rule: a declared type-variable
; parameter is honored as long as the body never forces it to a concrete type.

; Identity: $a stays a bare type variable through the body.
(: myid (-> $a $a))
(= (myid $x) $x)
!(test (myid 7) 7)
!(test (myid "s") "s")

; Head-literal instantiation: each clause head binds $a to a concrete type via
; its pattern (5 -> Number, "hi" -> String) BEFORE the parametricity snapshot,
; so no position is left a bare type variable and the clauses are legal.
(: myg (-> $a Number))
(= (myg 5) 1)
(= (myg "hi") 2)
!(test (myg 5) 1)
!(test (myg "hi") 2)

; Pass-through to another polymorphic function: applying myid to the parameter
; aliases $a to myid's own type variable - still a variable, still legal.
(: mythrough (-> $a $a))
(= (mythrough $x) (myid $x))
!(test (mythrough 42) 42)
!(test (mythrough "t") "t")
