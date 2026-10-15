; A declared type-variable parameter claims universality: (-> $a Number) says
; fun accepts ANY first argument. But the body applies (+ $x 1), which forces
; $x to Number, so the declaration is dishonest - callers of the $a position are
; unchecked and !(fun "hello") would die with a raw is/2 error at runtime.
; Rejected in every mode (declare the concrete Number type instead).
(: fun (-> $a Number))
(= (fun $x) (+ $x 1))
!(test (fun 5) 6)
