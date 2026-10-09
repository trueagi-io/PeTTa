!(import! &self (library lib_builtin_types))
; Repeated-variable conflict detection is symmetric: Number inhabits both
; (| Number String) and Number, so (both $x $x) is legal.
(: both (-> (| Number String) Number Bool))
(= (both $a $b) True)
(: same-number (-> Number Bool))
(= (same-number $x) (both $x $x))
!(test (same-number 3) True)
