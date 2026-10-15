!(import! &self (library lib_builtin_types))

; A closure capturing a typed variable reaches the specializer as a
; partial(...) containing an attributed variable; specialization-key
; normalization must strip the type attributes (copy_term_nat) so names stay
; stable and specialization still succeeds.
(: offset (-> Number))
(= (offset) 3)

(= (apply-twice $f $x) ($f ($f $x)))

!(test (let $n (offset) (apply-twice (|-> ($y) (+ $y $n)) 5)) 11)
