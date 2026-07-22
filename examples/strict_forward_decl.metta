!(import! &self (library lib_builtin_types))

; Declarations are pre-cached per file, so a function may call a helper whose
; declaration and definition appear later in the same file.
(: caller (-> Number Number))
(= (caller $x) (later $x))

(: later (-> Number Number))
(= (later $x) (+ $x 1))

!(test (caller 41) 42)

; foldall over a typed accumulator is fully typed (no residual guards):
(: sum-generated (-> Number))
(= (sum-generated)
   (foldall + (superpose (1 2 3)) 0))

!(test (sum-generated) 6)
