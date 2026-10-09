; reverse/2, append/3, last/2 and length/2 all INVERT: handed a list argument
; that is partial or unbound they enumerate - every split, every length - and
; length/2 does not terminate at all. Nothing in the compiled program
; establishes that the argument is a proper list. A (List Number) declaration
; is a static claim, and the residual guard that would back it up at run time
; is typecheck_or_error/2, which succeeds on an unbound variable.
;
; The wrapper is certifiable only through its proper-list boundary proviso.
; Passing an unbound list violates that proviso before reverse/2 can enumerate
; list shapes.
(: rev (-[det]-> (List Number) (List Number)))
(= (rev $xs) (reverse $xs))

!(rev $free)
