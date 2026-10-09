; Inference not DEMANDING a type is not the same as inference ignoring a value
; it can see is wrong. Inference elided the guard inside f's body, so with no
; check at all this call would quietly compute 98 (SWI-Prolog reads a
; one-character string as its character code) instead of reporting the error.
; A definite mismatch against an inferred type therefore still fails - at the
; call site's guard, the same way the untyped translation would have failed.
(= (f $x) (+ $x 1))

!(f "a")
