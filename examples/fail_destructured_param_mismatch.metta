; Giving a destructured parameter's fields assumption types elides the guards
; inside the body, so the fields' types have to be REBUILT into the parameter's
; own inferred type - (stv Number Number) - or nothing is left to notice this
; call. Without that rebuilding it quietly evaluates to 9506: SWI-Prolog reads
; a one-character string as its character code. See strict_destructured_param.
(= (tvf2 (stv $s $c)) (* $s $c))

!(tvf2 (stv "a" "b"))
