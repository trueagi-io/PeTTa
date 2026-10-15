; and/or/not/xor/implies type-check their arguments with bool/1, which is two
; facts - so an UNBOUND argument is not reported, it is enumerated, and the
; operator answers twice. The declared (: and (-> Bool Bool Bool)) guarantees
; nothing here: the compiler's residual type guard succeeds on an unbound
; variable (it records a requirement, it does not test one), so a declared
; Bool never implies a bound one.
;
; And an unbound Bool comes out of code that is well typed throughout: (B $u)
; builds a Boxed whose field is simply not filled in. Declared det, this whole
; chain compiled clean and (run) returned true AND false.
;
; The enumeration is real and it is used deliberately - see
; examples/booleansolver.metta, which solves (and (or $x True) $y) for its
; variables - so the boolean operators are recorded nondet and a -[det]-> body
; may not call them. examples/determinism_builtins.metta shows the honest
; arrow.
(: Boxed Type)
(: B (-> Bool Boxed))

(: mk (-[det]-> Boxed))
(= (mk) (B $u))

(: unbox (-[det]-> Boxed Bool))
(= (unbox (B $b)) (and $b true))

(: run (-[det]-> Bool))
(= (run) (unbox (mk)))

!(run)
