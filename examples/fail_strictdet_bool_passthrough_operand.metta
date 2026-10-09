; A Bool PASSTHROUGH does not certify: (= (pass-bool $b) $b) consumes no
; boundness, so under need-based enforcement it gets no boundary check, its
; parameter may arrive unbound, and its result may BE that unbound value -
; exactly the hole bool/1 would enumerate. The bound_bool certificate
; requires every clause result provably bound, so the call stays an
; unqualified operand and the -[det]-> caller is rejected. Compare
; strictdet_bool_call_operand.metta, where the body builds the boolean.
(: pass-bool (-[det]-> Bool Bool))
(= (pass-bool $b) $b)

(: gate (-[det]-> Bool Bool))
(= (gate $x) (and (pass-bool $x) True))

!(gate True)
