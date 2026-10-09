; The data-pattern reading of = operands only relaxes a VARIABLE head. A
; FUN-headed operand is genuinely evaluated, so a nondet call in operand
; position still makes the enclosing -[det]-> body non-deterministic and is
; rejected - the relaxation does not hide a real dispatch.

(: gen (-[nondet]-> Number))
(= (gen) (superpose (1 2)))

(: check-shape (-[det]-> Bool))
(= (check-shape)
   (if (= (gen) 1) true false))

!(test (check-shape) true)
