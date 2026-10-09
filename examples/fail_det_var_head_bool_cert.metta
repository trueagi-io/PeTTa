; A variable-headed body ($h True True) applies whatever closure arrives in
; $h; its source syntax says nothing about the result. The Bool output
; certificate once unified that head with `and`, read the body as a boolean
; operation on two literals, and certified g as always returning a bound Bool.
; (and (g u) True) then passed as det, yet u returns an unbound Bool, so and/2
; enumerates it and (run) answered both true and false.
(: Boxed Type)
(: B (-> Bool Boxed))
(: mk (-[det]-> Boxed))
(= (mk) (B $u))
(: field (-[det]-> Boxed Bool))
(= (field (B $b)) $b)
(: u (-[det]-> Bool Bool Bool))
(= (u $x $y) (field (mk)))
(: g (-[det]-> (-[det]-> Bool Bool Bool) Bool))
(= (g $h) ($h True True))
(: run (-[det]-> Bool))
(= (run) (and (g u) True))
!(run)
