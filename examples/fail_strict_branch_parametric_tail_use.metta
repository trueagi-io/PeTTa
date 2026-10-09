; The branch may inspect a universally typed value, but restoring its temporary
; refinement must preserve sharing with the declaration's type variable.  The
; unconditional tail call pins the value to TailWrapped and must therefore
; reject the claimed `-[det]-> $value-type Bool` declaration.
(: TailWrapped Type)
(: TailWrap (-[det]-> Number TailWrapped))

(: accepts-tail-wrapped? (-[det]-> TailWrapped Bool))
(= (accepts-tail-wrapped? (TailWrap $_number)) true)

(: parametric-after-branch (-[det]-> $value-type Bool))
(= (parametric-after-branch $value)
   (progn
      (if (= $value (TailWrap $_number)) true false)
      (accepts-tail-wrapped? $value)))
