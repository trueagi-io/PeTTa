; A successful constructor match refines the value only inside the true
; branch.  It must discharge the concrete call there without pinning the
; universally quantified input type of inspect-parametric.
(: WrappedValue Type)
(: WrapValue (-[det]-> Number WrappedValue))

(: accept-wrapped-value (-[det]-> WrappedValue Bool))
(= (accept-wrapped-value (WrapValue $_number)) true)

(: inspect-parametric (-[det]-> $value-type Bool))
(= (inspect-parametric $value)
   (if (= $value (WrapValue $_number))
      (accept-wrapped-value $value)
      false))

!(test (inspect-parametric (WrapValue 7)) true)
!(test (inspect-parametric other-value) false)
