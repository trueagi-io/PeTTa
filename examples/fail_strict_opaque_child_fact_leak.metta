; A lambda body is syntax until the lambda is invoked.  Facts inferred while
; inspecting it must not escape into the enclosing sequence and certify $x as
; a Bool result.
(: opaque-bool-leak (-[nondet]-> Expression Bool))
(= (opaque-bool-leak $x)
   (progn
      (|-> () (let $x true true))
      $x))
