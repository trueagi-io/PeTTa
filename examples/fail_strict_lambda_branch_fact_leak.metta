; A type ascription in one lambda arm must not certify the same source
; variable in its fallback arm.  Generated lambdas receive their own unified
; clause record so branch facts remain local inside the deferred body too.
(: apply-leak-number
   (-[nondet]-> (-[nondet]-> Atom Number) Atom Number))
(= (apply-leak-number $function $value)
   ($function $value))

(: bad-lambda-result (-[nondet]-> Atom Number))
(= (bad-lambda-result $value)
   (apply-leak-number
      (|-> ($argument)
         (if (is-var $argument)
            (the Number $argument)
            $argument))
      $value))
