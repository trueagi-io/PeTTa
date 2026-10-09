; Generated lambdas are analyzed as nested clauses.  A branch-local input
; ascription is permitted, while the other arm establishes the same Number
; output independently.
(: apply-branch-number
   (-[det]-> (-[det]-> Atom Number) Atom Number))
(= (apply-branch-number $function $value)
   ($function $value))

(: valid-lambda-result (-[det]-> Atom Number))
(= (valid-lambda-result $value)
   (apply-branch-number
      (|-> ($argument)
         (if (is-var $argument)
            (the Number $argument)
            1))
      $value))

!(test (valid-lambda-result unused-atom) 1)
