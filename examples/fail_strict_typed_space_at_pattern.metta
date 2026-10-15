!(import! &self (library lib_patrick))

(: &functional-pattern-bad-jobs (SpaceOf (job Number String)))
!(add-atom &functional-pattern-bad-jobs (job 1 "one"))

; @ is elaborated as an ordinary call, but its successful value aliases its
; inner literal pattern.  The row schema therefore still proves this Number
; field's String literal can never match.
(: bad-functional-pattern (-> Expression))
(= (bad-functional-pattern)
   (match &functional-pattern-bad-jobs
      (@ $whole (job "wrong" $label))
      $whole))

!(bad-functional-pattern)
