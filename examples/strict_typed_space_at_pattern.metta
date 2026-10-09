!(import! &self (library lib_patrick))

(: &functional-pattern-jobs (SpaceOf (job Number String)))
!(add-atom &functional-pattern-jobs (job 1 "one"))

; A top-level functional pattern is elaborated to a match variable followed
; by the ordinary function call.  @ therefore aliases the complete stored row
; and unifies it with its inner pattern solely through @'s library definition.
(: functional-pattern-row (-> Number (job Number String)))
(= (functional-pattern-row $number)
   (match &functional-pattern-jobs
      (@ $whole (job $number $label))
      $whole))

; The same elaboration is recursive at a nested field position.
(: functional-pattern-labels (-> Number (String String)))
(= (functional-pattern-labels $number)
   (match &functional-pattern-jobs
      (job $number (@ $whole-label $label))
      (data $whole-label $label)))

; This helper has the same ordinary relational behavior but no privileged
; name.  Its use in a match pattern proves elaboration is function-generic.
(: same-pattern-value (-[det]-> $a $b $a))
(= (same-pattern-value $value $pattern)
   (let $value $pattern $value))

(: generic-functional-pattern-row (-> Number (job Number String)))
(= (generic-functional-pattern-row $number)
   (match &functional-pattern-jobs
      (same-pattern-value $whole (job $number $label))
      $whole))

!(test (functional-pattern-row 1) (job 1 "one"))
!(test (functional-pattern-labels 1) ("one" "one"))
!(test (generic-functional-pattern-row 1) (job 1 "one"))
