; (brand T V) acquires a nominal brand contextually at construction. When V is
; a value merged from control flow - (if $flag (left) (right)) - every branch is
; a ground literal, so the merged variable is not a known singleton. The
; explicit brand still covers it: the brand is applied to the merged variable
; and the output certification confirms EVERY branch candidate is admissible for
; the brand's representation (Expression here). No residual nominal guard is left
; - passing under --strict is the proof.

(: Box (Newtype Expression))

(: choose-box (-> Bool Box))
(= (choose-box $flag)
   (brand Box
      (if $flag
         (left)
         (right))))

!(test (choose-box true) (left))
!(test (choose-box false) (right))

; The brand covers the merge ONLY when every branch is acceptable to the
; representation: with a concrete representation, a branch that does not fit is
; still rejected (see fail_brand_control_flow_mismatch.metta). The rule is not
; loosened - a value acquires a brand at exactly one place, an explicit (brand).
