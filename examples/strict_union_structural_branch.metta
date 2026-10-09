; A branch of a control-flow construct may return a bare structural literal
; whose type value_single_type/2 cannot name - (a b) is a list of undeclared
; atoms, not a constructor application. Such a literal used to record the merge
; UNKNOWN marker, which cost a residual runtime guard (rejected under --strict),
; even though the literal itself provably fits the declared union member: a
; ground literal is instead recorded as a certifiable-literal candidate and
; discharged by running check_value/3 on the concrete value at the output
; certification, where the declared union type is in hand.

(: pick-value (-> Atom (| Number (List Atom))))
(= (pick-value $tag)
   (case $tag
      ((number 1)
       ($_other (a b)))))

!(test (pick-value number) 1)
!(test (pick-value other) (a b))

; The same holds through an if - the mechanism is the branch merge, not case.
(: pick-value-if (-> Atom (| Number (List Atom))))
(= (pick-value-if $tag)
   (if (== $tag number)
      1
      (a b)))

!(test (pick-value-if number) 1)
!(test (pick-value-if other) (a b))
