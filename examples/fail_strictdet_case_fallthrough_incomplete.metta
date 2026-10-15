; Ref has two constructors, but the first case branch consumes only mk-ref.
; The variable fallthrough must therefore retain (| Wrapped Ref), leaving the
; call to use-wrapped with a strict residual obligation.

(: Wrapped (Newtype Expression))
(: Ref Type)
(: mk-ref (-[det]-> Number Ref))
(: mk-other-ref (-[det]-> String Ref))

(: use-wrapped (-[det]-> Wrapped Number))
(= (use-wrapped $_w) 7)

(: dispatch (-[semidet]-> (| Wrapped Ref) Number))
(= (dispatch $v)
   (case $v
      (((mk-ref $n) $n)
       ($rest (use-wrapped $rest)))))

!(dispatch (mk-other-ref "still-ref"))
