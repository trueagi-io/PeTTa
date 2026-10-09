; The initial constructor-set snapshot lets the fallthrough exclude Ref.
; Adding another Ref constructor invalidates that proof and recompiles
; dispatch; the surviving (| Wrapped Ref) obligation is then rejected.

(: Wrapped (Newtype Expression))
(: Ref Type)
(: mk-ref (-[det]-> Number Ref))

(: use-wrapped (-[det]-> Wrapped Number))
(= (use-wrapped $_w) 7)

(: dispatch (-[semidet]-> (| Wrapped Ref) Number))
(= (dispatch $v)
   (case $v
      (((mk-ref $n) $n)
       ($rest (use-wrapped $rest)))))

!(add-atom &self (: mk-late-ref (-[det]-> String Ref)))
