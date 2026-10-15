; A variable branch of case is the committed fallthrough.  Earlier branches
; that consume every constructor of a union member remove that member from the
; fallthrough type.

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

!(test (dispatch (mk-ref 5)) 5)
!(test (dispatch (brand Wrapped payload)) 7)

; Subtraction can retain a reduced union, not just a singleton.  AOnly's sole
; constructor is consumed, so the fallthrough is (| BOnly COnly).
(: AOnly Type)
(: make-a (-[det]-> Number AOnly))
(: BOnly Type)
(: make-b (-[det]-> String BOnly))
(: COnly Type)
(: make-c (-[det]-> Bool COnly))

(: use-b-or-c (-[det]-> (| BOnly COnly) Atom))
(= (use-b-or-c $_v) remaining)

(: narrow-three (-[semidet]-> (| AOnly BOnly COnly) Atom))
(= (narrow-three $v)
   (case $v
      (((make-a $_n) consumed)
       ($rest (use-b-or-c $rest)))))

!(test (narrow-three (make-a 1)) consumed)
!(test (narrow-three (make-b "b")) remaining)
!(test (narrow-three (make-c True)) remaining)
