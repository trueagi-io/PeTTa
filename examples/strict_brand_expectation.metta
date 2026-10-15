!(import! &self (library lib_builtin_types))

; The outer brand supplies its representation as the contextual expectation.
; That product expectation reaches union-typed fields through direct
; construction, if results, and case results.
(: Payload (Newtype ((| Number String) Number)))

(: direct-payload (-> Payload))
(= (direct-payload)
   (brand Payload
      (data 0 6)))

(: conditional-payload (-> Bool Payload))
(= (conditional-payload $choice)
   (brand Payload
      (if $choice
          (data 1 7)
          (data "three" 8))))

(: case-payload (-> Bool Payload))
(= (case-payload $choice)
   (brand Payload
      (case $choice
        ((True (data 5 9))
         (False (data "seven" 10))))))

!(test (direct-payload) (0 6))
!(test (conditional-payload True) (1 7))
!(test (conditional-payload False) ("three" 8))
!(test (case-payload True) (5 9))
!(test (case-payload False) ("seven" 10))
