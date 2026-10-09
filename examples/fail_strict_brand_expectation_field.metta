!(import! &self (library lib_builtin_types))

(: Payload (Newtype ((| Number String) Number)))

; The representation expectation is an obligation: Bool contradicts the
; (| Number String) field type before the brand is applied.
(: bad-payload (-> Bool Payload))
(= (bad-payload $choice)
   (brand Payload
      (if $choice
          (data 1 7)
          (data True 8))))
