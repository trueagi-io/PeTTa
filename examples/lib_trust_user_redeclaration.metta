!(import! &self (library lib_trust_fixture))

; Repeating the declaration from user code deliberately opts the symbol back
; into conservative runtime guards.
(: trusted-number-id (-[det]-> Number Number))

(mystery-number 7)

(= (user-redeclared-result)
   (trusted-number-id
      (match &self (mystery-number $value) $value)))

!(test (user-redeclared-result) 7)
