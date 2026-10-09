!(import! &self (library lib_trust_fixture))

(mystery-number 7)

; A declaration on the caller opts its clause into ordinary boundary guards.
(: declared-library-result (-> Number))
(= (declared-library-result)
   (trusted-number-id
      (match &self (mystery-number $value) $value)))

!(test (declared-library-result) 7)
