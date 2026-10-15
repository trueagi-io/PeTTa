!(import! &self (library lib_trust_fixture))

(mystery-number 7)

; Library trust suppresses residuals only outside strict modes.
(= (strict-library-result)
   (trusted-number-id
      (match &self (mystery-number $value) $value)))
