!(import! &self (library lib_trust_fixture))

(mystery-library-number "oops")

; The trusted call remains guard-free, but its result is not certified because
; the match result did not statically discharge the Number argument.
(: unverified-library-result (-> Number))
(= (unverified-library-result)
   (trusted-number-id
      (match &self (mystery-library-number $value) $value)))

!(unverified-library-result)
