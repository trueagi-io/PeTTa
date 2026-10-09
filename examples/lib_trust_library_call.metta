!(import! &self (library lib_trust_fixture))

(mystery-number 7)

(= (trusted-library-result)
   (trusted-number-id
      (match &self (mystery-number $value) $value)))

!(test (trusted-library-result) 7)
