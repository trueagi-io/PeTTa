!(import! &self (library lib_trust_fixture))

(: trusted-fixture-use (-[det]-> Number Number))
(= (trusted-fixture-use $value)
   (trusted-number-id $value))

!(test (trusted-fixture-use 4) 4)
