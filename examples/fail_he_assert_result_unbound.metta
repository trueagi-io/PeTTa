!(import! &self (library lib_he))

; An unbound expected list must not be unified with the actual answers.
!(assertEqualToResult (empty) $expected)
