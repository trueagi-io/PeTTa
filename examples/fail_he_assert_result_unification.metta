!(import! &self (library lib_he))

; Variables in actual answers must not be bound to the expected values.
!(assertEqualToResult (quote ($x $x)) ((1 1)))
