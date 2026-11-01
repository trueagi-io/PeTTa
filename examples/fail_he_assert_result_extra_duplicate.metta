!(import! &self (library lib_he))

; Duplicate actual answers must not be discarded.
!(assertEqualToResult (superpose (1 1)) (1))
