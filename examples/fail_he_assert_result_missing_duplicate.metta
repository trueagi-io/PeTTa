!(import! &self (library lib_he))

; Duplicate expected answers must each be matched.
!(assertEqualToResult (superpose (1)) (1 1))
