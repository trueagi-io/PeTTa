!(import! &self (library lib_he))

; Every expected answer must be present.
!(assertEqualToResult (superpose (1 2)) (1 2 3))
