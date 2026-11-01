!(import! &self (library lib_he))

; A scalar answer must match the expected value.
!(assertEqualToResult (+ 1 2) (99))
