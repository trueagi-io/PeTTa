!(import! &self (library lib_he))

; The expected answers must be an expression, even when there are no answers.
!(assertEqualToResult (empty) invalid)
