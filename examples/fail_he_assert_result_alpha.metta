!(import! &self (library lib_he))

; Variables are compared strictly, without alpha-equivalence.
!(assertEqualToResult (quote ($x $x)) (($y $y)))
