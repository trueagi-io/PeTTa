!(import! &self (library lib_he))

(a 1)
(a 2)
(b 1)
(b 1)

; Multiple results.
!(test (assertEqualToResult (match &self (a $x) $x) (1 2)) True)

; A single result.
!(test (assertEqualToResult (+ 1 2) (3)) True)

; Duplicate answers must be preserved.
!(test (assertEqualToResult (match &self (b $x) $x) (1 1)) True)

; No solutions produce an empty result list.
!(test (assertEqualToResult (match &self (missing $x) $x) ()) True)

; An incorrect expected result list must be rejected.
!(test (assertEqualToResult (match &self (a $x) $x) (1 2 3)) False)

; Result ordering does not affect multiset equality.
!(test (assertEqualToResult (match &self (a $x) $x) (2 1)) True)

; Duplicates must be preserved when comparing result multisets.
!(test (assertEqualToResult (match &self (b $x) $x) (1)) False)
