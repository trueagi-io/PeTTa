; Tests for the match-count builtin.
; match-count counts the number of atoms in a space that match a pattern,
; without materializing the result set — useful on very large knowledge bases
; where `(length (collapse (match ...)))` would exhaust the stack.

(parent alice bob)
(parent alice carol)
(parent bob dave)
(parent bob eve)
(parent carol frank)

; 1) Counts every atom matching the pattern.
!(test (match-count &self (parent $p $c)) 5)

; 2) Counts only atoms matching a bound pattern (alice's direct children).
!(test (match-count &self (parent alice $c)) 2)

; 3) Returns 0 when nothing matches, without failing.
!(test (match-count &self (parent zelda $c)) 0)