; Even a bound probe can yield the same true answer twice when the literal
; contains duplicates, so this call cannot satisfy det.
(: duplicate-known-name? (-[det]-> Atom Bool))
(= (duplicate-known-name? $name)
   (is-member $name (alpha alpha)))
