; A bound probe against a ground duplicate-free literal makes is-member det.
; Its result is a proper bound Bool and may feed another Boolean operation.
(: known-name-and-enabled? (-[det]-> Atom Bool))
(= (known-name-and-enabled? $name)
   (and (is-member $name (alpha beta gamma)) true))

!(test (known-name-and-enabled? beta) true)
!(test (known-name-and-enabled? delta) false)
