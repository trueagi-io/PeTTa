; A bound Bool parameter selects exactly one non-overlapping, exhaustive head.
(= (bool-code True) 1)
(= (bool-code False) 0)

(: selected-code (-[det]-> Bool Number))
(= (selected-code $value) (bool-code $value))

!(test (selected-code True) 1)
!(test (selected-code False) 0)
