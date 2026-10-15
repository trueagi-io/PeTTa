; Constructor C alone is not covered by the pinned (C 0) pattern.
(: C (-[det]-> Number T))
(: D (-[det]-> Number T))

(= (decode (C 0)) 0)
(= (decode (D $x)) 1)

(: bad-decode (-[det]-> T Number))
(= (bad-decode $value) (decode $value))
