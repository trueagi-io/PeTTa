(: bad-choose (Number -[det]-> Number))
(= (bad-choose $x)
   (superpose ($x (+ $x 1))))
