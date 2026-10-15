(: f2 (-[det]-> Atom Atom))
(= (f2 a) a)
(= (f2 b) b)

!(test (f2 a) a)
!(test (f2 b) b)
