; tests for alpha-unique-atom 

; 1 Basic duplicates with different variables
!(test (=alpha (alpha-unique-atom ((link $x human) (link $y human) (link $z human)))
               ((link $a human)))
       True)

!(test (=alpha (alpha-unique-atom ((parent $x human) (parent $y human) (child $z human)))
               ((parent $a human) (child $b human)))
       True)

; 2 Different functors
!(test (=alpha (alpha-unique-atom ((parent $x human) (child $y human) (friend $z human)))
               ((parent $a human) (child $b human) (friend $c human)))
       True)

!(test (=alpha (alpha-unique-atom ((likes $x) (hates $y) (knows $z)))
               ((likes $a) (hates $b) (knows $c)))
       True)

; 3 Nested structures
!(test (=alpha (alpha-unique-atom ((link (foo $x) human) (link (foo $y) human) (link (bar $z) human)))
               ((link (foo $a) human) (link (bar $b) human)))
       True)

!(test (=alpha (alpha-unique-atom ((parent (child $x) human) (parent (child $y) human) (parent (child $x) human)))
               ((parent (child $a) human)))
       True)

; 4 Mix of unique and duplicates
!(test (=alpha (alpha-unique-atom ((link $x human) (parent $x human) (link $y human) (parent $z human) (link $x human)))
               ((link $a human) (parent $a human)))
       True)

!(test (=alpha (alpha-unique-atom ((foo $x) (foo $y) (bar $x) (foo $x) (bar $y)))
               ((foo $a) (bar $a)))
       True)

; 5 Numbers and atoms
!(test (=alpha (alpha-unique-atom (1 2 2 3 1 4 4 5))
               (1 2 3 4 5))
       True)

!(test (=alpha (alpha-unique-atom (a b a c b d e a))
               (a b c d e))
       True)

; 6 Empty and single-element lists
!(test (=alpha (alpha-unique-atom ())
               ())
       True)

!(test (=alpha (alpha-unique-atom (1))
               (1))
       True)

!(test (=alpha (alpha-unique-atom ((link $x human)))
               ((link $a human)))
       True)
