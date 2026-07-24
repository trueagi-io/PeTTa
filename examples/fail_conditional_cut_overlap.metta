; A cut inside an if branch may never execute, so it cannot justify the
; clause-entry commit that overlapping det clauses would need.
(: choose (-[det]-> Atom Expression))
(= (choose $x) (if (== $x z) (cut)))
(= (choose a) a)
!(test (choose a) a)
