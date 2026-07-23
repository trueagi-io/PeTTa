; A cut inside an if branch may never execute, so it cannot justify the
; clause-entry commit that overlapping det clauses would need.
(: choose (Atom -[det]-> Expression))
(= (choose $x) (if (== $x z) (cut)))
(= (choose a) a)
!(test (choose a) a)
