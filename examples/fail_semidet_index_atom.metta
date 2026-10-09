; index-atom's guard only rejects an index that is BOUND to a non-integer; an
; unbound one falls through to nth0/3, which enumerates the list. Three
; solutions from a -[semidet]-> function, which promises at most one.
;
; The elements it yields are real, so the predicate is behaving correctly and
; the table was wrong about it: index-atom is nondet. -[semidet]-> is for a
; function that may have NO answer, not for one that may have several.
(: ia (-[semidet]-> Atom))
(= (ia) (index-atom (1 2 3) $i))

!(ia)
