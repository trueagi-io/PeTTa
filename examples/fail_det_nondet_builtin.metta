; (get-atoms &self) enumerates the space one solution per atom, so a body that
; calls it is not deterministic. The checker used to assume that any builtin
; with no MeTTa clauses was det - the strongest possible claim about the one
; part of the system it never analyses - and this -[det]-> declaration
; compiled clean, then returned one 7 per atom in the space.
(: howmany (-[det]-> Number))
(= (howmany) (let $a (get-atoms &self) 7))

!(test (collapse (howmany)) (7))
