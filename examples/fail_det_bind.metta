; bind!'s only clause is 'bind!'(A, ['new-state', B], C) - the second argument
; must literally be a (new-state ...) expression. The standard idiom
; (bind! &counter 5) matches nothing and fails, so bind! is semidet: zero or
; one, never guaranteed one. Declared det, this compiled clean and the whole
; function quietly produced nothing.
(: bb (-[det]-> Atom))
(= (bb) (bind! &counter 5))

!(bb)
