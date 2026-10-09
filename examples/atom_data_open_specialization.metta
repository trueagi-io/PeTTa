; Atom arguments stay as source data.  Specializing a recursive consumer of
; such an argument used to turn (cdr-atom $term) into an open list pattern and
; make functional-pattern arity discovery enumerate forever.
(: walk-atom (-[det]-> Atom Bool))
(= (walk-atom ()) false)
(= (walk-atom (cons $_head $tail)) (walk-atom $tail))

(: atom-tail? (-[det]-> Atom Bool))
(= (atom-tail? $term) (walk-atom (cdr-atom $term)))

!(test (atom-tail? (a b)) false)
