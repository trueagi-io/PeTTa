; The other provable case: the clauses discriminate on the constructors of a
; nominal type whose constructor set is known here in full - Circle, Square and
; the nullary Point - and Point matches no clause. A -[det]-> function that
; cannot answer for a Point is not deterministic, it is partial: -[semidet]->.
; The verdict is a LOWER bound on incompleteness, never a totality guarantee:
; a constructor declared in a LATER file cannot be seen from here.
(: Shape Type)
(: Circle (-> Number Shape))
(: Square (-> Number Shape))
(: Point Shape)

(: area (-[det]-> Shape Number))
(= (area (Circle $r)) (* 3 (* $r $r)))
(= (area (Square $s)) (* $s $s))

!(test (area (Square 3)) 9)
