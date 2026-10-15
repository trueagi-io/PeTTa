; Covering the empty/cons keys is insufficient when the cons clause pins its
; head field: lists whose first element is not 1 remain uncovered.
(= (list-code ()) 0)
(= (list-code (cons 1 $tail)) 1)

(: bad-list-code (-[det]-> (List Number) Number))
(= (bad-list-code $values) (list-code $values))
