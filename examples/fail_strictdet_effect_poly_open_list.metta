(: effect-map
   (-[$effect]-> (-[$effect]-> Number Number)
                 (List Number)
                 (List Number)))
(= (effect-map $function ()) ())
(= (effect-map $function (cons $head $tail))
   (cons ($function $head) (effect-map $function $tail)))

(: plus-one (-[det]-> Number Number))
(= (plus-one $value) (+ $value 1))

(: map-an-open-list (-[det]-> (List Number)))
(= (map-an-open-list) (effect-map plus-one $open))
