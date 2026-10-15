(: effect-map
   (-[$e]-> (-[$e]-> $a $b) (List $a) (List $b)))
(= (effect-map $f ()) ())
(= (effect-map $f (cons $x $xs))
   (cons ($f $x) (effect-map $f $xs)))

(: effect-inc (-[det]-> Number Number))
(= (effect-inc $x) (+ $x 1))

(: effect-choice (-[nondet]-> Number Number))
(= (effect-choice $x) (superpose ($x (+ $x 10))))

(: effect-positive (-[semidet]-> Number Number))
(= (effect-positive $x) (if (> $x 0) $x))

(: effect-map-det (-[det]-> (List Number) (List Number)))
(= (effect-map-det $xs) (effect-map effect-inc $xs))

(: effect-map-semidet (-[semidet]-> (List Number) (List Number)))
(= (effect-map-semidet $xs) (effect-map effect-positive $xs))

(: effect-map-nondet (-[nondet]-> (List Number) (List Number)))
(= (effect-map-nondet $xs) (effect-map effect-choice $xs))

; A declared List parameter is honest selection evidence because the
; committed wrapper publishes a proper-list boundary proviso.
(: effect-list-source (-[det]-> (List Number)))
(= (effect-list-source) (1 2))

(: effect-map-produced (-[det]-> (List Number)))
(= (effect-map-produced)
   (let $xs (effect-list-source)
      (effect-map effect-inc $xs)))

; An inline producer expression contributes its proper-list output
; certificate directly to selection; no staging let is required.
(: effect-map-collapsed (-[det]-> (List Number)))
(= (effect-map-collapsed)
   (effect-map effect-inc (collapse (superpose (1 2)))))

; Finite nominal selectors use the parallel nonvar proviso.
(: effect-select
   (-[$e]-> (-[$e]-> Number Number) Bool Number))
(= (effect-select $f True) ($f 1))
(= (effect-select $f False) ($f 2))

(: effect-select-det (-[det]-> Bool Number))
(= (effect-select-det $which) (effect-select effect-inc $which))

!(test (effect-map-det (1 2)) (2 3))
!(test (effect-map-produced) (2 3))
!(test (effect-map-collapsed) (2 3))
!(test (effect-select-det True) 2)
!(test (effect-select-det False) 3)
!(test (collapse (effect-map-semidet (1 -1))) ())
!(test
   (collapse (effect-map-nondet (1 2)))
   ((1 2) (1 12) (11 2) (11 12)))
