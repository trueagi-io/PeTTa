; The higher-order builtins exist in TWO forms and both are live. The
; pseudo-lambda form (map-atom $l $x Body) is rewritten inline by the
; translator; the CLOSURE form (map-atom $l $f) is what src/metta.pl actually
; defines - 'map-atom'/3, 'foldl-atom'/4 and 'filter-atom'/3, i.e. two, three
; and two MeTTa arguments plus the result. Under --strict-det both forms must
; discharge an explicit commitment from the closure they are handed.

(: double (-[det]-> Number Number))
(= (double $x) (* $x 2))

(: add (-[det]-> Number Number Number))
(= (add $acc $x) (+ $acc $x))

(: positive (-[det]-> Number Bool))
(= (positive $x) (> $x 0))

; Closure form, named det function.
(: doubled (-[det]-> $l %Undefined%))
(= (doubled $l) (map-atom $l double))
!(test (doubled (1 2 3)) (2 4 6))

(: total (-[det]-> $l %Undefined%))
(= (total $l) (foldl-atom $l 0 add))
!(test (total (1 2 3)) 6)

(: positives (-[det]-> $l %Undefined%))
(= (positives $l) (filter-atom $l positive))
!(test (positives (1 -2 3)) (1 3))

; Closure form, lambda: its body is analysed directly.
(: tripled (-[det]-> $l %Undefined%))
(= (tripled $l) (map-atom $l (|-> ($x) (* $x 3))))
!(test (tripled (1 2)) (3 6))

; Closure form, arrow-typed parameter: naming the arrow is what lets the
; commitment travel from the caller through this function to map-atom.
(: apply-to-all (-[det]-> $l (-[det]-> $a $b) %Undefined%))
(= (apply-to-all $l $f) (map-atom $l $f))
!(test (apply-to-all (1 2) double) (2 4))

; Pseudo-lambda form: the element variable is a binder, not a closure.
(: squared (-[det]-> (List Number) %Undefined%))
(= (squared $l) (map-atom $l $x (* $x $x)))
!(test (squared (2 3)) (4 9))
