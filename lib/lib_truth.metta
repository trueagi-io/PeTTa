;; Truth value conversion functions for NARS/PLN systems
;; These functions convert between confidence (c) and weight (w) representations

;; Convert confidence to weight: w = c / (1 - c)
(: Truth_c2w (-> Number Number))
(= (Truth_c2w $c)
(/safe $c (- 1 $c)))

;; Convert weight to confidence: c = w / (w + 1)
(: Truth_w2c (-> Number Number))
(= (Truth_w2c $w)
(/safe $w (+ $w 1)))

;; Safe division that avoids division by zero
(: /safe (-> Number Number Number))
(= (/safe $A $B)
(if (> $B 0.0)
(/ $A $B)
(empty)))
