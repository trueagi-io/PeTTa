; foldall may return its initializer when the generator is empty. An
; initializer with no known type therefore cannot certify the accumulator's
; Number output type.
(: add-step (-> Number Number Number))
(= (add-step $acc $x) (+ $acc $x))

(: bad-fold (-> %Undefined% Number))
(= (bad-fold $init) (foldall add-step (empty) $init))
