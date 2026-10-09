!(import! &self (library lib_builtin_types))
!(import! &self (library lib_roman))

; collapse must retain a polymorphic List output through let, and the lambda
; parameter must acquire Number from the declared + call in strict mode.
(: collect-increments (-> (List Number) (List Number)))
(= (collect-increments $xs)
   (let $collected (collapse (superpose $xs))
      (map-flat (|-> ($x) (+ $x 1)) $collected)))

; Space mutation primitives are predicates and therefore have Bool output.
(: remember (-> %Undefined% Bool))
(= (remember $atom) (add-atom &self $atom))

!(test (collect-increments (1 2 3)) (2 3 4))
!(test (remember (strict-type-flow fact)) true)
