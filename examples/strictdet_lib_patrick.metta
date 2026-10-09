!(import! &self (library lib_patrick))

!(test
   (collapse
      (for $x (1 2 3)
           (if (> $x 1) $x)))
   (2 3))

(: patrick-step (-[det]-> Number Number Number))
(= (patrick-step $i $state) (+ $state $i))

(: patrick-choice-step (-[nondet]-> Number Number Number))
(= (patrick-choice-step $i $state)
   (superpose ($state (+ $state (+ $i 1)))))

!(test (iterate 0 2 0 patrick-step) 1)
!(test (collapse (iterate 0 1 0 patrick-choice-step)) (0 1))
