(: later-defined (-> Number Number))
(= (later-defined $x) (+ $x 100))
(: late-effect! (-> Number Bool))
(= (late-effect! $x) (add-atom &effects (effect $x)))
