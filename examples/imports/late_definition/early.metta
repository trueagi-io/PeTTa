; Compiled before late.metta defines later-defined and late-effect!.
(= (early-call $xs) (later-defined (car-atom $xs)))
(= (early-statement $x) (let $_ (late-effect! $x) done))
(= (early-native $x) (late-native (car-atom $x)))
