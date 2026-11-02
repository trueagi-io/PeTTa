; Compiled before late.metta defines late-effect!.
(: early-checked-statement (-> Number Bool))
(= (early-checked-statement $x) (let $_ (late-effect! $x) True))
