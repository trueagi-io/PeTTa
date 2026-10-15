(: b B)
(: only-a (-> A %Undefined%))
(= (only-a $x) (ok $x))
(= (fa (pair $a $unused)) (only-a $a))
; A partial inferred shape must still enforce its known field at the caller.
!(fa (pair b 1))
