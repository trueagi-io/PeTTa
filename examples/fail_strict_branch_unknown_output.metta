; Same rule in output position: the declared (-> Expression Number) is only
; discharged if every case branch is known to produce a Number. The second
; branch yields a field of an arbitrary expression, so the certification needs
; a runtime guard - which --strict rejects. With only that branch the checker
; always rejected; adding a typed branch in front must not buy an exemption.
(: f (-> Expression Number))
(= (f $x) (case $x ((0 0.1) (($a $b) $b))))

!(test (f (p "str")) 1)
