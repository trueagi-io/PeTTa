(: apply-one (-> (-> Number Number) Number Number))
(= (apply-one $f $x) ($f $x))

(: bogus-arrow-result (-> Number))
(= (bogus-arrow-result) (apply-one bogus 1))

!(bogus-arrow-result)
