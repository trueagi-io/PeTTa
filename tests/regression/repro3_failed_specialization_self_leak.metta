; Failed specialization must remove its temporary type atoms from &self,
; while preserving the user-declared type of wrap (#237). The function
; parameter is forwarded but never called, so specializing it has no benefit.
(= (myfun $x) $x)
(: wrap (-> (-> Number Number) Number Number))
(= (wrap $f $x) (wrap2 $f $x))
(= (wrap2 $f $x) $x)

!(wrap myfun 1)
!(wrap myfun 2)
!(wrap myfun 3)
!(wrap myfun 4)
!(collapse (match &self (: $what $type) ($what $type)))
