; quote can construct a literal product without treating its variable head as
; a closure call. The quoted runtime value has the elementwise structural type.
(: quote-weighted (-[det]-> Atom Number (Atom Number)))
(= (quote-weighted $value $weight)
   (quote ($value $weight)))

!(test (quote-weighted sample 0.75) (sample 0.75))
