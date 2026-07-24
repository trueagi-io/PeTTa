; A declared output type variable that occurs in no argument type claims
; parametric polymorphism: only a bottom implementation can honor it.
(: magic (-> $a))
(= (magic) 0)
!(test (magic) 0)
