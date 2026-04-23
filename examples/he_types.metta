!(import! &self lib/lib_he)

!(test (is-function (-> Atom Atom)) True)
!(test (is-function Atom) False)
;TODO:
;(: type1 Type)
;!(type-cast A type1 &self)
;!(type-cast 1 type1 &self)
!(test (match-types Atom Atom "Matched!" "Didn't match") "Matched!")
!(test (match-types Atom Number "Matched!" "Didn't match") "Didn't match")
!(test (first-from-pair (A B)) A)
!(test (second-from-pair (A B)) B)
!(test (match-type-or True Number Bool) True)
