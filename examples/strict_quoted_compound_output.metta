; Quoted compound code has the declared Expression type.
(: quoted-code (-[det]-> Expression))
(= (quoted-code) (quote (+ 1 2)))

!(test (quoted-code) (quote (+ 1 2)))
