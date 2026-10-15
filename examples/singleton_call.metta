; ($f) calls $f when $f is a function, and is a one-element list otherwise.
(= (answer) 42)
(= (invoke $f) ($f))
!(test (invoke answer) 42)
!(test (invoke 7) (7))
(: wrap (-> $a (List $a)))
(= (wrap $x) ($x))
!(test (wrap 7) (7))
