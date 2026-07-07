(= (myfun $x) $x)

(= (wrap $f $x) (pass $f $x))
(= (pass $f $x) (wrap2 $f $x))
(= (wrap2 $f $x) $x)

!(wrap myfun 1)
!(wrap myfun 2)
!(wrap myfun 3)
!(wrap myfun 4)
!(wrap myfun 5)
