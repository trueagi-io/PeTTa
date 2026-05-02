!(add-atom &persistent (= (g $x) (+ $x 1)))

!(add-atom &persistent (= (f $g) ($g 1)))
!(add-atom &persistent (= (f $g) 42))

!(test (collapse (f g)) (2 42))

!(remove-atom &persistent (= (f $g) 42))

!(test (collapse (f g)) (2))

!(add-atom &persistent (= (f $g) 42))

!(remove-atom &persistent (= (f $g) ($g 1)))

!(test (collapse (f g)) (42))

!(remove-atom &persistent (= (f $g) 42))

!(test (collapse (f g)) ((f g)))
