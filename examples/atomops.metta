!(test (cons-atom 0 (1 2 3))
       (0 1 2 3))

!(test (car-atom (1 2 3))
       1)

!(test (cdr-atom (1 2 3))
       (2 3))

!(test (index-atom (1 2 3) 1)
       2)

!(test (id 5) 5)

!(test (=alpha (Father $X) (Father $Y)) True)

!(test (=alpha (Father $X) (Son $X)) False)

!(test (first-from-pair (A B)) A)

!(test (second-from-pair (A B)) B)

!(test (index-atom (1 2 3) 5) ())

!(test (index-atom (1 2 3) a) ())

!(test (size-atom 5) ())

!(test (sort-atom 5) ())

!(test (unique-atom 5) ())

!(test (alpha-unique-atom 5) ())

!(test (min-atom 5) ())

!(test (max-atom 5) ())

!(test (intersection-atom 5 (a)) ())
