!(import! &self (library lib_combinatorics))

!(test (choose2l (a b c)) ((a b) (a c) (b c)))
!(test (takeK 2 (a b c)) (a b))
