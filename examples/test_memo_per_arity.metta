!(import! &self (library lib_memo))

!(memoize add 2)

(= (add $x $y) (+ $x $y))

(= (add $x $y $z) (+ (+ $x $y) $z))

!(test (add 3 4) 7)
!(test (add 3 4) 7)

!(test (add 1 2 3) 6)

!(test (add 5 6) 11)
!(test (add 5 6) 11)
