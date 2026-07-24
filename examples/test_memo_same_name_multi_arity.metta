!(import! &self (library lib_memo))

(= (mix $x) (+ $x 1))
(= (mix $x $y) (+ $x $y))

!(memoize mix 1)
!(test (is-memoized mix 1) true)
!(test (is-memoized mix 2) false)

!(test (mix 5) 6)
!(test (mix 5) 6)

!(test (mix 3 4) 7)
!(test (mix 3 4) 7)

!(memoize mix 2)
!(test (is-memoized mix 2) true)
!(test (mix 8 9) 17)
!(test (mix 8 9) 17)
