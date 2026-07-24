!(import! &self (library lib_memo))

!(memoize sq)
(= (sq $x) (* $x $x))

; One miss, then two hits on the same key (9*9 = 81).
!(test (sq 9) 81)
!(test (sq 9) 81)
!(test (sq 9) 81)
