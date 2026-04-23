!(import! &self lib/lib_roman)
;Test hihger order functions
!(test (map-flat (+ 1) (1 2 3)) (2 3 4))
!(test (map-nested (+ 1) (1 (2 3))) (2 (3 4)))

!(test (fold-flat + 0 (1 2 3)) 6)
!(test (foldr-flat cons () (1 (2 3) 4)) (1 (2 3) 4))
!(test (fold-nested + 0 (1 (2 3))) 6)

;Test set operations
!(test (/=\ (1 2 $a) (2 3 4)) (2 2))
!(test (/==\ (1 2 3) (2 3 4)) (2 3))
!(test (/=a\ (1 2 $a) (2 $a 4)) (2 $a))

!(test (\= (1 2 3) ($a 3 4)) (2))
!(test (\== (1 2 3) (2 3 4)) (1))
!(test (\=a (1 2 $a) (2 $a 4)) (1))

!(test (\=/ (1 2 3) ($a 3 4)) (2 1 3 4))
!(test (\==/ (1 2 3) (2 3 4)) (1 2 3 4))
!(test (\=a/ (1 2 $a) (2 $a 4)) (1 2 $a 4))

;Test composition

!(test (. (+ 1) (* 2) 1) 3)
!(test (.: (+ 1) + 2 3) 6)
!(test (&&& (+ 2) (* 2) 1) (3 2))

(= (mfail $x) (empty))
!(test (collapse (&^& (+ 1) (mfail) 1)) (2))

;Test reverse function matching
!(test (let (@ $lst (cons $h $t)) (1 2 3) ($lst $h $t)) ((1 2 3) 1 (2 3)))

!(test (let (head $x) (1 2 3) $x) 1)
!(test (let (tail $xs) (1 2 3) $xs) (2 3))

!(test (let (mylast $x) (1 2 3) $x) 3)
!(test (let (init $xs) (1 2 3) $xs) (1 2))

!(test (let (rcons $xs $x) (1 2 3) ($xs $x)) ((1 2) 3))

;Test utils

!(test (prog1 (+ 1 1) (+ 2 2)) 2)
!(test (progn (+ 1 1) (+ 2 2)) 4)

