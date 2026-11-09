; File signatures are known before their equations have been installed.
!(test (collapse (twice 4)) ((twice 4)))
(= (twice $x) (* 2 $x))
!(test (twice 4) 8)

; Calls in stored bodies and match templates follow the same rule.
(= (outer $x) (inner $x))
!(test (collapse (outer 3)) ((inner 3)))
(= (inner $x) (+ $x 1))
!(test (outer 3) 4)
(edge 4 1)
!(test (collapse (match &self (edge $a $b) (tag $b))) ((tag 1)))
(= (tag $x) (tagged $x))
!(test (collapse (match &self (edge $a $b) (tag $b))) ((tagged 1)))

; Late runtime definitions refresh stored bodies, including anonymous functions.
(= (ask $x) (answer $x))
!(test (ask 1) (answer 1))
!(add-atom &self (= (answer $x) done))
!(test (ask 1) done)
!(test (let $f (|-> ($x) (late $x))
         (let $u (add-atom &self (= (late $x) (* 10 $x))) ($f 2))) 20)

; A retired complete call returns its expression; restoration calls the new body.
(= (use-gone) (gone 1))
(= (gone $x) (+ $x 1))
!(test (use-gone) 2)
!(remove-atom &self (= (gone $x) (+ $x 1)))
!(test (collapse (use-gone)) ((gone 1)))
!(test (gone 1) (gone 1))
!(add-atom &self (= (gone $x) (+ $x 10)))
!(test (use-gone) 11)

; The current invocation keeps its compiled data expression. The next uses a call.
(= (define-and-use)
   (let $u (add-atom &self (= (fresh $x) (+ $x 10))) (fresh 1)))
!(test (collapse (define-and-use)) ((fresh 1)))
!(test (fresh 1) 11)
