; case with an Empty case evaluates its key once, streams the key's values,
; and takes the Empty case only if the key produced no value at all.

(= (tick! $s) (let $v (+ (get-state $s) 1) (let $_ (change-state! $s $v) $v)))

; The key is evaluated once, whether it yields a value or not:
!(bind! &n (new-state 0))
!(test (case (tick! &n) ((1 one) (2 two) (Empty none))) one)
!(test (get-state &n) 1)
!(bind! &m (new-state 0))
!(test (case (let $_ (tick! &m) (empty)) ((1 one) (Empty none))) none)
!(test (get-state &m) 1)

; A key that yields a value only once (like popping a queue) does not also take the Empty case:
!(bind! &q (new-state full))
(= (pop! $q) (if (== (get-state $q) full) (let $_ (change-state! $q drained) 5) (empty)))
!(test (collapse (case (pop! &q) ((5 got-five) (Empty got-nothing)))) (got-five))

; Key values are streamed: once stops generating at the first answer:
!(bind! &g (new-state 0))
!(test (once (case (let $v (superpose (1 2 3)) (let $_ (tick! &g) $v)) ((2 two) (Empty none)))) two)
!(test (get-state &g) 2)

; Unchanged: first matching case per key value, values without a match give no result,
; and the Empty case is not taken when the key produced values:
!(test (collapse (case (superpose (1 2 3)) ((2 two) ($x (other $x)) (Empty none)))) ((other 1) two (other 3)))
!(test (collapse (case (superpose (1 2 3)) ((9 nine) (Empty none)))) ())

; Unchanged: re-entering the case by backtracking decides Empty afresh each time:
!(test (collapse (let $y (superpose (1 2 1)) (case (if (== $y 1) 5 (empty)) ((5 five) (Empty none))))) (five none five))

; Unchanged: a cut inside the key still commits the enclosing function clause:
(= (committed) (case (progn (cut) a) ((a A) (Empty DEFAULT))))
(= (committed) OUTSIDE)
!(test (collapse (committed)) (A))
