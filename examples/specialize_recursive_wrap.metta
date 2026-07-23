; A recursive call that wraps its higher-order argument used to diverge at
; COMPILE time: each nested specialization built a bigger key
; (partial(twice,[partial(twice,[derive])]) ...) and never terminated.
; Re-specializing a function already being specialized is now refused; the
; inner call compiles unspecialized and translation terminates.
(= (derive $g) $g)
(= (twice $r $g) ($r ($r $g)))
(= (evolve $r $n $g) (if (== $n 0) $g (evolve (twice $r) (- $n 1) $g)))
!(test (evolve derive 2 stmt) stmt)
