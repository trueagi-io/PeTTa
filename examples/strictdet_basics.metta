!(import! &self (library lib_roman))

; Under --strict-det a plain -> is a determinism commitment: functions are
; deterministic unless declared -[nondet]->. Overlapping clause heads,
; superpose/match bodies, and dynamic eval all become compile errors for ->
; functions; nondeterminism must be visible in the arrow.

(: dec (-> Number Number))
(= (dec $n) (- $n 1))

(: countdown (-> Number Number))
(= (countdown $n) (if (< $n 1) 0 (countdown (dec $n))))

!(test (countdown 5) 0)

; Explicit nondeterminism: allowed, and visible in the type.
(: pick (Number -[nondet]-> Number))
(= (pick $n) (superpose (1 2 $n)))

!(test (collapse (pick 9)) (1 2 9))

; Higher-order functions are deterministic when their closure parameter's
; arrow says so - a plain (-> $a $b) param is a det closure under this flag:
!(test (fold-flat + 0 (1 2 3)) 6)
!(test (map-flat dec (3 4 5)) (2 3 4))

; A nondet closure poisons the application point: wrapping it in a det
; function is rejected, declaring the wrapper nondet is fine.
(: spread ((Number -[nondet]-> Number) Number -[nondet]-> Number))
(= (spread $f $n) ($f $n))

!(test (collapse (spread pick 7)) (1 2 7))
