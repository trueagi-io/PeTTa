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
(: pick (-[nondet]-> Number Number))
(= (pick $n) (superpose (1 2 $n)))

!(test (collapse (pick 9)) (1 2 9))

; Higher-order functions are deterministic when their closure parameter's
; arrow says so - a plain (-> $a $b) param is a det closure under this flag:
!(test (fold-flat + 0 (1 2 3)) 6)
!(test (map-flat dec (3 4 5)) (2 3 4))

; A nondet closure poisons the application point: wrapping it in a det
; function is rejected, declaring the wrapper nondet is fine.
(: spread (-[nondet]-> (-[nondet]-> Number Number) Number Number))
(= (spread $f $n) ($f $n))

!(test (collapse (spread pick 7)) (1 2 7))

; A variable head with a provably non-function type is data construction,
; not an application - deterministic like any other structure building:
(: make-pair-list (-> Number Number (List (Number Number))))
(= (make-pair-list $x $y) (cons ($x $y) ()))

!(test (make-pair-list 1 2) ((1 2)))

; case and let patterns are matched, not executed: a variable-headed
; pattern is structure, not a dynamic call.
(: select-tail (-> Expression Expression))
(= (select-tail $value)
   (case $value
      ((($head $tail) $tail)
       ($other $other))))

!(test (select-tail (a b)) b)
!(test (select-tail x) x)

(: second (-> Expression %Undefined%))
(= (second $pair) (let ($a $b) $pair $b))

!(test (second (1 2)) 2)
