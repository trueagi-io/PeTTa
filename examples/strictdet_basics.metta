; Under --strict-det every arrow is explicit. Overlapping clause heads,
; superpose/match bodies, and dynamic eval are compile errors for -[det]->
; functions; nondeterminism must be visible in the arrow.

(: dec (-[det]-> Number Number))
(= (dec $n) (- $n 1))

(: countdown (-[det]-> Number Number))
(= (countdown $n) (if (< $n 1) 0 (countdown (dec $n))))

!(test (countdown 5) 0)

; Explicit nondeterminism: allowed, and visible in the type.
(: pick (-[nondet]-> Number Number))
(= (pick $n) (superpose (1 2 $n)))

!(test (collapse (pick 9)) (1 2 9))

; Higher-order functions are deterministic when their closure carries
; explicit det evidence:
!(test (foldl-atom (1 2 3) 0 +) 6)
!(test (map-atom (3 4 5) dec) (2 3 4))

; A nondet closure poisons the application point: wrapping it in a det
; function is rejected, declaring the wrapper nondet is fine.
(: spread (-[nondet]-> (-[nondet]-> Number Number) Number Number))
(= (spread $f $n) ($f $n))

!(test (collapse (spread pick 7)) (1 2 7))

; A variable head with a provably non-function type is data construction,
; not an application - deterministic like any other structure building:
(: make-pair-list (-[det]-> Number Number (List (Number Number))))
(= (make-pair-list $x $y) (cons ($x $y) ()))

!(test (make-pair-list 1 2) ((1 2)))

; case and let patterns are matched, not executed: a variable-headed
; pattern is structure, not a dynamic call.
(: select-tail (-[det]-> Expression Expression))
(= (select-tail $value)
   (case $value
      ((($head $tail) $tail)
       ($other $other))))

!(test (select-tail (a b)) b)
!(test (select-tail x) x)

; Expression admits non-pairs, so destructuring may fail.
(: second (-[semidet]-> Expression %Undefined%))
(= (second $pair) (let ($a $b) $pair $b))

!(test (second (1 2)) 2)
