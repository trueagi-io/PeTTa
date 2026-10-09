; A head pattern with a VARIABLE TAIL. (cons Premises $p) compiles to the
; partial list ['Premises'|$p], whose arity is not known at compile time - and
; a partial list is not an n-argument constructor application, so the value
; typing must decline to type it rather than try every arity.
;
; It used to try every arity. length/2 on a partial list is a generator, so
; the constructor-application clause of value_candidate_types/2 proposed
; N = 0, 1, 2, ... and, finding no declaration at any of them, never stopped:
; this two-line file took 35 seconds and 2.2 GB before dying with "Stack limit
; exceeded". Parameter inference started descending into the FIELDS of a
; destructuring pattern, which is how a partial list first reached that clause
; from a nested position; the same pattern at the top of the head reached it
; from the start.
;
; Both positions are pinned below. The wall clock is the assertion that
; matters - a regression here is a compile that still produces the right
; answer, only after minutes and gigabytes - so the elapsed time of loading
; this file is checked as well. The bound is deliberately loose (the whole
; file compiles in about 0.2s) so that a loaded machine cannot trip it.
!(change-state! &t0 (current-time))

; nested: the partial list is a field of an enclosing pattern
(= (impl-premises (Implication (cons Premises $p))) $p)

; top level: the whole parameter is the partial list
(= (tail-of (cons Premises $p)) $p)

!(test (impl-premises (Implication (Premises a b))) (a b))
!(test (tail-of (Premises x y)) (x y))

!(test (< (- (current-time) (get-state &t0)) 30) True)
