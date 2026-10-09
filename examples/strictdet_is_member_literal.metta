; is-member is det when the probe is bound (here an enforced-bound direct
; parameter) and the list is a ground, duplicate-free literal: member/2 is then
; a test with at most one success, and the two mutually exclusive clauses give
; exactly one answer. The runtime predicate's generator mode is untouched.
(: known-name? (-[det]-> Atom Bool))
(= (known-name? $x) (is-member $x (alpha beta gamma)))

!(test (known-name? beta) true)
!(test (known-name? delta) false)

; Nested data literals are values too, as probe and as list elements.
(: has-b2? (-[det]-> Bool))
(= (has-b2?) (is-member (b 2) ((a 1) (b 2))))
!(test (has-b2?) true)
