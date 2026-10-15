!(import! &self (library lib_builtin_types))

; (data ...) constructs an expression without dispatching its first field.
; The declared positional result type is pushed into each result branch.
(: choose-pair (-> Bool (Number String)))
(= (choose-pair $pick-first)
   (if $pick-first
       (data (+ 1 1) "first")
       (let $n (+ 2 1)
         (data $n "second"))))

!(test (choose-pair True) (2 "first"))
!(test (choose-pair False) (3 "second"))

; The same expectation reaches case result branches.
(: case-pair (-> Bool (Number Number)))
(= (case-pair $choice)
   (case $choice
     ((True (data 1 2))
      (False (data 3 4)))))

!(test (case-pair True) (1 2))
!(test (case-pair False) (3 4))

; Nested positional products receive their own field expectations.
(: nested-pair (-> ((Number Number) String)))
(= (nested-pair)
   (data (data 8 9) "nested"))

!(test (nested-pair) ((8 9) "nested"))

; A variable in head position is still data, even when its type could admit a
; callable value. This is the ambiguity the explicit form removes.
(: prepend-field (-[det]-> Atom Number (Atom Number)))
(= (prepend-field $head $number)
   (data $head $number))

!(test (prepend-field label 7) (label 7))

; The construct is unambiguous without a surrounding result declaration too:
; a function-valued first field is retained rather than invoked.
!(test (let $head + (data $head 7)) (quote (+ 7)))
