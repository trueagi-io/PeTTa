!(import! &self (library lib_spaces))

; car-atom, cdr-atom, first and last carry a (List T) argument's ELEMENT type
; through to their result. Only the list CONSTRUCTORS used to be typed
; contextually, and (: cdr-atom (-> $expression (List %Undefined%))) sat in
; front of that and replaced whatever the argument knew - so an element came
; back untyped, every use of one needed a runtime guard, and --strict rejected
; the guard it could not discharge.

(: Choice Type)
(: mk (-> Number Choice))
(: choice-num (-> Choice Number))
(= (choice-num (mk $n)) $n)

(: head-num (-> (List Choice) Number))
(= (head-num $xs) (choice-num (car-atom $xs)))
!(test (head-num ((mk 1) (mk 2) (mk 3))) 1)

(: second-num (-> (List Choice) Number))
(= (second-num $xs) (choice-num (car-atom (cdr-atom $xs))))
!(test (second-num ((mk 1) (mk 2) (mk 3))) 2)

(: last-num (-> (List Choice) Number))
(= (last-num $xs) (choice-num (last $xs)))
!(test (last-num ((mk 1) (mk 2) (mk 3))) 3)

; first/1 is the head of a two-element list, so its element type is the same
; projection. (lib_roman's first/2 is a different, pair-mapping function.)
(: pair-head-num (-> (List Choice) Number))
(= (pair-head-num $xs) (choice-num (first $xs)))
!(test (pair-head-num ((mk 4) (mk 5))) 4)

; The PeTTaChainer pattern: a match over a typed constructor field collapses
; to a (List Choice), and the accessor no longer throws the element type away.
!(bind! &config (new-space))
(: ChoiceRow Type)
(: choice-row (-> Choice ChoiceRow))

(: selected-choice (-> Choice))
(= (selected-choice)
   (let $matches
        (collapse (match &config (choice-row $c) $c))
        (if (== $matches ()) (mk 0) (car-atom $matches))))

!(add-atom &config (choice-row (mk 7)))
!(test (choice-num (selected-choice)) 7)
