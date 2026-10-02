; A change to a function's arrow declarations reaches the equations that were translated before it.
(: a A)
(: b B)

; A declaration written after its caller,
(= (late $x) (seen $x))
(= (late-caller $x) (late $x))
(= (late-outer $x) (late-caller $x))
!(test (collapse (late-outer b)) ((seen b)))
(: late (-> A %Undefined%))
!(test (collapse (late-outer b)) ())
!(test (collapse (late-outer a)) ((seen a)))
; and one that is removed, here by a pattern.
!(remove-atom &self (: late $type))
!(test (collapse (late-outer b)) ((seen b)))

; An argument declared Atom is passed unevaluated,
(= (quo $x) (quoted $x))
(= (quo-caller) (quo (+ 1 2)))
!(test (quo-caller) (quoted 3))
!(add-atom &self (: quo (-> Atom %Undefined%)))
!(test (quo-caller) (quote (quoted (+ 1 2))))
; and an Atom result holds the body.
(= (held $x) (+ $x 1))
!(test (held 6) 7)
!(add-atom &self (: held (-> Number Atom)))
!(test (held 6) (quote (+ 6 1)))
!(remove-atom &self (: held (-> Number Atom)))
!(test (held 6) 7)

; Declarations that arrive with an imported file do the same.
(= (sum3 $x) (+ $x 3))
!(import! &self (library lib_builtin_types))
!(test (collapse (sum3 4)) (7))
!(test (collapse (sum3 a)) ())

; So do specializations of higher-order functions,
(= (apply-to $f $x) ($f $x))
(= (target $x) (reached $x))
(= (through $x) (apply-to target $x))
!(test (collapse (through b)) ((reached b)))
!(add-atom &self (: target (-> A %Undefined%)))
!(test (collapse (through b)) ())
!(test (collapse (through a)) ((reached a)))
; and anonymous functions that were made earlier.
(= (inner $x) (kept $x))
!(bind! &made (new-space))
!(let $g (|-> ($x) (inner $x)) (add-atom &made (saved $g)))
!(test (collapse (match &made (saved $g) ($g b))) ((kept b)))
!(add-atom &self (: inner (-> A %Undefined%)))
!(test (collapse (match &made (saved $g) ($g b))) ())

; A declaration of another function leaves them as they were.
!(add-atom &self (: other (-> A %Undefined%)))
!(test (collapse (through a)) ((reached a)))

; A declaration that a stored equation cannot be translated with is refused, and nothing changes.
(= (taken $x) (took $x))
(= (taken-caller $x) (taken $x))
!(test (collapse (add-atom &self (: taken (-> A)))) ())
!(test (collapse (match &self (: taken $type) $type)) ())
!(test (taken-caller b) (took b))

; An equation that is already running finishes as it was translated, and its next run follows the change.
; This is what happens when an equation is added:
(= (grow) (let $u (add-atom &self (= (grow) new)) old))
!(test (collapse (grow)) (old))
!(test (collapse (grow)) (old new))
; and it is the same for a declaration:
(= (now $x) (was $x))
(= (running) (let $u (add-atom &self (: now (-> Atom %Undefined%))) (now (+ 1 2))))
!(test (running) (was 3))
!(test (running) (quote (was (+ 1 2))))
