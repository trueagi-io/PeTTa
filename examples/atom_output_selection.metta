; Each arity selects its own declaration.
(: atom_pick (-> Number Atom))
(= (atom_pick $x) (+ $x 1))
(= (atom_pick $x $y) (+ $x $y))
!(test (atom_pick 5) (quote (+ 5 1)))
!(test (let $f atom_pick ($f 5 6)) 11)

(: atom_zero (-> Atom))
(= (atom_zero) (+ 1 2))
(= (atom_zero $x) (+ $x 1))
!(test (atom_zero) (quote (+ 1 2)))
!(test (let $f atom_zero ($f 5)) 6)

; Unresolved names, relations and types are not concrete function declarations.
(: $target (-> Number Atom))
(= (atom_named $x) (if true $x 0))
!(test (let $f atom_named ($f 5)) 5)
!(remove-atom &self (: $target (-> Number Atom)))

($relation atom_relation (-> Number Atom))
(= (atom_relation $x) (if true $x 0))
!(test (let $f atom_relation ($f 5)) 5)
!(remove-atom &self ($relation atom_relation (-> Number Atom)))

(: atom_unresolved $type)
(= (atom_unresolved $x) (+ $x 1))
!(test (let $f atom_unresolved ($f 5)) 6)
