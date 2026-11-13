; Occupy a generated name before the first allocation. The allocation must
; skip it and also stay separate from a literal name written afterwards.
!(let $occupied (atom_concat "&space(" "1)")
   (add-atom $occupied (occupied)))
!(test (let $space (new-space)
         (let $_ (add-atom &space_1 (user-row))
           (collapse (match $space $row $row)))) ())

; Runtime handle text contains parentheses rather than a literal symbol name.
!(test (let $space (new-space)
         (is-member (atom_concat " " "") (atom_chars $space))) False)
!(test (let $space (new-space)
         (is-member (atom_concat "(" "") (atom_chars $space))) True)

; Space handles keep the same metatype and typed-call behavior as &self.
!(test (let $space (new-space) (is-space $space)) True)
!(test (get-metatype (new-space)) Symbol)
!(test (get-metatype &self) Symbol)
(: space-symbol (-> Symbol Bool))
(= (space-symbol $space) True)
!(test (space-symbol (new-space)) True)
!(test (space-symbol &self) True)
(: space-grounded (-> Grounded Bool))
(= (space-grounded $space) True)
!(test (collapse (space-grounded (new-space))) ())
!(test (collapse (space-grounded &self)) ())

; Empty spaces can be read, and each call creates an independent store.
!(test (let $space (new-space) (collapse (get-atoms $space))) ())
!(test (let $space (new-space) (collapse (match $space $x $x))) ())
!(test (let* (($first (new-space)) ($second (new-space)))
         (== $first $second)) False)
!(test (let* (($first (new-space)) ($second (new-space))
              ($_ (add-atom $first (value 7))))
         ((collapse (match $first (value $x) $x))
          (collapse (match $second (value $x) $x)))) ((7) ()))

; The operation also runs inside an ordinary function body.
(= (make-space) (new-space))
!(test (let $space (make-space)
         (let $_ (add-atom $space (inside))
           (collapse (match $space $x $x)))) ((inside)))
!(test (let* (($first (make-space)) ($second (make-space)))
         (== $first $second)) False)
