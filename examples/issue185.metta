;; Test for issue #185
;; Atom type should pass argument as data (not evaluate)
;; case Empty default should match the empty value

;; Original bug: Atom type with case Empty
(: foo (-> Atom %Undefined%))
(= (foo $x)
   (case $x
     ((Empty Ok)
      ($_ Nok))))

!(test (foo (empty)) Ok)
!(test (foo 1) Nok)

;; Case-order behavior: Empty matches the empty value
;; regardless of where it appears in the case branches

(= (foo2 $x)
   (case $x
     (($_ Nok)
      (Empty Ok))))

!(test (foo2 (empty)) Ok)
!(test (foo2 1) Nok)
