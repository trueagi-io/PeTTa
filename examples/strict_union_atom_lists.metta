!(import! &self (library lib_builtin_types))

; union-atom stays undeclared (concatenating heterogeneous expressions is
; legal MeTTa), but when both arguments are provably (List T) with a common
; element type, the result is known to be (List T) - no residual guard.
(: Item Type)
(: item (-> Number Item))

(: source-items (-> Number (List Item)))
(= (source-items 0) (cons (item 0) ()))
(= (source-items 1) (cons (item 1) ()))

(: combine-direct (-> (List Item) (List Item) (List Item)))
(= (combine-direct $left $right)
   (union-atom $left $right))

!(test (combine-direct ((item 1)) ((item 2))) ((item 1) (item 2)))

; An ascribed literal keeps the author's stated type, so both if branches
; agree on (List Item) and the knowledge survives the let* binding.
(: combine-conditionally (-> Number Number (List Item)))
(= (combine-conditionally $key $wildcard-key)
   (let*
      (($direct (source-items $key))
       ($wildcard
          (if (== $key $wildcard-key)
              (the (List Item) ())
              (source-items $wildcard-key))))
      (union-atom $direct $wildcard)))

!(test (combine-conditionally 0 0) ((item 0)))
!(test (combine-conditionally 0 1) ((item 0) (item 1)))
