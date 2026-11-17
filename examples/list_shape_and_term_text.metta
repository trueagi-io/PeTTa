; An unknown element leaves the outer list complete. An unknown tail does not.
!(test (get-metatype (quote (1 $element))) Expression)
!(test (let $cell (cons 1 $tail)
         ((is-expr $cell) (is-ground $cell) (collapse (get-metatype $cell))))
       (False False ()))

; The first length solution completes the open tail with ().
!(test (let $cell (cons 1 $tail)
         (let $size (once (size-atom $cell)) ($size $tail)))
       (1 ()))

; MeTTa source reads as an expression. Prolog term text preserves the cell.
!(test (parse "(cons 1 2)") (quote (cons 1 2)))
!(import_prolog_function atom_to_term)

(= (read-repra $text)
   (let $_ (atom_to_term $text $value) $value))

!(test (read-repra (repra (cons 1 2))) (cons 1 2))
!(test (read-repra (repra (quote (cons 1 2)))) (quote (cons 1 2)))

; The head and tail also occur outside the cell, so the two variable
; identities and their sharing must survive the round trip.
(= (open-cell-and-parts)
   (let $cell (cons $head $tail) (parts $cell $head $tail)))
!(test (read-repra (repra (open-cell-and-parts))) (open-cell-and-parts))
