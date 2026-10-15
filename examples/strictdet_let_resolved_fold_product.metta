!(import! &self (library lib_builtin_types))
!(import! &self (library lib_roman))

; The accumulator type of fold-flat is parametric.  Its closure argument
; resolves that type to a concrete positional product at this call site, so
; destructuring the result into distinct fresh variables cannot fail.
(: keep-resolved-triple
   (-[det]-> (Number Number Number) Number (Number Number Number)))
(= (keep-resolved-triple $state $_item) $state)

(: resolved-fold-third
   (-[det]-> (List Number) Number))
(= (resolved-fold-third $items)
   (let
      ($first $second $third)
      (fold-flat keep-resolved-triple (data 1 2 3) $items)
      $third))

!(test (resolved-fold-third (4 5 6)) 3)
