!(import! &self (library lib_builtin_types))

; A parameter already declared as a positional product can be inserted into a
; nested data field without repeating its type with (the ...).
(: nest-product (-> (Number Number) ((Number Number) String)))
(= (nest-product $pair)
   (data $pair "kept"))

!(test (nest-product (1 2)) ((1 2) "kept"))

; fold-flat resolves its positional accumulator from the closure before
; translating the initializer. Contextual construction can therefore acquire
; the erased Count brands without tagged accumulator boilerplate.
!(import! &self (library lib_roman))

(: Count (Newtype Expression))

(: keep-counts (-[det]-> (Count Count) Number (Count Count)))
(= (keep-counts $state $_item) $state)

(: folded-counts (-> (List Number) (Count Count)))
(= (folded-counts $items)
   (fold-flat keep-counts (data zero zero) $items))

!(test (folded-counts (1 2 3)) (zero zero))

; The same propagation must work when the accumulator closure has one bound
; argument, as in (in_fold $formula).
(: keep-five-counts
   (-[det]-> Atom
              (Count Count Count Count Count)
              Number
              (Count Count Count Count Count)))
(= (keep-five-counts $_formula $state $_item) $state)

(: folded-five-counts
   (-> (List Number) (Count Count Count Count Count)))
(= (folded-five-counts $items)
   (fold-flat
      (keep-five-counts formula)
      (data a b c d e)
      $items))

!(test (folded-five-counts (1 2 3)) (a b c d e))

; compile.metta's shape: the positional initializer is produced in an earlier
; let* binding, the fold consumes it through a partial closure, and the outer
; function returns a tagged value rather than the accumulator directly.
(: FiveResult Type)
(: five-result
   (-> (Count Count Count Count Count) FiveResult))

(: staged-folded-five-counts
   (-> (List Number) FiveResult))
(= (staged-folded-five-counts $items)
   (let*
      (($init (data a b c d e))
       ($folded
          (fold-flat
             (keep-five-counts formula)
             $init
             $items)))
      (five-result $folded)))

!(test
   (staged-folded-five-counts (1 2 3))
   (five-result (a b c d e)))

; The contextual rule is declaration-driven, not keyed to fold-flat. This
; differently named higher-order function shares its state type between the
; closure, input, and result.
(: apply-twice (-> (-> $s $s) $s $s))
(= (apply-twice $f $x) ($f ($f $x)))

(: keep-count-pair (-> (Count Count) (Count Count)))
(= (keep-count-pair $state) $state)

(: twice-count-pair (-> (Count Count)))
(= (twice-count-pair)
   (apply-twice keep-count-pair (data count-a count-b)))

!(test (twice-count-pair) (count-a count-b))

; The source-level let* lookahead is likewise generic.
(: staged-twice-count-pair (-> (Count Count)))
(= (staged-twice-count-pair)
   (let*
      (($init (data count-c count-d))
       ($twice (apply-twice keep-count-pair $init)))
      $twice))

!(test (staged-twice-count-pair) (count-c count-d))

; An @ as-pattern binds both the whole positional product and its nested
; destructuring pattern. The inner fields retain their positional types.
(: at-product-fields (-> (Number String) Number))
(= (at-product-fields $row)
   (let (@ $_whole ($number $_text)) $row
      (+ $number 1)))

!(test (at-product-fields (7 "seven")) 8)

; The compile.metta shape nests @ below a cons destructuring pattern.
(: at-product-list-head
   (-> (List (Number String Bool Number)) Number))
(= (at-product-list-head $rows)
   (let
      (cons (@ $_whole ($first $_text $_flag $last)) $_rest)
      $rows
      (+ $first $last)))

!(test
   (at-product-list-head ((2 "two" True 5)))
   7)
