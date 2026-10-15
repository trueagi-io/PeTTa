!(import! &self (library lib_builtin_types))

; Arrow declarations are pre-cached before this source-ordered alias arrives.
(: make-late-row (-> Number String Row))
(: sum-late-rows (-> (List Row) Number))

(: Row (Alias (Number String)))

; The corrected declaration supplies the contextual product result.
(= (make-late-row $number $label)
   (data $number $label))

!(test (make-late-row 7 "seven") (7 "seven"))

; Recursive calls see (List (Number String)), not the stale atom Row.
(= (sum-late-rows ()) 0)
(= (sum-late-rows (cons ($number $_label) $rest))
   (+ $number (sum-late-rows $rest)))

!(test (sum-late-rows ((1 "one") (2 "two") (3 "three"))) 6)

; This definition is already compiled when LateRow arrives, so the alias cache
; must rewrite its declaration and send the clause through full recompilation.
(: late-row-id (-> LateRow LateRow))
(= (late-row-id $row) $row)

(: LateRow (Alias (Number String)))

!(test (late-row-id (9 "nine")) (9 "nine"))
