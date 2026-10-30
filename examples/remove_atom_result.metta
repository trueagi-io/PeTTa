; remove-atom says whether it removed anything: True if a row of the space went,
; False if none did.
!(test (remove-atom &rows (foo 1)) False)
!(add-atom &rows (foo 1))
!(add-atom &rows (foo 2))
!(test (remove-atom &rows (foo 1)) True)
!(test (remove-atom &rows (foo 1)) False)
!(test (collapse (match &rows (foo $x) $x)) (2))

; A pattern removes every row it unifies with, and leaves its variables unbound:
!(add-atom &rows (foo 3))
!(test (remove-atom &rows (foo $x)) True)
!(test (collapse (match &rows (foo $x) $x)) ())
!(test (remove-atom &rows (foo $x)) False)
!(add-atom &rows (foo 4))
!(test (let $removed (remove-atom &rows (foo $y)) (is-var $y)) True)

; Numbers, strings and symbols report the same way:
!(add-atom &rows 42)
!(test (remove-atom &rows 42) True)
!(test (remove-atom &rows 42) False)

; A function atom reports the same way:
(= (f 1) 2)
!(test (remove-atom &self (= (f 1) 2)) True)
!(test (remove-atom &self (= (f 1) 2)) False)

; A function pattern can take a data row: (= $h 1) is data, its head being a
; variable, and removing (= (g $x) $b) takes it although g has no clause:
!(add-atom &other (= $h 1))
!(test (remove-atom &other (= (g $x) $b)) True)
!(test (collapse (match &other (= $h $y) $y)) ())

; Of several threads removing the same row, exactly one is told True:
!(add-atom &token token)
(= (claim $i) (if (remove-atom &token token) (add-atom &winners $i) ()))
!(hyperpose ((claim 1) (claim 2) (claim 3) (claim 4) (claim 5) (claim 6) (claim 7) (claim 8)))
!(test (size-atom (collapse (match &winners $w $w))) 1)

; Existing remove-all matching is retained, including duplicate stored rows:
!(add-atom &duplicates (item 1))
!(add-atom &duplicates (item 1))
!(test (remove-atom &duplicates (item 1)) True)
!(test (collapse (match &duplicates (item $x) $x)) ())

; Memoization still works when a definition exists only in another space.
; Skipping legacy function cleanup on an absent &self row duplicates this clause.
!(import! &self (library lib_memo))
!(add-atom &memo-other (= (answer-other $x) $x))
!(memoize answer-other)
!(test (collapse (answer-other 0)) (0))
