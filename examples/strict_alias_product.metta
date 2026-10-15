!(import! &self (library lib_builtin_types))

; Structural aliases erase to their positional representation. The expanded
; result expectation reaches data fields through both branches of the if.
(: Row (Alias (Number String)))

(: choose-row (-> Bool Row))
(= (choose-row $first)
   (if $first
       (data (+ 1 1) "first")
       (data (+ 2 1) "second")))

!(test (choose-row True) (2 "first"))
!(test (choose-row False) (3 "second"))

; The same alias supplies positional field types to a parameter.
(: row-number (-> Row Number))
(= (row-number ($number $_label)) $number)

!(test (row-number (7 "seven")) 7)

; Alias representations may use earlier aliases inside other type forms.
(: Rows (Alias (List Row)))

(: singleton-row (-> Row Rows))
(= (singleton-row $row) (cons $row ()))

!(test (singleton-row (8 "eight")) ((8 "eight")))

; `the` receives the expanded product type too.
(: ascribed-row (-> Row))
(= (ascribed-row) (the Row (data 9 "nine")))

!(test (ascribed-row) (9 "nine"))
