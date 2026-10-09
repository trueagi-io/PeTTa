; A declared type variable is a promise to every caller, wherever it sits in
; the argument type. (List $a) says "sumh works for a list of ANY element
; type", so no call site is checked on the element - but the body adds the head
; to a number, which only works for (List Number).
;
; Every call site gets a FRESH COPY of the declaration, so the binding the body
; makes here reaches nobody: it does not check the element type, it ELIDES the
; check. (sumh (cons "x" ())) used to compile with zero runtime checks even
; under --strict and print 121, because SWI reads a one-character string as its
; character code and adds 1 to it.
;
; The declaration has to name the type its implementation needs:
; (-> (List Number) Number).
(: sumh (-> (List $a) Number))
(= (sumh (cons $h $t)) (+ $h 1))

!(sumh (cons 5 ()))
