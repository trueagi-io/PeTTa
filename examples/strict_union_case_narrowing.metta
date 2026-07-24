; Case branches narrow a union-typed scrutinee by constructor tag: in the
; (box $n) branch $n is Number statically - compiling under --strict is
; the proof that no residual check is generated.
(: Box Type)
(: box (-> Number Box))

(: consume (-> (| Box (List $none)) Number))
(= (consume $x)
   (case $x
     ((() 0)
      ((box $n) (+ $n 1)))))

!(test (consume (box 1)) 2)
!(test (consume ()) 0)
