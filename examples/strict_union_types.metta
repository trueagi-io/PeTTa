!(import! &self (library lib_builtin_types))
!(import! &self (library lib_roman))

; Union types (| T1 T2): a heterogeneous list can declare its element type,
; and case/head patterns narrow to the member they select - no ascriptions,
; no wrapper constructors. Mirrors the PeTTaChainer execution-list shape.
(: TV Type)
(: mkTV (-> Number TV))

(: exec-tv (-> (| (CPU %Undefined% %Undefined% %Undefined%)
                  (: %Undefined% %Undefined% %Undefined% TV))
               TV))
(= (exec-tv $e)
   (case $e
      (((CPU $f $args $r) (mkTV 0))
       ((: $kb $proof $type $tv) $tv))))

!(test (exec-tv (CPU f args r)) (mkTV 0))
!(test (exec-tv (: kb proof type (mkTV 5))) (mkTV 5))

; head patterns narrow too, and the union element type flows through cons:
(: exec-tvs (-> (List (| (CPU %Undefined% %Undefined% %Undefined%)
                         (: %Undefined% %Undefined% %Undefined% TV)))
                (List TV)))
(= (exec-tvs ()) ())
(= (exec-tvs (cons $e $rest))
   (cons (exec-tv $e) (exec-tvs $rest)))

!(test (exec-tvs ((: kb p t (mkTV 7)) (CPU f a r)))
       ((mkTV 7) (mkTV 0)))
