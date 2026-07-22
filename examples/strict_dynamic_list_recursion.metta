!(import! &self (library lib_builtin_types))

; (List %Undefined%) is a list of dynamically typed elements: consing ANY
; head onto such a list is still (List %Undefined%), so recursive rebuilding
; needs no residual guards even though item-key's output is %Undefined%.
(: item-key (-> %Undefined% %Undefined%))
(= (item-key $item) $item)

(: item-keys (-> (List %Undefined%) (List %Undefined%)))
(= (item-keys ()) ())
(= (item-keys (cons $item $rest))
   (cons (item-key $item) (item-keys $rest)))

!(test (item-keys (1 "two" three)) (1 "two" three))
!(test (item-keys ()) ())
