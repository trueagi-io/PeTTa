; A constructor-tagged head pattern also parses as a plain list, so it could
; shape-match a (List ...) union member too. The constructor tag is the
; discriminator: (box $pair) narrows to Box and $pair keeps its field type.

(: Box Type)
(: box (-> (Number String) Box))

(: consume-pair (-> (Number String) Number))
(= (consume-pair ($_number $_string)) 0)

(: consume-box (-> (| Box (List $none)) Number))
(= (consume-box (box $pair))
   (consume-pair $pair))
(= (consume-box ()) 0)

!(test (consume-box (box (7 "x"))) 0)
!(test (consume-box ()) 0)
