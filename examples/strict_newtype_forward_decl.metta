; A Newtype declaration must be visible before function bodies are compiled,
; even when the declaration appears later in the file.
(: choose-box (-> Bool Box))
(= (choose-box $flag)
   (brand Box
      (if $flag
         (left)
         (right))))

(: Box (Newtype Expression))

!(test (choose-box true) (left))
!(test (choose-box false) (right))
