; The explicit brand covers a control-flow merge ONLY when every branch is
; admissible for the brand's representation. With a concrete representation
; (Number), the literal branch (left) is not a Number, so the certification
; rejects it - the brand rule is not loosened into "accept any merge".

(: Box (Newtype Number))

(: choose-box (-> Bool Box))
(= (choose-box $flag)
   (brand Box
      (if $flag
         1
         (left))))

!(test (choose-box true) 1)
