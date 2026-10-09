(= (decode-form (left $value)) 1)
(= (decode-form (right $value)) 2)

(: compiler-form-selector (-[semidet]-> %Undefined% Number))
(= (compiler-form-selector $tag)
   (decode-form (data $tag 0)))
