; With a det closure, this declaration would instantiate to det. The Bool
; head coverage therefore owes the same exhaustiveness proof as -[det]->.
(: incomplete-effect-dispatch
   (-[$e]-> (-[$e]-> Number Number) Bool Number))
(= (incomplete-effect-dispatch $f True) ($f 1))
