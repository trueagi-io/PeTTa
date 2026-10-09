(: same-type (-> $t $t Bool))
(= (same-type $a $b) True)

(: num-str (-> Number String Bool))
(= (num-str $a $b) True)

!(test (same-type 1 2) True)
!(test (same-type "x" "y") True)
!(test (num-str 1 "x") True)

; Deferred requirements are conjunctive. With no binding forced here, both
; remain attached to the shared variable and the call itself can return.
!(test (collapse (num-str $x $x)) (True))
