(: same-type (-> $t $t Bool))
(= (same-type $a $b) True)

(: num-str (-> Number String Bool))
(= (num-str $a $b) True)

!(test (same-type 1 2) True)
!(test (same-type "x" "y") True)
!(test (num-str 1 "x") True)

; The same variable cannot satisfy both Number and String in one call.
!(test (collapse (num-str $x $x)) ())
