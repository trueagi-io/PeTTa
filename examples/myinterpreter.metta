(: myinterpreter (-> Atom %Undefined%))
(= (myinterpreter $code)
   (let $temp (println! ("Runtime-interpreting code" $code))
        (eval $code)))

(= (w) 42)
(= (v) 43)

!(test (myinterpreter (if (== 1 1) (w) (v))) 42)
!(test (myinterpreter (if (== 1 2) (w) (v))) 43)
