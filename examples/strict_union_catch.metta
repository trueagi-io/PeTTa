!(import! &self (library lib_roman))

(: numeric-distance2-result (-> (| Number (Error $detail $ctx)) Number))
(= (numeric-distance2-result $mdelta)
   (if (= $mdelta (cons Error $_)) 1000000.0
      (let $delta (checked-number $mdelta) (* $delta $delta))))

(: numeric-distance2 (-> $left $right Number))
(= (numeric-distance2 $a $b)
   (let $mdelta (catch (- (the Number $a) (the Number $b)))
      (numeric-distance2-result $mdelta)))

!(test (numeric-distance2 5 2) 9)
!(test (numeric-distance2 5 x) 1000000.0)
