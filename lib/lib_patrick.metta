;General function composition:
(: compose (-> Atom _ _))
(= (compose (cons $f $rest) $args)
   (if (== $rest ())
       (if (== (length $args) 1) ;optimization for single arg, avoids
           (let ($arg) $args ($f $arg)) ;reduce for function call
           (let $func (cons $f $args) (reduce $func)))
       ($f (compose $rest $args))))

;Reverse Function Matching
(= (@ $a $b) (let $a $b $a))

;For loop
(: for (-> Atom _ Atom _))
(= (for $var $collection $body)
   (quote (let $var (superpose $collection)
                    $body)))
!(add-translator-rule! for)

;Iteration with function
(= (iterate $i $n $state $step)
   (if (== $n 0)
       $state
       (iterate (+ $i 1)
                (- $n 1)
                ($step $i $state)
                $step)))
