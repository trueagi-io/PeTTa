(: for (-> Atom Atom Atom Atom))
(= (for $var $collection $body)
   (quote (let $var (superpose $collection)
                    $body)))

!(add-translator-rule! for)

(= (myfun $L)
   (for $x $L
        (if (== (% $x 2) 0)
            (even $x)
            (odd $x))))

!(test (collapse (myfun (3 4)))
       ((odd 3) (even 4)))
