;Tracing helper
(: traceid (-> $a $a))
(= (traceid $x) (trace! $x $x))
(: tracem (-> $msg $a $a))
(= (tracem $msg $x) (trace! ($msg $x) $x))

;Higher order functions
(: map-flat (-> (-> $a $b) (List $a) (List $b)))
(= (map-flat $f ()) ())
(= (map-flat $f (cons $x $xs)) (cons ($f $x) (map-flat $f $xs)))

(: map-nested (-> Expression Expression Expression))
(= (map-nested $f ()) ())
(= (map-nested $f (cons $x $xs))
     (if (is-expr $x)
         (cons (map-nested $f $x) (map-nested $f $xs))
         (cons ($f $x) (map-nested $f $xs))))

(: fold-flat (-> Expression $a (List $b) $a))
(= (fold-flat $f $init ()) $init)
(= (fold-flat $f $init (cons $x $xs)) (fold-flat $f ($f $init $x) $xs))

(: foldr-flat (-> Expression $b (List $a) $b))
(= (foldr-flat $f $init ()) $init)
(= (foldr-flat $f $init (cons $x $xs)) ($f $x (foldr-flat $f $init $xs)))

(: fold-nested (-> Expression $a Expression $a))
(= (fold-nested $f $init ()) $init)
(= (fold-nested $f $init (cons $x $xs))
      (if (is-expr $x)
        (fold-nested $f (fold-nested $f $init $x) $xs)
        (fold-nested $f ($f $init $x) $xs)))

;Set operations on Expressions

;Intersection
(: (/?\) (-> Expression (List $a) (List $b) (List $a)))
(= (/?\ $pred () $b) ())
(= (/?\ $pred (cons $a $as) $bs)
      (if (fold-flat or False (map-flat ($pred $a) $bs))
         (cons $a (/?\ $pred $as $bs))
         (/?\ $pred $as $bs)))

(: (/=\) (-> (List $a) (List $a) (List $a)))
(= (/=\ $a $b) (/?\ = $a $b))
(: (/==\) (-> (List $a) (List $a) (List $a)))
(= (/==\ $a $b) (/?\ == $a $b))
(: (/=a\) (-> (List $a) (List $a) (List $a)))
(= (/=a\ $a $b) (/?\ =alpha $a $b))

;Subtraction
(: (\?) (-> Expression (List $a) (List $b) (List $a)))
(= (\? $pred () $bs) ())
(= (\? $pred (cons $a $as) $bs)
  (if (fold-flat or False (map-flat ($pred $a) $bs))
    (\? $pred $as $bs)
    (cons $a (\? $pred $as $bs))))

(: (\=) (-> (List $a) (List $a) (List $a)))
(= (\= $a $b) (\? = $a $b))
(: (\==) (-> (List $a) (List $a) (List $a)))
(= (\== $a $b) (\? == $a $b))
(: (\=a) (-> (List $a) (List $a) (List $a)))
(= (\=a $a $b) (\? =alpha $a $b))

;Unification
(: (\?/) (-> Expression (List $a) (List $b) (List $a)))
(= (\?/ $pred () $bs) $bs)
(= (\?/ $pred (cons $a $as) $bs)
  (if (fold-flat or False (map-flat ($pred $a) $bs))
    (\?/ $pred $as $bs)
    (cons $a (\?/ $pred $as $bs))))

(: (\=/) (-> (List $a) (List $a) (List $a)))
(= (\=/ $a $b) (\?/ = $a $b))
(: (\==/) (-> (List $a) (List $a) (List $a)))
(= (\==/ $a $b) (\?/ == $a $b))
(: (\=a/) (-> (List $a) (List $a) (List $a)))
(= (\=a/ $a $b) (\?/ =alpha $a $b))

;Function Composition

(: . (-> (-> $b $c) (-> $a $b) $a $c))
(= (. $f1 $f2 $arg) ($f1 ($f2 $arg)))

(: .. (-> (-> $b $c) (-> $a $b) $a $c))
(= (.. $f1 $f2 $arg) ($f1 ($f2 $arg)))

(: .: (-> (-> $c $d) (-> $a $b $c) $a $b $d))
(= (.: $f1 $f2 $arg1 $arg2) ($f1 ($f2 $arg1 $arg2)))

(: &&& (-> (-> $a $b) (-> $a $c) $a Expression))
(= (&&& $f1 $f2 $arg) (($f1 $arg) ($f2 $arg)))

(: &^& (-> (-> $a $b) (-> $a $b) $a $b))
(= (&^& $f1 $f2 $arg) (superpose (($f1 $arg) ($f2 $arg))))

;Reverse Function Matching
(: @ (-> $a $b $a))
(= (@ $a $b) (let $a $b $a))

(: head (-> $a (List $a)))
(= (head $x) (cons $x $xs))
(: tail (-> (List $a) (List $a)))
(= (tail $xs) (cons $x $xs))

(: mylast (-> $a (List $a)))
(= (mylast $x) (union-atom $xs ($x)))
(: init (-> (List $a) (List $a)))
(= (init $xs) (union-atom $xs ($x)))

(: rcons (-> (List $a) $a (List $a)))
(= (rcons $xs $x) (union-atom $xs ($x)))

;Utils

(: fst (-> Expression Expression))
(= (fst ($a $b)) $a)
(: snd (-> Expression Expression))
(= (snd ($a $b)) $b)

;(= (prog1 (cons $x $xs)) $x)
;(= (progn (rcons $xs $x)) $x)
