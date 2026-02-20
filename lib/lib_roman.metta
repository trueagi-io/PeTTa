;Tracing helper
(= (traceid $x) (trace! $x $x))
(= (tracem $msg $x) (trace! ($msg $x) $x))

;Higher order functions
(= (map-flat $f ()) ())
(= (map-flat $f (cons $x $xs)) (cons ($f $x) (map-flat $f $xs)))

(= (map-nested $f ()) ())
(= (map-nested $f (cons $x $xs))
     (if (is-expr $x)
         (cons (map-nested $f $x) (map-nested $f $xs))
         (cons ($f $x) (map-nested $f $xs))))

(= (fold-flat $f $init ()) $init)
(= (fold-flat $f $init (cons $x $xs)) (fold-flat $f ($f $init $x) $xs))

(= (foldr-flat $f $init ()) $init)
(= (foldr-flat $f $init (cons $x $xs)) ($f $x (foldr-flat $f $init $xs)))

(= (fold-nested $f $init ()) $init)
(= (fold-nested $f $init (cons $x $xs))
      (if (is-expr $x)
        (fold-nested $f (fold-nested $f $init $x) $xs)
        (fold-nested $f ($f $init $x) $xs)))

;Set operations on Expressions

;Intersection
(= (/?\ $pred () $b) ())
(= (/?\ $pred (cons $a $as) $bs)
      (if (fold-flat or False (map-flat ($pred $a) $bs))
         (cons $a (/?\ $pred $as $bs))
         (/?\ $pred $as $bs)))

(= (/=\ $a $b) (/?\ = $a $b))
(= (/==\ $a $b) (/?\ == $a $b))
(= (/=a\ $a $b) (/?\ =alpha $a $b))

;Subtraction
(= (\? $pred () $bs) ())
(= (\? $pred (cons $a $as) $bs)
  (if (fold-flat or False (map-flat ($pred $a) $bs))
    (\? $pred $as $bs)
    (cons $a (\? $pred $as $bs))))

(= (\= $a $b) (\? = $a $b))
(= (\== $a $b) (\? == $a $b))
(= (\=a $a $b) (\? =alpha $a $b))

;Unification
(= (\?/ $pred () $bs) $bs)
(= (\?/ $pred (cons $a $as) $bs)
  (if (fold-flat or False (map-flat ($pred $a) $bs))
    (\?/ $pred $as $bs)
    (cons $a (\?/ $pred $as $bs))))

(= (\=/ $a $b) (\?/ = $a $b))
(= (\==/ $a $b) (\?/ == $a $b))
(= (\=a/ $a $b) (\?/ =alpha $a $b))

;Function Composition

(= (. $f1 $f2 $arg) ($f1 ($f2 $arg)))

(= (.. $f1 $f2 $arg) ($f1 ($f2 $arg)))

(= (.: $f1 $f2 $arg1 $arg2) ($f1 ($f2 $arg1 $arg2)))

(= (&&& $f1 $f2 $arg) (($f1 $arg) ($f2 $arg)))

(= (&^& $f1 $f2 $arg) (superpose (($f1 $arg) ($f2 $arg))))

;Reverse Function Matching
(= (@ $a $b) (let $a $b $a))

(= (head $x) (cons $x $xs))
(= (tail $xs) (cons $x $xs))

(= (mylast $x) (union-atom $xs ($x)))
(= (init $xs) (union-atom $xs ($x)))

(= (rcons $xs $x) (union-atom $xs ($x)))

;Utils

(= (fst ($a $b)) $a)
(= (snd ($a $b)) $b)

;(= (prog1 (cons $x $xs)) $x)
;(= (progn (rcons $xs $x)) $x)
