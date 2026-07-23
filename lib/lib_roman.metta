;Tracing helper
(: traceid (-> $a $a))
(= (traceid $x) (trace! $x $x))
(: tracem (-> $msg $a $a))
(= (tracem $msg $x) (trace! (quote ($msg $x)) $x))

;Higher order functions
(: map-flat (-> (-> $a $b) (List $a) (List $b)))
(= (map-flat $f ()) ())
(= (map-flat $f (cons $x $xs)) (cons ($f $x) (map-flat $f $xs)))

(: map-nested (-> (-> $a $b) Expression Expression))
(= (map-nested $f ()) ())
(= (map-nested $f (cons $x $xs))
     (if (is-expr $x)
         (cons (map-nested $f $x) (map-nested $f $xs))
         (cons ($f $x) (map-nested $f $xs))))

(: fold-flat (-> (-> $acc $item $acc) $acc (List $item) $acc))
(= (fold-flat $f $init ()) $init)
(= (fold-flat $f $init (cons $x $xs)) (fold-flat $f ($f $init $x) $xs))

(: foldr-flat (-> (-> $item $acc $acc) $acc (List $item) $acc))
(= (foldr-flat $f $init ()) $init)
(= (foldr-flat $f $init (cons $x $xs)) ($f $x (foldr-flat $f $init $xs)))

(: fold-nested (-> (-> $acc $item $acc) $acc Expression $acc))
(= (fold-nested $f $init ()) $init)
(= (fold-nested $f $init (cons $x $xs))
      (if (is-expr $x)
        (fold-nested $f (fold-nested $f $init $x) $xs)
        (fold-nested $f ($f $init $x) $xs)))

;Set operations on Expressions

;Intersection
(: /?\ (-> (-> $a $b Bool) (List $a) (List $b) (List $a)))
(= (/?\ $pred () $b) ())
(= (/?\ $pred (cons $a $as) $bs)
      (if (fold-flat or False (map-flat ($pred $a) $bs))
         (cons $a (/?\ $pred $as $bs))
         (/?\ $pred $as $bs)))

(: /=\ (-> (List $a) (List $a) (List $a)))
(= (/=\ $a $b) (/?\ = $a $b))
(: /==\ (-> (List $a) (List $a) (List $a)))
(= (/==\ $a $b) (/?\ == $a $b))
(: /=a\ (-> (List $a) (List $a) (List $a)))
(= (/=a\ $a $b) (/?\ =alpha $a $b))

;Subtraction
(: \? (-> (-> $a $b Bool) (List $a) (List $b) (List $a)))
(= (\? $pred () $bs) ())
(= (\? $pred (cons $a $as) $bs)
  (if (fold-flat or False (map-flat ($pred $a) $bs))
    (\? $pred $as $bs)
    (cons $a (\? $pred $as $bs))))

(: \= (-> (List $a) (List $a) (List $a)))
(= (\= $a $b) (\? = $a $b))
(: \== (-> (List $a) (List $a) (List $a)))
(= (\== $a $b) (\? == $a $b))
(: \=a (-> (List $a) (List $a) (List $a)))
(= (\=a $a $b) (\? =alpha $a $b))

;Unification
(: \?/ (-> (-> $a $b Bool) (List $a) (List $b) (List $a)))
(= (\?/ $pred () $bs) $bs)
(= (\?/ $pred (cons $a $as) $bs)
  (if (fold-flat or False (map-flat ($pred $a) $bs))
    (\?/ $pred $as $bs)
    (cons $a (\?/ $pred $as $bs))))

(: \=/ (-> (List $a) (List $a) (List $a)))
(= (\=/ $a $b) (\?/ = $a $b))
(: \==/ (-> (List $a) (List $a) (List $a)))
(= (\==/ $a $b) (\?/ == $a $b))
(: \=a/ (-> (List $a) (List $a) (List $a)))
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

(: &^& ((-> $a $b) (-> $a $b) $a -[nondet]-> $b))
(= (&^& $f1 $f2 $arg) (superpose (($f1 $arg) ($f2 $arg))))

;Error handling

(: if-error (-> $value $result $result $result))
(= (if-error $value $then $else)
   (if (= $value (cons Error $_)) $then $else))

(: checked-number (-> %Undefined% Number))
(= (checked-number $value) (the Number $value))

(: checked-bool (-> %Undefined% Bool))
(= (checked-bool $value) (the Bool $value))

(: if-error-number (-> %Undefined% Number Number))
(= (if-error-number $value $fallback)
   (if (= $value (cons Error $_)) $fallback (checked-number $value)))

(: if-error-bool (-> %Undefined% Bool Bool))
(= (if-error-bool $value $fallback)
   (if (= $value (cons Error $_)) $fallback (checked-bool $value)))

;Reverse Function Matching
(: @ (-> $a $b $a))
(= (@ $a $b) (let $a $b $a))

(: head (-> $a %Undefined%))
(= (head $x) (cons $x $xs))
(: tail (-> (List $a) %Undefined%))
(= (tail $xs) (cons $x $xs))

(: mylast ($a -[nondet]-> %Undefined%))
(= (mylast $x) (union-atom $xs ($x)))
(: init ((List $a) -[nondet]-> %Undefined%))
(= (init $xs) (union-atom $xs ($x)))

(: rcons (-> (List $a) $a (List $a)))
(= (rcons $xs $x) (union-atom $xs ($x)))

;Utils

(: fst (-> ($a $b) $a))
(= (fst ($a $b)) $a)
(: snd (-> ($a $b) $b))
(= (snd ($a $b)) $b)

; Pair utilities: results are pairs whose heads may themselves be applied,
; so outputs stay dynamic and determinism is unprovable for the appliers.
(: first ((-> $a $c) ($a $b) -[nondet]-> %Undefined%))
(= (first $f ($a $b)) (($f $a) $b))
(: second ((-> $b $c) ($a $b) -[nondet]-> %Undefined%))
(= (second $f ($a $b)) ($a ($f $b)))

(: cns (-> ($a $b) %Undefined%))
(= (cns ($a $b)) (cons $a $b))

(: flip (($a $b) -[nondet]-> %Undefined%))
(= (flip ($a $b)) ($b $a))

;(= (prog1 (cons $x $xs)) $x)
;(= (progn (rcons $xs $x)) $x)
