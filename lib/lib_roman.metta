;Tracing helper
(: traceid (-[det]-> $a $a))
(= (traceid $x) (trace! $x $x))
(: tracem (-[det]-> $msg $a $a))
(= (tracem $msg $x) (trace! (quote ($msg $x)) $x))

;Higher order functions
(: map-flat (-[$e]-> (-[$e]-> $a $b) (List $a) (List $b)))
(= (map-flat $f ()) ())
(= (map-flat $f (cons $x $xs)) (cons ($f $x) (map-flat $f $xs)))

(: map-nested (-[$e]-> (-[$e]-> $a $b) Expression Expression))
(= (map-nested $f ()) ())
(= (map-nested $f (cons $x $xs))
     (if (is-expr $x)
         (cons (map-nested $f $x) (map-nested $f $xs))
         (cons ($f $x) (map-nested $f $xs))))

(: fold-flat (-[$e]-> (-[$e]-> $acc $item $acc) $acc (List $item) $acc))
(= (fold-flat $f $init ()) $init)
(= (fold-flat $f $init (cons $x $xs)) (fold-flat $f ($f $init $x) $xs))

(: foldr-flat (-[$e]-> (-[$e]-> $item $acc $acc) $acc (List $item) $acc))
(= (foldr-flat $f $init ()) $init)
(= (foldr-flat $f $init (cons $x $xs)) ($f $x (foldr-flat $f $init $xs)))

(: fold-nested (-[$e]-> (-[$e]-> $acc $item $acc) $acc Expression $acc))
(= (fold-nested $f $init ()) $init)
(= (fold-nested $f $init (cons $x $xs))
      (if (is-expr $x)
        (fold-nested $f (fold-nested $f $init $x) $xs)
        (fold-nested $f ($f $init $x) $xs)))

;Set operations on Expressions

;or/2 is a boolean GENERATOR on an unbound operand - bool/1 enumerates the
;type rather than testing it - so it is nondet, and passing it as a fold's
;accumulator hands that nondeterminism to every function below. These
;membership tests only ever fold over Bools that have already been computed
;by $pred, so what they need is the restriction of or to bound operands:
;one clause, one answer, and it says so in its arrow.
(: bool-or (-[det]-> Bool Bool Bool))
(= (bool-or $seen $next) (if $seen True $next))

;Intersection
(: /?\ (-[$e]-> (-[$e]-> $a $b Bool) (List $a) (List $b) (List $a)))
(= (/?\ $pred () $b) ())
(= (/?\ $pred (cons $a $as) $bs)
      (if (fold-flat bool-or False (map-flat ($pred $a) $bs))
         (cons $a (/?\ $pred $as $bs))
         (/?\ $pred $as $bs)))

(: /=\ (-[det]-> (List $a) (List $a) (List $a)))
(= (/=\ $a $b) (/?\ = $a $b))
(: /==\ (-[det]-> (List $a) (List $a) (List $a)))
(= (/==\ $a $b) (/?\ == $a $b))
(: /=a\ (-[det]-> (List $a) (List $a) (List $a)))
(= (/=a\ $a $b) (/?\ =alpha $a $b))

;Subtraction
(: \? (-[$e]-> (-[$e]-> $a $b Bool) (List $a) (List $b) (List $a)))
(= (\? $pred () $bs) ())
(= (\? $pred (cons $a $as) $bs)
  (if (fold-flat bool-or False (map-flat ($pred $a) $bs))
    (\? $pred $as $bs)
    (cons $a (\? $pred $as $bs))))

(: \= (-[det]-> (List $a) (List $a) (List $a)))
(= (\= $a $b) (\? = $a $b))
(: \== (-[det]-> (List $a) (List $a) (List $a)))
(= (\== $a $b) (\? == $a $b))
(: \=a (-[det]-> (List $a) (List $a) (List $a)))
(= (\=a $a $b) (\? =alpha $a $b))

;Unification
(: \?/ (-[$e]-> (-[$e]-> $a $b Bool) (List $a) (List $b) (List $a)))
(= (\?/ $pred () $bs) $bs)
(= (\?/ $pred (cons $a $as) $bs)
  (if (fold-flat bool-or False (map-flat ($pred $a) $bs))
    (\?/ $pred $as $bs)
    (cons $a (\?/ $pred $as $bs))))

(: \=/ (-[det]-> (List $a) (List $a) (List $a)))
(= (\=/ $a $b) (\?/ = $a $b))
(: \==/ (-[det]-> (List $a) (List $a) (List $a)))
(= (\==/ $a $b) (\?/ == $a $b))
(: \=a/ (-[det]-> (List $a) (List $a) (List $a)))
(= (\=a/ $a $b) (\?/ =alpha $a $b))

;Function Composition

(: . (-[$e]-> (-[$e]-> $b $c) (-[$e]-> $a $b) $a $c))
(= (. $f1 $f2 $arg) ($f1 ($f2 $arg)))

(: .. (-[$e]-> (-[$e]-> $b $c) (-[$e]-> $a $b) $a $c))
(= (.. $f1 $f2 $arg) ($f1 ($f2 $arg)))

(: .: (-[$e]-> (-[$e]-> $c $d) (-[$e]-> $a $b $c) $a $b $d))
(= (.: $f1 $f2 $arg1 $arg2) ($f1 ($f2 $arg1 $arg2)))

(: &&& (-[$e]-> (-[$e]-> $a $b) (-[$e]-> $a $c) $a Expression))
(= (&&& $f1 $f2 $arg) (($f1 $arg) ($f2 $arg)))

(: &^& (-[nondet]-> (-[nondet]-> $a $b) (-[nondet]-> $a $b) $a $b))
(= (&^& $f1 $f2 $arg) (superpose (($f1 $arg) ($f2 $arg))))

;Error handling

(: if-error (-[det]-> $value $result $result $result))
(= (if-error $value $then $else)
   (if (= $value (cons Error $_)) $then $else))

(: checked-number (-[det]-> %Undefined% Number))
(= (checked-number $value) (the Number $value))

(: checked-bool (-[det]-> %Undefined% Bool))
(= (checked-bool $value) (the Bool $value))

(: if-error-number (-[det]-> %Undefined% Number Number))
(= (if-error-number $value $fallback)
   (if (= $value (cons Error $_)) $fallback (checked-number $value)))

(: if-error-bool (-[det]-> %Undefined% Bool Bool))
(= (if-error-bool $value $fallback)
   (if (= $value (cons Error $_)) $fallback (checked-bool $value)))

;Reverse Function Matching
(: @ (-[det]-> $a $b $a))
(= (@ $a $b) (let $a $b $a))

(: head (-[det]-> $a %Undefined%))
(= (head $x) (cons $x $xs))
(: tail (-[det]-> (List $a) %Undefined%))
(= (tail $xs) (cons $x $xs))

(: mylast (-[nondet]-> $a %Undefined%))
(= (mylast $x) (union-atom $xs ($x)))
(: init (-[nondet]-> (List $a) %Undefined%))
(= (init $xs) (union-atom $xs ($x)))

;union-atom is append, and append INVERTS: with an unbound first list it
;enumerates every split, which is exactly what makes (let (rcons $xs $x)
;(1 2 3) ...) solve for the front and the last element. So rcons is nondet
;for the same reason mylast and init above are, and says so explicitly.
(: rcons (-[nondet]-> (List $a) $a (List $a)))
(= (rcons $xs $x) (union-atom $xs ($x)))

;Utils

(: fst (-[det]-> ($a $b) $a))
(= (fst ($a $b)) $a)
(: snd (-[det]-> ($a $b) $b))
(= (snd ($a $b)) $b)

; Pair utilities: results are pairs whose heads may themselves be applied,
; so outputs stay dynamic and determinism is unprovable for the appliers.
(: first (-[nondet]-> (-[nondet]-> $a $c) ($a $b) %Undefined%))
(= (first $f ($a $b)) (($f $a) $b))
(: second (-[nondet]-> (-[nondet]-> $b $c) ($a $b) %Undefined%))
(= (second $f ($a $b)) ($a ($f $b)))

(: cns (-[det]-> ($a $b) %Undefined%))
(= (cns ($a $b)) (cons $a $b))

(: flip (-[nondet]-> ($a $b) %Undefined%))
(= (flip ($a $b)) ($b $a))

;(= (prog1 (cons $x $xs)) $x)
;(= (progn (rcons $xs $x)) $x)
