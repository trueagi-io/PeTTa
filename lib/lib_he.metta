; %%%% Hyperon-Experimental compatibility library. %%%%
; Many of the included functions were considered ad hoc and
; straightforward to express directly in MeTTa itself:

;Equality and Reduction
; Branch-polymorphic helpers: then/else may have different types, so the
; output is declared dynamic (%Undefined%) rather than forced to one type.
(: if-equal (-[det]-> $a $b $then $else %Undefined%))
(= (if-equal $A $B $Then $Else)
   (if (== $A $B) $Then $Else))

(: if-equal2 (-[det]-> $a $b $then $else %Undefined%))
(= (if-equal2 $A $B $Then $Else)
   (if (== $A $B) $Then $Else))

(: assertEqual (-[det]-> $a $b Bool))
(= (assertEqual $A $B)
   (assert (== $A $B)))

(: assertAlphaEqual (-[det]-> $a $b Bool))
(= (assertAlphaEqual $A $B)
   (assert (=alpha $A $B)))

(: assertEqualToResult (-[det]-> $a $b Bool))
(= (assertEqualToResult $A $B)
   (assertEqual $A $B))

(: assertAlphaEqualToResult (-[det]-> $a $b Bool))
(= (assertAlphaEqualToResult $A $B)
   (assertAlphaEqual $A $B))

(: unify (-[nondet]-> Atom Atom Atom Atom %Undefined%))
(= (unify $space $pattern $then $else)
   (if (is-space $space)
       (let $temp (cut)
            (case (match $space $pattern $pattern)
                  (($pattern (eval $then))
                   (Empty (eval $else))))) (empty)))

(= (unify $a $b $then $else)
   (if (= $a $b)
       (eval $then)
       (eval $else)))

(: add-reduct (-[nondet]-> %Undefined% Atom Bool))
(= (add-reduct $space $f)
   (let* (($headbody (cdr-atom $f))
          ($head (car-atom $headbody))
          ($body (cdr-atom $headbody))
          ($bodyreduced (eval $body)))
         (add-atom $space (= $head $bodyreduced))))

;Error Handling
(: if-error (-[det]-> $x $a $b %Undefined%))
(= (if-error $X $A $B)
   (if (= $X (cons Error $_)) 
       $A $B))

(: return-on-error (-[det]-> $r $b %Undefined%))
(= (return-on-error $result $B)
   (if-error $result
             $result
             $B))

;Evaluation control
;The second parameter is applied to every element, so it is an arrow, not an
;anonymous $f: naming its shape is what lets the checker carry the closure's
;determinism through map-atom to this function's own effect.
(: for-each-in-atom (-[$e]-> $l (-[$e]-> $a $b) %Undefined%))
(= (for-each-in-atom $L $f)
   (map-atom $L $f))

(: evalc (-[nondet]-> Atom %Undefined% %Undefined%))
(= (evalc $expression &self)
   (eval $expression))

;Quoting
(: unquote (-[nondet]-> Atom %Undefined%))
(= (unquote (quote $A))
   (let $cut (cut) (eval $A)))
(= (unquote $A) (quote (unquote $A)))

(: noreduce-eq (-[det]-> Atom Atom Bool))
(= (noreduce-eq $A $B)
   (== $A $B))

;Types
(: is-function (-[det]-> $a Bool))
(= (is-function (-> $A $B)) (let $temp (cut) True))
(= (is-function $X) False)

;get-type collects its candidates through a soft cut, so it offers every one
;of them - and user refinement clauses add more. A wrapper cannot be more
;deterministic than what it wraps.
(: get-type-space (-[nondet]-> $s $a %Undefined%))
(= (get-type-space &self $x)
   (get-type $x))

(: match-types (-[det]-> $a $b $then $else %Undefined%))
(= (match-types $A $B $Then $Else)
   (if (== $A $B) $Then $Else))

(: match-type-or (-[det]-> $v $t1 $t2 %Undefined%))
(= (match-type-or $value $type1 $type2)
   (match-types $type1 $type2 True $value))
