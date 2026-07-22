; %%%% Hyperon-Experimental compatibility library. %%%%
; Many of the included functions were considered ad hoc and
; straightforward to express directly in MeTTa itself:

;Equality and Reduction
; Branch-polymorphic helpers: then/else may have different types, so the
; output is declared dynamic (%Undefined%) rather than forced to one type.
(: if-equal (-> $a $b $then $else %Undefined%))
(= (if-equal $A $B $Then $Else)
   (if (== $A $B) $Then $Else))

(: if-equal2 (-> $a $b $then $else %Undefined%))
(= (if-equal2 $A $B $Then $Else)
   (if (== $A $B) $Then $Else))

(: assertEqual (-> $a $b Bool))
(= (assertEqual $A $B)
   (assert (== $A $B)))

(: assertAlphaEqual (-> $a $b Bool))
(= (assertAlphaEqual $A $B)
   (assert (=alpha $A $B)))

(: assertEqualToResult (-> $a $b Bool))
(= (assertEqualToResult $A $B)
   (assertEqual $A $B))

(: assertAlphaEqualToResult (-> $a $b Bool))
(= (assertAlphaEqualToResult $A $B)
   (assertAlphaEqual $A $B))

(: unify (-> Expression Expression Expression Expression %Undefined%))
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

(: add-reduct (-> %Undefined% Expression Bool))
(= (add-reduct $space $f)
   (let* (($headbody (cdr-atom $f))
          ($head (car-atom $headbody))
          ($body (cdr-atom $headbody))
          ($bodyreduced (eval $body)))
         (add-atom $space (= $head $bodyreduced))))

;Error Handling
(: if-error (-> $x $a $b %Undefined%))
(= (if-error $X $A $B)
   (if (= $X (cons Error $_)) 
       $A $B))

(: return-on-error (-> $r $b %Undefined%))
(= (return-on-error $result $B)
   (if-error $result
             $result
             $B))

;Evaluation control
(: for-each-in-atom (-> $l $f %Undefined%))
(= (for-each-in-atom $L $f)
   (map-atom $L $f))

(: evalc (-> Expression %Undefined% %Undefined%))
(= (evalc $expression &self)
   (eval $expression))

;Quoting
(: unquote (-> Expression %Undefined%))
(= (unquote (quote $A))
   (let $cut (cut) (eval $A)))
(= (unquote $A) (quote (unquote $A)))

(: noreduce-eq (-> Expression Expression Bool))
(= (noreduce-eq $A $B)
   (== $A $B))

;Types
(: is-function (-> $a Bool))
(= (is-function (-> $A $B)) (let $temp (cut) True))
(= (is-function $X) False)

(: get-type-space (-> $s $a %Undefined%))
(= (get-type-space &self $x)
   (get-type $x))

(: match-types (-> $a $b $then $else %Undefined%))
(= (match-types $A $B $Then $Else)
   (if (== $A $B) $Then $Else))

(: match-type-or (-> $v $t1 $t2 %Undefined%))
(= (match-type-or $value $type1 $type2)
   (match-types $type1 $type2 True $value))
