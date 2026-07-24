; %%%% Type definitions of MeTTa built-in operators. %%%%

;; Type definitions of arithmetic operators
(: + (-> Number Number Number))
(: - (-> Number Number Number))
(: * (-> Number Number Number))
(: / (-> Number Number Number))
(: % (-> Number Number Number))

;; Type definitions of ordered comparison operators
(: < (-> Number Number Bool))
(: <= (-> Number Number Bool))
(: > (-> Number Number Bool))
(: >= (-> Number Number Bool))

;; Type definitions of equality and negated equality operators
(: = (-> $a $b Bool))
(: == (-> $a $b Bool))
(: != (-> $a $b Bool))

;; Type definitions of common mathematical functions
(: pow-math (-> Number Number Number))
(: sqrt-math (-> Number Number))
(: abs-math (-> Number Number))
(: log-math (-> Number Number Number))
(: trunc-math (-> Number Number))
(: ceil-math (-> Number Number))
(: floor-math (-> Number Number))
(: round-math (-> Number Number))
(: sin-math (-> Number Number))
(: asin-math (-> Number Number))
(: cos-math (-> Number Number))
(: acos-math (-> Number Number))
(: tan-math (-> Number Number))
(: atan-math (-> Number Number))
(: min-atom (-> $a Number))
(: max-atom (-> $a Number))
(: min (-> Number Number Number))
(: max (-> Number Number Number))
(: exp (-> Number Number))

;; Type definitions of isnan and isinf predicates
(: isnan-math (-> Number Bool))
(: isinf-math (-> Number Bool))

;; Type definitions of boolean operators
(: and (-> Bool Bool Bool))
(: or (-> Bool Bool Bool))
(: not (-> Bool Bool))
(: xor (-> Bool Bool Bool))

;; Type definitions of predicates and reflection helpers
(: is-var (-> $a Bool))
(: is-ground (-> $a Bool))
(: is-expr (-> $a Bool))
(: is-space (-> $a Bool))
(: is-member (-> $a $b Bool))
(: is-alpha-member (-> $a $b Bool))
(: =alpha (-> $a $b Bool))
(: =@= (-> $a $b Bool))
(: =? (-> $a $b Bool))

;; Type definitions of expression helpers with concrete outputs
(: size-atom (-> $a Number))
(: length (-> $a Number))
(: heap_size (-> $heap Number))
(: repr (-> $a String))
(: repra (-> $a String))
(: copy_term (-> $a $a))
(: println! (-> $a Bool))

;; Expression tails are always sequences, but their elements may be
;; heterogeneous source atoms.
(: cdr-atom (-> $expression (List %Undefined%)))

;; Collection operators always return sequences.  `collapse` may collect
;; heterogeneous alternatives.  `union-atom`, `append`, `subtraction-atom` and
;; `list_to_set` stay undeclared: a global (List $item) signature would reject
;; legal heterogeneous expressions (e.g. appending a number list to a string
;; list), so the checker types them contextually instead, preserving
;; compatible list element types without rejecting anything.
(: collapse (-> $value (List %Undefined%)))
(: superpose (-> (List $item) $item))

;; State mutation stores any value and reports success.
(: change-state! (-> $key $value Bool))

;; assert calls its (already evaluated) argument goal and yields true
(: assert (-> %Undefined% Bool))

;; empty is bottom: it produces no value, so its result fits every type
(: empty (-> $a))

;; Type definitions of random generators (3-argument forms)
(: random-int (-> Number Number Number))
(: random-float (-> Number Number Number))
