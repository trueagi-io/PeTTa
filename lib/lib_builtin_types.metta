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
(: implies (-> Bool Bool Bool))

;; Type definitions of predicates and reflection helpers
(: is-var (-> $a Bool))
(: is-ground (-> $a Bool))
(: is-expr (-> $a Bool))
(: is-space (-> $a Bool))
(: is-member (-> $a $b Bool))
(: is-alpha-member (-> $a $b Bool))
(: =alpha (-> $a $b Bool))
(: =@= (-> $a $b Bool))
(: @< (-> $a $b Bool))
(: @> (-> $a $b Bool))
(: @<= (-> $a $b Bool))
(: @>= (-> $a $b Bool))
(: =? (-> $a $b Bool))

;; Type definitions of expression helpers with concrete outputs
(: size-atom (-> $a Number))
(: length (-> $a Number))
(: heap_size (-> $heap Number))
(: repr (-> $a String))
(: repra (-> $a String))
(: copy_term (-> $a $a))
(: println! (-> $a Bool))

;; The list ACCESSORS - car-atom, cdr-atom, first, last - stay undeclared for
;; the same reason the list constructors do, and with a sharper consequence.
;; (: cdr-atom (-> $expression (List %Undefined%))) used to live here, and a
;; declaration is consulted BEFORE the contextual typing in untyped_call_out/3,
;; so it did not merely fail to help: it replaced whatever the argument knew
;; with (List %Undefined%). (cdr-atom $xs) on a (List Choice) came back as a
;; list of nothing in particular, every later use of an element needed a
;; runtime guard, and --strict rejected the guard it could not discharge. The
;; checker now derives (List T) -> T for car-atom/first/last and (List T) for
;; cdr-atom, keeping the (List %Undefined%) floor when the element type is not
;; known.

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

;; Trusted foreign promises: determinism arrows for Prolog predicates called
;; through (callPredicate (Predicate (g ...))). No MeTTa clauses exist for
;; these symbols, so the arrows are BELIEVED, not validated - the same
;; standing a -[nondet]-> on a reflective wrapper has. Declare further
;; Prolog predicates the same way in user code when callPredicate needs
;; their determinism.
(: assertz (-[det]-> $clause $ref Bool))
(: erase (-[det]-> $ref Bool))
