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
(: repr (-> $a String))
(: repra (-> $a String))
(: println! (-> $a Bool))

;; Type definitions of random generators (3-argument forms)
(: random-int (-> Number Number Number))
(: random-float (-> Number Number Number))
