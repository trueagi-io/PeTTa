; %%%% Type definitions of MeTTa built-in operators. %%%%
; This file contains type declarations for built-in MeTTa operators
; Import shared library types
(import! lib_types)

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
