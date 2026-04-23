!(import! &self lib/lib_builtin_types)

;; Test type definitions of arithmetic operators
!(test (get-type +) (-> Number Number Number))
!(test (get-type -) (-> Number Number Number))
!(test (get-type *) (-> Number Number Number))
!(test (get-type /) (-> Number Number Number))
!(test (get-type %) (-> Number Number Number))

;; Test type definitions of comparison operators
!(test (get-type <) (-> Number Number Bool))
!(test (get-type <=) (-> Number Number Bool))
!(test (get-type >) (-> Number Number Bool))
!(test (get-type >=) (-> Number Number Bool))

;; Type definitions of equality and negated equality operators
!(test (get-type ==) (-> $a $b Bool))
!(test (get-type !=) (-> $a $b Bool))

;; Test type definitions of common mathematical functions
!(test (get-type pow-math) (-> Number Number Number))
!(test (get-type sqrt-math) (-> Number Number))
!(test (get-type abs-math) (-> Number Number))
!(test (get-type log-math) (-> Number Number Number))
!(test (get-type trunc-math) (-> Number Number))
!(test (get-type ceil-math) (-> Number Number))
!(test (get-type floor-math) (-> Number Number))
!(test (get-type round-math) (-> Number Number))
!(test (get-type sin-math) (-> Number Number))
!(test (get-type asin-math) (-> Number Number))
!(test (get-type cos-math) (-> Number Number))
!(test (get-type acos-math) (-> Number Number))
!(test (get-type tan-math) (-> Number Number))
!(test (get-type atan-math) (-> Number Number))
!(test (get-type min-atom) (-> $a Number))
!(test (get-type max-atom) (-> $a Number))
!(test (get-type min) (-> Number Number Number))
!(test (get-type max) (-> Number Number Number))
!(test (get-type exp) (-> Number Number))

;; Test type definitions of isnan and isinf predicates
!(test (get-type isnan-math) (-> Number Bool))
!(test (get-type isinf-math) (-> Number Bool))

;; Type definitions of boolean operators
!(test (get-type and) (-> Bool Bool Bool))
!(test (get-type or) (-> Bool Bool Bool))
!(test (get-type not) (-> Bool Bool))
!(test (get-type xor) (-> Bool Bool Bool))
