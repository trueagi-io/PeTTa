!(import! &self (library lib_he))
!(import! &self (library lib_builtin_types))

(: inc (-> Number Number))
(= (inc $x) (+ $x 1))

(: double (-> Number Number))
(= (double $x) (+ $x $x))

(: expects-number (-> Number Bool))
(= (expects-number $x) True)

(: expects-string (-> String Bool))
(= (expects-string $x) True)

; Successful pipeline: Number -> Number -> Number
!(test (double (inc 2)) 6)

; Successful handoff through another typed function
!(test (expects-number (inc 5)) True)

; Failing pipeline: Number output passed to String input -> rich error
!(test
   (let* (($expr (quote (expects-string (inc 5))))
          ($result (catch (eval $expr))))
         (car-atom $result))
   Error)

; Higher-order typed composition: map over numbers then increment
!(test (map-atom (1 2 3) $x (inc $x)) (2 3 4))

; Higher-order typed composition: fold with nested typed call in body
!(test (foldl-atom (1 2 3) 0 $acc $x (+ $acc (inc $x))) 9)

; Higher-order typed mismatch in pipeline: Number -> String -> rich error
!(test
   (let* (($expr (quote (map-atom (1 2) $x (expects-string (inc $x)))))
          ($result (catch (eval $expr))))
         (car-atom $result))
   Error)

; Explicit error outcome: arithmetic exception is captured as Error
!(test (if-error (catch (/ 1 0)) True False) True)

; Explicit error outcome inside higher-order body
!(test (if-error (catch (map-atom (1 2) $x (/ 1 (- $x $x)))) True False) True)
