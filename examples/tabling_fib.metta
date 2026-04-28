; Tabling Fibonacci Example
; =========================
; This example demonstrates the use of tabling for memoization.
; Without tabling, fibonacci has exponential time complexity.
; With tabling, it becomes linear.

; Import tabling support
!(import! &self (library lib_tabling.metta))

; Define fibonacci function
(= (fib $n) 
   (case $n 
      (0 1)
      (1 1)
      (_ (+ (fib (- $n 1)) (fib (- $n 2))))))

; Test without tabling first
!(fib 10)

; Enable tabling for better performance on subsequent calls
!(tabled (fib 10))

; Test with larger numbers - these benefit from tabling
!(fib 15)
!(fib 20)
