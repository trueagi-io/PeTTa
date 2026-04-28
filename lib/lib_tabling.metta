; PeTTa Standard Library - Tabling Support
; ========================================
; This module provides Prolog tabling (memoization) support for MeTTa.
; Tabling enables efficient recursive queries by caching intermediate results.
;
; Usage:
;   !(import! &self (library lib_tabling.metta))
;   
;   ; Table a recursive function
;   (= (fib $n) 
;      (case $n 
;         (0 1)
;         (1 1)
;         (_ (+ (fib (- $n 1)) (fib (- $n 2))))))
;   
;   ; Enable tabling for performance
;   (tabled (fib 10))
;

; Helper to inject raw Prolog code
(= (injectPrologCode $code) 
   (progn 
     (translatePredicate (open_string $code $S))
     (translatePredicate (load_files user ((Predicate (stream $S)))))
     True))

; Main tabling operator
; Transforms a MeTTa function into a tabled Prolog predicate
(: tabled (-> Expression Atom))
(= (tabled $call) 
   (progn 
     ; Extract function name
     (translatePredicate (atom_string (car-atom $call) $strname))
     
     ; Calculate arity (number of arguments)
     (translatePredicate (atom_string (+ 1 (length (cdr-atom $call))) $strarity))
     
     ; Generate Prolog tabling directive
     (translatePredicate 
       (atomics_to_string ":- table " $strname "/" $strarity "." $code))
     
     ; Inject the directive into Prolog
     (injectPrologCode $code)))

; Example usage (commented out):
; 
; ; Define fibonacci
; (= (fib $n) 
;    (case $n 
;       (0 1)
;       (1 1)
;       (_ (+ (fib (- $n 1)) (fib (- $n 2))))))
; 
; ; Enable tabling for better performance
; ; (tabled (fib 5))
