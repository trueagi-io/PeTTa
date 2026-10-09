;; Test that space-based unify evaluates its then/else branches.
;; Inspired by pverify's add_c / add_v conflict checks which nest
;; unify calls in the else branch and return (Error ...) or () from
;; the then branch.
;;
;; Without the eval fix (lib_he.metta lines 24-25), space-based unify
;; returns unevaluated expressions such as (+ 1 2) instead of 3.

!(import! &self ../lib/lib_he)

;; --- Knowledge base atoms (like pverify's Constant/Var declarations) ---
(Constant wff (Type "$c"))
(Var x 0 (Type "$v"))

;; Test 1: then-branch needs eval (expression in matched case)
!(test (unify &self (Constant wff (Type "$c"))
         (Error (Constant wff) "already declared")
         ())
       (Error (Constant wff) "already declared"))

;; Test 2: else-branch needs eval (fallthrough to nested unify)
!(test (unify &self (Constant y (Type "$c"))
         (Error (Constant y) "already declared")
         (unify &self (Var y 0 (Type "$v"))
           (Error (Var y) "active variable conflict")
           ()))
       ())

;; Test 3: else-branch nested unify hits (real conflict chain)
!(test (unify &self (Constant x (Type "$c"))
         (Error (Constant x) "already declared")
         (unify &self (Var x 0 (Type "$v"))
           (Error (Var x) "active variable conflict")
           ()))
       (Error (Var x) "active variable conflict"))

;; Test 4: arithmetic in branches (minimal reproducer)
!(test (unify &self (Constant wff (Type "$c")) (+ 1 2) 0) 3)

;; Test 5: arithmetic in else-branch
!(test (unify &self (Constant NOSUCH (Type "$c")) 0 (+ 10 20)) 30)
