;; Regression test for a known incompatibility between lib_memo and
;; PeTTa's printer when a memoized function uses trace! on a term that
;; contains a fresh variable.
;;
;; Background: PeTTa's swrite/2 attaches a petta_swrite_name attribute
;; to any unbound variable it serializes (so it can give it a stable
;; display name like $_0). lib_memo's canonicalize_args_key/2 calls
;; numbervars/3 on a copy of the call args to normalize the cache key;
;; numbervars rejects terms that contain attributed variables, which
;; would crash with:
;;
;;   numbervars/4: Type error: `...' contains attributed variables
;;
;; lib_memo.pl strips those attributes before numbervars; this test
;; exercises that path by combining memoize, trace!, recursion, and a
;; non-ground call arg in one function.

!(import! &self (library lib_memo))

!(memoize contains)

(= (contains $x $xs)
   (trace! (contains-lookup $x in $xs)
   (if (== $xs ())
       False
       (if (== $x (car-atom $xs))
           True
           (contains $x (cdr-atom $xs))))))

;; Ground call: works without touching the variant cache.
!(test (contains a (a b c)) True)

;; First non-ground call: $rest is fresh, so variant caching kicks in.
;; Without the fix this would crash the second time the recursive
;; call re-enters canonicalize_args_key/2 with an attributed variable.
!(test (contains a (a $rest)) True)

;; Second distinct variant: also returns True (after recursing into
;; the tail). Before the fix, this is the call that crashes.
!(test (contains b (a b $rest)) True)
