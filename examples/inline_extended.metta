; Extending an imported callee must reach the imported caller that copied its
; only clause.
!(import! &self inline_extended_lib)

(= (pick 1 $y) (+ $y 20))

!(test (use-pick 0) 11)
!(test (use-pick 1) 21)
