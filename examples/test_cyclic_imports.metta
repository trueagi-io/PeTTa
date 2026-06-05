!(import! &self file_a)

;; direct
!(test (add_10 10) 20)

;; cross-module
!(test (mul_2 5) 30)