!(import! &self ../lib/lib_roman)

(= (inc $x) (+ $x 1))

!(test (first inc (1 9)) (2 9))
!(test (second inc (1 9)) (1 10))
!(test (flip (left right)) (right left))
