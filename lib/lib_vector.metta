(= (dot () ()) 0.0)
(= (dot (cons $a $as) (cons $b $bs))
   (+ (* $a $b) (dot $as $bs)))

(= (cosine-of-normalized $a $b)
   (dot $a $b))

(= (norm $v)
   (sqrt-math (dot $v $v)))

(= (cosine $a $b)
   (/ (dot $a $b)
      (* (norm $a) (norm $b))))

(= (random-normal-vector $N)
   (random-normal-vector $N ()))

(= (random-normal-vector $N $L)
   (if (> $N 0)
       (random-normal-vector (- $N 1) (cons (random-float 0.0 1.0) $L))
       (let $norm (norm $L)
            (collapse (let $x (superpose $L) (/ $x $norm))))))

