;Weighted superpositions: alternatives that carry a measure.
;
;A weighted superposition is a tuple of (weight value) pairs, the head shape
;of an annotated disjunction in probabilistic logic programming (Vennekens,
;Verbaeten, Bruynooghe, "Logic Programs with Annotated Disjunctions", ICLP
;2004; Sato's distribution semantics underneath). The operations here are the
;measure algebra those programs and modern attention share: normalize is a
;distribution, softmax with a temperature is attention's own weighting
;(Ramsauer et al., "Hopfield Networks is All You Need"), sampling executes the
;probabilistic choice, argmax is the zero-temperature limit, and top-k is beam
;search's keep rule. One representation, every reading.

;The mass of a weighted superposition. foldl-atom's list parameter is
;Expression, so it holds its operand as written; the mapped weights are NAMED
;and the name handed over.
(= (ws-total $ps)
   (let $weights (map-atom $ps (|-> ($p) (index-atom $p 0)))
     (foldl-atom $weights 0.0 +)))

;Scale every weight so the mass is one; a distribution:
(= (ws-normalize $ps)
   (if (== $ps ())
       ()
       (let $t (ws-total $ps)
            (if (<= (abs-math $t) 0.0)
                (Error $ps "ws-normalize requires nonzero total mass")
                (map-atom $ps
                  (|-> ($p) ((/ (index-atom $p 0) $t)
                              (index-atom $p 1))))))))

;Softmax with a temperature: scores become a distribution. Low temperature
;sharpens toward the best pair, high temperature flattens toward uniform.
(= (ws-softmax $ps $temp)
   (if (<= (abs-math $temp) 0.0)
       (Error $ps "ws-softmax requires a nonzero temperature")
       (ws-normalize
         (map-atom $ps (|-> ($p) ((exp (/ (index-atom $p 0) $temp))
                                  (index-atom $p 1)))))))

;The pair with more weight, then the best value; the temperature-zero limit:
(= (ws-pickmax $a $b) (if (>= (index-atom $a 0) (index-atom $b 0)) $a $b))
(= (ws-best $ps)
   (let $rest (cdr-atom $ps)
     (let $first (car-atom $ps)
       (let $best (foldl-atom $rest $first ws-pickmax)
         (index-atom $best 1)))))

;Pairs sorted best-first, and the first k of them, beam search's keep rule:
(= (ws-ranked $ps) (reverse (sort-atom $ps)))
(= (ws-take $ps $k)
   (if (or (== $ps ()) (<= $k 0))
       ()
       (let ($h $t) (decons-atom $ps)
            (let $rest (ws-take $t (- $k 1))
              (cons-atom $h $rest)))))
(= (ws-top $ps $k) (ws-take (ws-ranked $ps) $k))

;Draw one value with probability proportional to its weight: the annotated
;disjunction's choice, executed.
(= (ws-sample! $ps)
   (ws-sample-walk $ps (random-float 0.0 (ws-total $ps))))
(= (ws-sample-walk $ps $u)
   (let ($h $t) (decons-atom $ps)
        (if (or (== $t ()) (< $u (index-atom $h 0)))
            (index-atom $h 1)
            (ws-sample-walk $t (- $u (index-atom $h 0))))))

;Merge pairs carrying the same value, adding their weights: the summing of
;explanations for one conclusion.
(= (ws-merge-into $acc $p)
   (if (== $acc ())
       ($p)
       (let ($h $t) (decons-atom $acc)
            (if (== (index-atom $h 1) (index-atom $p 1))
                (let $summed (+ (index-atom $h 0) (index-atom $p 0))
                  (let $value (index-atom $h 1)
                    (cons-atom ($summed $value) $t)))
                (let $rest (ws-merge-into $t $p)
                  (cons-atom $h $rest))))))
(= (ws-collapse $ps) (foldl-atom $ps () ws-merge-into))

;The expected value of a numeric weighted superposition:
(= (ws-expect $ps)
   (let $terms (map-atom $ps (|-> ($p) (* (index-atom $p 0) (index-atom $p 1))))
     (foldl-atom $terms 0.0 +)))

;Nondeterministic reading: each pair is one alternative, measure as data:
(= (ws-choose $ps) (superpose $ps))

;Pairs above a weight, and (score value) from (value score), for sources
;such as knn retrieval that answer value-first:
(= (ws-filter $ps $least)
   (filter-atom $ps (|-> ($p) (>= (index-atom $p 0) $least))))
(= (ws-flip $ps)
   (map-atom $ps (|-> ($p) ((index-atom $p 1) (index-atom $p 0)))))
