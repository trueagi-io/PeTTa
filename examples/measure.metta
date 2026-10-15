;lib_measure: alternatives that carry a measure, and the operations over them.
;
;A weighted superposition is a tuple of (weight value) pairs. The same shape
;reads as a distribution, as attention weights, as a beam, or as a set of
;annotated disjunctions, depending only on which operation you apply.

!(import! &self ../lib/lib_measure)

;The mass, and scaling it to one:
!(test (ws-total ((0.25 a) (0.75 b))) 1.0)
!(test (ws-normalize ((1.0 a) (3.0 b))) ((0.25 a) (0.75 b)))
!(test (ws-normalize ()) ())

;An empty mass has no distribution, and says so rather than dividing:
!(test (ws-normalize ((0.0 a))) (Error ((0.0 a)) "ws-normalize requires nonzero total mass"))

;Softmax over equal scores is uniform, whatever the temperature:
!(test (ws-softmax ((1.0 a) (1.0 b)) 1.0) ((0.5 a) (0.5 b)))
!(test (ws-softmax ((1.0 a) (1.0 b)) 0.0)
       (Error ((1.0 a) (1.0 b)) "ws-softmax requires a nonzero temperature"))

;The heaviest value, which is softmax's zero-temperature limit:
!(test (ws-best ((0.2 a) (0.5 c) (0.3 b))) c)

;Best-first order, and beam search's keep rule over it:
!(test (ws-ranked ((0.2 a) (0.5 c) (0.3 b))) ((0.5 c) (0.3 b) (0.2 a)))
!(test (ws-top ((0.2 a) (0.5 c) (0.3 b)) 2) ((0.5 c) (0.3 b)))
!(test (ws-top ((0.2 a)) 5) ((0.2 a)))
!(test (ws-top ((0.2 a)) 0) ())

;Two explanations for one conclusion are one conclusion carrying both weights:
!(test (ws-collapse ((0.2 a) (0.3 b) (0.5 a))) ((0.7 a) (0.3 b)))

;The expected value of a numeric superposition:
!(test (ws-expect ((0.5 2.0) (0.5 4.0))) 3.0)

;Pairs above a weight, and the flip for sources that answer value-first:
!(test (ws-filter ((0.2 a) (0.6 b)) 0.5) ((0.6 b)))
!(test (ws-flip ((a 0.2) (b 0.6))) ((0.2 a) (0.6 b)))

;The nondeterministic reading: each pair is one alternative:
!(test (collapse (ws-choose ((0.2 a) (0.6 b)))) ((0.2 a) (0.6 b)))

;Sampling executes the choice. A one-pair superposition has one outcome, so
;the draw is checkable without pinning a seed:
!(test (ws-sample! ((1.0 only))) only)
