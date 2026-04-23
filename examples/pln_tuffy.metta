!(import! &self lib/lib_pln)

(= (STV (Concept Anna)) (stv 0.1667 0.9))
(= (STV (Concept Bob)) (stv 0.1667 0.9))
(= (STV (Concept Edward)) (stv 0.1667 0.9))
(= (STV (Concept Frank)) (stv 0.1667 0.9))
(= (STV (Concept Gary)) (stv 0.1667 0.9))
(= (STV (Concept Helen)) (stv 0.1667 0.9))

(= (kb)
   ((Sentence ((Implication (Inheritance (Product $1 $2) friend)
                            (Implication (Inheritance $1
                                                      (IntSet smokes))
                                         (Inheritance $2
                                                      (IntSet smokes))))
               (stv 0.4 0.9)) (1))
    (Sentence ((Implication (Inheritance $1
                                         (IntSet smokes))
                            (Inheritance $1
                                         (IntSet cancerous)))
               (stv 0.6 0.9)) (2))
    (Sentence ((Inheritance (Product Anna Bob)
                            friend) 
               (stv 1.0 0.9)) (3))
    (Sentence ((Inheritance (Product Anna Edward)
                            friend)
               (stv 1.0 0.9)) (4))
    (Sentence ((Inheritance (Product Anna Frank)
                            friend)
               (stv 1.0 0.9)) (5))
    (Sentence ((Inheritance (Product Edward Frank)
                            friend)
               (stv 1.0 0.9)) (6))
    (Sentence ((Inheritance (Product Gary Helen)
                            friend)
               (stv 1.0 0.9)) (7))
    (Sentence ((Inheritance (Product Gary Frank)
                            friend)
               (stv 0.0 0.9)) (8))
    (Sentence ((Inheritance Anna
                            (IntSet smokes))
               (stv 1.0 0.9)) (9))
    (Sentence ((Inheritance Edward
                            (IntSet smokes))
               (stv 1.0 0.9)) (10))))

!(test (PLN.Query (kb)
                  (Inheritance Edward
                               (IntSet cancerous)))
       ((stv 0.5199204545454544 0.8290782204129108) (1 2 4 9 10)))

