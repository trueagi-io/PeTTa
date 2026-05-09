!(import! &self (library lib_pln))

;PLN IMPLICATION TO CLAUSE COMPILATION EXTENSION:
(: => (-> Atom Atom Atom Atom))
(= (=> (cons , $args) $C $stvImp)
       (progn (cut) (add-atom &self (= $C (Truth_ModusPonens (foldl-atom $args (stv 1.0 1.0) Truth_ModusPonens) $stvImp)))))
(= (=> $A $C $stvImp)
   (add-atom &self (= $C (Truth_ModusPonens $A $stvImp))))

(: ? (-> Atom Atom))
(= (? $term)
   (unique-atom (collapse ($term (progn (reduce $term) ;ensures binding to specific answer, then combines evidence for all derived variants:
                                        (foldl-atom (collapse (reduce $term)) (stv 0.5 0.0) Truth_Revision))))))

;EXAMPLE:
!(=> (, (father $a $b) (father $b $c)) (grandfather $a $c) (stv 1.0 0.9))
!(=> (, (father $a $b) (mother $b $c)) (grandfather $a $c) (stv 1.0 0.9))
!(=> (grandfather $a $x) (old $a) (stv 1.0 0.9))

(= (father a b) (stv 1.0 0.9))
(= (father b c) (stv 1.0 0.9))

(= (father a y) (stv 1.0 0.9))
(= (mother y c) (stv 0.0 0.9))

!(test (? (grandfather a c))
       (quote (((grandfather a c) (stv 0.51 0.8432620011567381)))))
!(test (? (grandfather $who c))
       (quote (((grandfather a c) (stv 0.51 0.8432620011567381)))))
!(test (? (old $who))
       (quote (((old a) (stv 0.5198 0.7923434575206812)))))

