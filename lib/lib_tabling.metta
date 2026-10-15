(= (injectPrologCode $code)
   (progn (translatePredicate (open_string $code $S))
          (translatePredicate (load_files user ((Predicate (stream $S)))))
          True))

(: tabled (-[nondet]-> Atom %Undefined%))
(= (tabled $call)
   (progn (translatePredicate (atomics_to_string (":- table '" (car-atom $call) "'/" (+ 1 (length (cdr-atom $call))) ".") $code))
          (injectPrologCode $code)))
