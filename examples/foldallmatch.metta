!(import! &self /home/carlo/code/prolog/PeTTa/lib/lib_import.metta)
!(use-module! aggregate)
!(import_prolog_function foldall)

(kb 1)
(kb 2)

!(test (foldall + (match &self (kb $n) (+ $n 1)) 0)
       5)

(= (f) 1)
(= (f) 2)

!(test (foldall + (let $x (f) (+ 1 $x)) 0)
        5)
