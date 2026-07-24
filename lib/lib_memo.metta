!(import_prolog_function consult)

(= (import_prolog_functions_from_file $prologfile $importfunctions) (let* (($prologfile (exists_file))
                                                                           ($prologfile (consult))
                                                                           ($function (superpose $importfunctions)))
                                                                          (import_prolog_function $function)))

!(import_prolog_functions_from_file
    (library lib_memo.pl)
    (memoize config-memoize get-memoize-config clear-memoize invalidate-memoize is-memoized get-memoize-stats clear-memoize-stats))
