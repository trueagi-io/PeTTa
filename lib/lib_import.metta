!(import_prolog_function consult)

(= (import_prolog_functions_from_file $prologfile $importfunctions) (let* (($prologfile (exists_file))
                                                                           ($prologfile (consult))
                                                                           ($function (superpose $importfunctions)))
                                                                          (import_prolog_function $function)))

!(import_prolog_function use_module)

(= (import_prolog_functions_from_module $prologfile $importfunctions) (let* (($prologfile (exists_file))
                                                                             ($prologfile (use_module))
                                                                             ($function (superpose $importfunctions)))
                                                                            (import_prolog_function $function)))

!(import_prolog_functions_from_file (library lib_import.pl) (static-import! git-import! use-module!))
