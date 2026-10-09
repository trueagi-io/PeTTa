!(import_prolog_function consult)

;; Loads a Prolog file and registers the named functions, one solution per
;; name (superpose): nondet, declared so - which is also what makes this
;; library loadable by a --strict program.
(: import_prolog_functions_from_file (-[nondet]-> %Undefined% (List %Undefined%) %Undefined%))

(= (import_prolog_functions_from_file $prologfile $importfunctions) (let* (($prologfile (exists_file))
                                                                           ($prologfile (consult))
                                                                           ($function (superpose $importfunctions)))
                                                                          (import_prolog_function $function)))

!(import_prolog_function use_module)

(: import_prolog_functions_from_module (-[nondet]-> %Undefined% (List %Undefined%) %Undefined%))

(= (import_prolog_functions_from_module $prologfile $importfunctions) (let* (($prologfile (exists_file))
                                                                             ($prologfile (use_module))
                                                                             ($function (superpose $importfunctions)))
                                                                            (import_prolog_function $function)))

!(import_prolog_functions_from_file (library lib_import.pl) (static-import! git-import! use-module!))
