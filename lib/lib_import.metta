!(import_prolog_function consult)

(= (import_prolog_functions_from_file $prologfile $importfunctions)
   (progn (translatePredicate (exists_file $prologfile))
          (translatePredicate (consult $prologfile))
          (let $function (superpose $importfunctions)
               (import_prolog_function $function))))

!(import_prolog_function use_module)

(= (import_prolog_functions_from_module $prologfile $importfunctions)
   (progn (translatePredicate (exists_file $prologfile))
          (translatePredicate (use_module $prologfile))
          (let $function (superpose $importfunctions)
               (import_prolog_function $function))))

!(import_prolog_functions_from_file (library lib_import.pl) (static-import! git-import! use-module!))
