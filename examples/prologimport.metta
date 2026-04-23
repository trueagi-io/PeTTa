;%%% FILE LOADING, REGEX MANIPULATION, PROCESS METTA STRING %%%

;Loading a Prolog builtin predicate that follows function form convention:
!(import_prolog_function re_replace)

;Using a Prolog builtin that has the last arg as an input argument (an instantiated options list):
(= (read_file_to_string_functionized $filename $options)
   (progn (callPredicate (Predicate (read_file_to_string $filename $text $options)))
          $text))

;which in most cases we can ignore:
(= (read_file_to_string_functionized $filename)
   (read_file_to_string_functionized $filename ()))

;Loading a PeTTa-internal predicate that follows function form convention:
!(import_prolog_function process_metta_string)

;Process the file string without the !(test (fib 30) 832040) call:
!(let $sourcecode (read_file_to_string_functionized "./examples/fib.metta")
      (process_metta_string (re_replace "\\!\\(test \\(fib 30\\) 832040\\)" "" $sourcecode)))

;Do our own call:
!(test (fib 10) 55)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;%%% HIJACK PROLOG CONSULT TO LOAD PREDICATES AS FUNCTIONS %%%

;Let's allow us to use Prolog's consult and import_prolog_function in combination to import functions from a pl file:
!(import! &self lib/lib_import)

;Let's use it to import myfunc, a predicate in function form convention defined as Prolog code in prologimport_example.pl:
!(import_prolog_functions_from_file "./examples/prologimport_example.pl" (myfunc))

;Test it too:
!(test (myfunc 41) 42)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;%%% DIRECTLY WORK WITH PREDICATES AND PREDICATE AS FUNCTION

;Let's go further, we want to define a Prolog clause as MeTTa S-expressions and assert it:
(: Predicate (-> Expression %Undefined%))

;Assert Prolog predicate (hello world)
!(assertaPredicate (Predicate (hello world)))

;Let's query our predicate:
!(test (let $temp (callPredicate (Predicate (hello $what))) $what) world)

;Let's suppose it is in function convention:
!(import_prolog_function hello)

;Meaning (hello) evaluates to world:
!(test (hello) world)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;%%% DIRECTLY WORK WITH CLAUSES AND CLAUSE AS FUNCTION

;Let's build a clause instead of just a predicate:
(= (myAddPred) (let* (($InputPlus1Predicate (Predicate (+ $Input 1)))
                      ($Body (, (Predicate (is $InputPlus1 $InputPlus1Predicate))
                                (Predicate (+ $InputPlus1 1 $Output))))
                      ($Clause (:- (Predicate (myAdd $Input $Output))
                                   (Predicate $Body))))
                     (Predicate $Clause)))

;Assert Prolog predicate (myAdd $Input $Output)
!(assertaPredicate (myAddPred))

;We can invoke it as a predicate:
!(test (progn (callPredicate (Predicate (myAdd 140 $x))) $x) 142)

;But let's suppose it is in function convention:
!(import_prolog_function myAdd)

;So we can call it more elegantly:
!(test (myAdd 140) 142)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;%%% METTA FUNCTION AS PREDICATE/CLAUSE

;Of course this also goes the other way, from MeTTa function:
(= (myAddMeTTa $x) (+ $x 1))

;allowing normal function call:
!(test (myAddMeTTa 241) 242)

;and invocation as predicate:
;!(test (let $temp (callPredicate (Predicate (myAddMeTTa 241 $x))) $x) 242)
