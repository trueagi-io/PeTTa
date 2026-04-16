:- multifile metta_try_dispatch_call/4.
:- multifile metta_on_function_changed/1.
:- multifile metta_on_function_removed/1.

metta_try_dispatch_call(_, _, _, _) :- fail.
metta_on_function_changed(_).
metta_on_function_removed(_).
