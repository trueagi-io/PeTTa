:- multifile memo_call/3.
:- multifile memo_enabled_fun/1.
:- multifile memo_invalidate/1.
:- multifile memo_disable/1.

% Default behavior when no memo library is imported.
memo_enabled_fun(_) :- fail.
memo_invalidate(_).
memo_disable(_).

memo_maybe_call(Fun, Args, Out, Goal) :-
    ( memo_enabled_fun(Fun)
    -> Goal = memo_call(Fun, Args, Out)
    ; append(Args, [Out], DirectArgs),
      Goal =.. [Fun|DirectArgs]
    ).
