:- ensure_loaded(tracing).

:- use_module(library(http/json)). % json_write_dict/3 for --debug-format=json

:- dynamic debug_category/1.
:- dynamic debug_goal_target/1.
:- dynamic debug_break_target/1.
:- dynamic debug_break_condition/3.
:- dynamic debug_break_space/1.
:- dynamic debug_break_match_fail/0.
:- dynamic debug_break_error/0.
:- dynamic debug_break_once/0.
:- dynamic debug_break_fired/0.
:- dynamic debug_break_continue/0.
:- dynamic debug_break_context/4.
:- dynamic debug_step_mode/1.
:- dynamic debug_step_armed/0.
:- dynamic debug_eval_suspended/0.
:- dynamic debug_format_json/0.
:- dynamic debug_indent/0.
:- dynamic debug_break_line/1.
:- dynamic dap_mode/0.
:- dynamic debug_break_skip/1.
:- dynamic debug_break_hits/1.
:- dynamic debug_max_depth/1.
:- dynamic debug_max_events/1.
:- dynamic debug_event_count/1.
:- dynamic debug_event_limit_notified/1.
:- dynamic debug_source_form/4.
:- dynamic debug_output_stream/1.
:- dynamic silent/1.
:- dynamic metta_call_stack/1.
metta_call_stack([]).
debug_event_count(0).

init_runtime_flags(Args) :-
    retractall(silent(_)),
    retractall(debug_category(_)),
    retractall(debug_goal_target(_)),
    retractall(debug_break_target(_)),
    retractall(debug_break_condition(_, _, _)),
    retractall(debug_break_space(_)),
    retractall(debug_break_match_fail),
    retractall(debug_break_error),
    retractall(debug_break_once),
    retractall(debug_break_fired),
    retractall(debug_break_continue),
    retractall(debug_break_context(_, _, _, _)),
    retractall(debug_step_mode(_)),
    retractall(debug_step_armed),
    retractall(debug_eval_suspended),
    retractall(debug_format_json),
    retractall(debug_indent),
    retractall(debug_break_line(_)),
    retractall(debug_break_skip(_)),
    retractall(debug_break_hits(_)),
    retractall(debug_max_depth(_)),
    retractall(debug_max_events(_)),
    retractall(debug_event_count(_)),
    retractall(debug_event_limit_notified(_)),
    retractall(debug_source_form(_, _, _, _)),
    close_debug_output_streams,
    assertz(debug_break_hits(0)),
    assertz(debug_event_count(0)),
    ( silent_requested(Args) -> assertz(silent(true)) ; assertz(silent(false)) ),
    debug_categories(Args, Categories),
    forall(member(Category, Categories), assertz(debug_category(Category))),
    debug_goal_targets(Args, Targets),
    forall(member(Target, Targets), assertz(debug_goal_target(Target))),
    debug_break_targets(Args, BreakTargets),
    forall(member(Target, BreakTargets), assertz(debug_break_target(Target))),
    debug_break_conditions(Args, BreakConditions),
    forall(member(Head-Condition-Spec, BreakConditions), assertz(debug_break_condition(Head, Condition, Spec))),
    debug_break_spaces(Args, BreakSpaces),
    forall(member(Space, BreakSpaces), assertz(debug_break_space(Space))),
    ( debug_break_match_fail_requested(Args) -> assertz(debug_break_match_fail) ; true ),
    ( debug_break_error_requested(Args) -> assertz(debug_break_error) ; true ),
    ( debug_format_json_requested(Args) -> assertz(debug_format_json) ; true ),
    ( debug_indent_requested(Args) -> assertz(debug_indent) ; true ),
    ( debug_break_once_requested(Args) -> assertz(debug_break_once) ; true ),
    ( debug_break_skip_arg(Args, BreakSkip) -> assertz(debug_break_skip(BreakSkip)) ; true ),
    ( debug_output_arg(Args, OutputPath) -> open(OutputPath, write, OutputStream), assertz(debug_output_stream(OutputStream)) ; true ),
    ( debug_max_depth_arg(Args, MaxDepth) -> assertz(debug_max_depth(MaxDepth)) ; true ),
    ( debug_max_events_arg(Args, MaxEvents) -> assertz(debug_max_events(MaxEvents)) ; true ),
    warn_unknown_debug_args(Args).

:- initialization((current_prolog_flag(argv, Args), init_runtime_flags(Args))).

silent_requested(Args) :-
    member(Arg, Args),
    memberchk(Arg, [silent, '--silent', '-s']).

debug_indent_requested(Args) :-
    memberchk('--debug-indent', Args).

% Beginner guardrail: warn (don't fail) on mistyped --debug categories and
% unrecognized --debug-* flags so a typo doesn't look like a broken debugger.
warn_unknown_debug_args(Args) :-
    forall(member(Arg, Args), warn_if_unknown_debug_arg(Args, Arg)).

warn_if_unknown_debug_arg(_Args, Arg) :-
    atom(Arg),
    atom_concat('--debug=', Spec, Arg),
    !,
    atomic_list_concat(Parts, ',', Spec),
    forall(member(Part, Parts), warn_if_unknown_category(Part)).
warn_if_unknown_debug_arg(_Args, Arg) :-
    atom(Arg),
    atom_concat('--debug', _, Arg),
    \+ runtime_option(Arg),
    !,
    format(user_error, "PeTTa: unknown debug option '~w'. Run: sh debug.sh --debug-help~n", [Arg]).
warn_if_unknown_debug_arg(_Args, _Arg).

warn_if_unknown_category(Part) :-
    normalize_debug_category(Part, _),
    !.
warn_if_unknown_category(Part) :-
    findall(Label, (debug_category_name(C), debug_category_label(C, Label)), Labels),
    atomic_list_concat(Labels, ', ', Valid),
    ( closest_category(Part, Suggestion)
      -> format(user_error, "PeTTa: unknown debug category '~w'. Did you mean '~w'? Valid: ~w~n",
                [Part, Suggestion, Valid])
      ;  format(user_error, "PeTTa: unknown debug category '~w'. Valid: ~w~n", [Part, Valid])
    ).

% A cheap "did you mean" -- prefer a valid category that the typo is a prefix of
% (runtim -> runtime), then any that shares the typo as a substring.
closest_category(Part, Suggestion) :-
    debug_category_name(C),
    debug_category_label(C, Suggestion),
    atom_concat(Part, _, Suggestion),
    Part \== Suggestion,
    !.
closest_category(Part, Suggestion) :-
    sub_atom(Part, 0, _, _, ''),
    atom_length(Part, L), L >= 3,
    debug_category_name(C),
    debug_category_label(C, Suggestion),
    sub_atom(Suggestion, _, _, _, Part),
    !.

% True when the args carry a --debug* option (used to distinguish "ran the
% debugger but forgot the file" from "ran the bare interpreter demo").
debug_args_present(Args) :-
    member(Arg, Args),
    atom(Arg),
    atom_concat('--debug', _, Arg),
    !.

debug_categories(Args, Categories) :-
    findall(Category, debug_category_arg(Args, Category), RawCategories),
    sort(RawCategories, Categories).

debug_category_arg(Args, all) :-
    member('--debug', Args).
debug_category_arg(Args, all) :-
    member('--debug-all', Args).
debug_category_arg(Args, Category) :-
    member(Arg, Args),
    atom(Arg),
    atom_concat('--debug=', Spec, Arg),
    debug_category_spec(Spec, Category).

debug_category_spec(Spec, Category) :-
    atomic_list_concat(Parts, ',', Spec),
    member(Part, Parts),
    normalize_debug_category(Part, Category).

debug_goal_targets(Args, Targets) :-
    findall(Target, debug_goal_target_arg(Args, Target), RawTargets),
    sort(RawTargets, Targets).

debug_goal_target_arg(Args, Target) :-
    member(Arg, Args),
    atom(Arg),
    atom_concat('--debug-goal=', Spec, Arg),
    atomic_list_concat(Parts, ',', Spec),
    member(Part, Parts),
    Target = Part.

debug_break_targets(Args, Targets) :-
    findall(Target, debug_break_target_arg(Args, Target), RawTargets),
    sort(RawTargets, Targets).

debug_break_target_arg(Args, Target) :-
    member(Arg, Args),
    atom(Arg),
    atom_concat('--debug-break=', Spec, Arg),
    atomic_list_concat(Parts, ',', Spec),
    member(Part, Parts),
    Target = Part.

debug_break_conditions(Args, Conditions) :-
    findall(Head-Condition-Spec, debug_break_condition_arg(Args, Head, Condition, Spec), RawConditions),
    sort(RawConditions, Conditions).

debug_break_condition_arg(Args, Head, Condition, Spec) :-
    member(Arg, Args),
    atom(Arg),
    atom_concat('--debug-break-if=', Spec, Arg),
    atomic_list_concat(Parts, ',', Spec),
    member(Part, Parts),
    parse_break_condition_spec(Part, Head, Condition),
    Spec = Part.

debug_break_spaces(Args, Spaces) :-
    findall(Space, debug_break_space_arg(Args, Space), RawSpaces),
    sort(RawSpaces, Spaces).

debug_break_space_arg(Args, Space) :-
    member(Arg, Args),
    atom(Arg),
    atom_concat('--debug-break-space=', Spec, Arg),
    atomic_list_concat(Parts, ',', Spec),
    member(Part, Parts),
    Space = Part.

debug_format_json_requested(Args) :-
    ( member('--debug-jsonl', Args)
    ; member(Arg, Args),
      atom(Arg),
      atom_concat('--debug-format=', Value, Arg),
      memberchk(Value, [json, jsonl])
    ),
    !.

debug_break_match_fail_requested(Args) :-
    member('--debug-break-match-fail', Args).

debug_break_error_requested(Args) :-
    member('--debug-break-error', Args).

parse_break_condition_spec(Spec, Head, Condition) :-
    sub_atom(Spec, ColonPos, 1, _, ':'),
    sub_atom(Spec, 0, ColonPos, _, Head),
    ExprStart is ColonPos + 1,
    sub_atom(Spec, ExprStart, _, 0, Expr),
    parse_break_condition_expr(Expr, Condition).

parse_break_condition_expr(Expr, Condition) :-
    parse_break_or_expr(Expr, Condition).

parse_break_or_expr(Expr, or(Left, Right)) :-
    sub_atom(Expr, OrPos, 1, _, '|'),
    sub_atom(Expr, 0, OrPos, _, LeftExpr),
    RightStart is OrPos + 1,
    sub_atom(Expr, RightStart, _, 0, RightExpr),
    parse_break_or_expr(LeftExpr, Left),
    parse_break_and_expr(RightExpr, Right),
    !.
parse_break_or_expr(Expr, Condition) :-
    parse_break_and_expr(Expr, Condition).

parse_break_and_expr(Expr, and(Left, Right)) :-
    sub_atom(Expr, AndPos, 1, _, '&'),
    sub_atom(Expr, 0, AndPos, _, LeftExpr),
    RightStart is AndPos + 1,
    sub_atom(Expr, RightStart, _, 0, RightExpr),
    parse_break_and_expr(LeftExpr, Left),
    parse_break_base_expr(RightExpr, Right),
    !.
parse_break_and_expr(Expr, Condition) :-
    parse_break_base_expr(Expr, Condition).

parse_break_base_expr(Expr, condition(ArgIndex, Op, Value)) :-
    break_operator(Op, Token),
    sub_atom(Expr, OpPos, TokenLen, _, Token),
    TokenLen > 0,
    sub_atom(Expr, 0, OpPos, _, ArgSpec),
    RightStart is OpPos + TokenLen,
    sub_atom(Expr, RightStart, _, 0, ValueSpec),
    parse_break_arg_spec(ArgSpec, ArgIndex),
    parse_break_value(ValueSpec, Value),
    !.
parse_break_base_expr(Expr, result_condition(Op, Value)) :-
    break_operator(Op, Token),
    sub_atom(Expr, OpPos, TokenLen, _, Token),
    TokenLen > 0,
    sub_atom(Expr, 0, OpPos, _, result),
    RightStart is OpPos + TokenLen,
    sub_atom(Expr, RightStart, _, 0, ValueSpec),
    parse_break_value(ValueSpec, Value),
    !.

parse_break_arg_spec(ArgSpec, ArgIndex) :-
    atom_concat(arg, IndexAtom, ArgSpec),
    atom_number(IndexAtom, ArgIndex),
    ArgIndex >= 1.

parse_break_value(ValueSpec, Value) :-
    atom_number(ValueSpec, Number),
    !,
    Value = Number.
parse_break_value(true, true) :-
    !.
parse_break_value(false, false) :-
    !.
parse_break_value(ValueSpec, Value) :-
    % A parenthesised spec is a MeTTa s-expression: parse it so structural
    % (=, !=) and substring (~) comparisons work against the actual term, e.g.
    % match:arg1=(set 1 a) compares against the list [set,1,a].
    sub_atom(ValueSpec, 0, 1, _, '('),
    catch(sread(ValueSpec, Parsed), _, fail),
    !,
    Value = Parsed.
parse_break_value(ValueSpec, ValueSpec).

break_operator(>=, '>=').
break_operator(=<, '<=').
break_operator(\=, '!=').
break_operator(>, '>').
break_operator(<, '<').
break_operator(~, '~').
break_operator(=, '=').

debug_max_depth_arg(Args, MaxDepth) :-
    member(Arg, Args),
    atom(Arg),
    atom_concat('--debug-depth=', Spec, Arg),
    atom_number(Spec, MaxDepth),
    MaxDepth >= 0.

debug_max_events_arg(Args, MaxEvents) :-
    member(Arg, Args),
    atom(Arg),
    atom_concat('--debug-max-events=', Spec, Arg),
    atom_number(Spec, MaxEvents),
    MaxEvents >= 0.

debug_break_once_requested(Args) :-
    member(Arg, Args),
    memberchk(Arg, ['--debug-break-once']).

debug_break_skip_arg(Args, BreakSkip) :-
    member(Arg, Args),
    atom(Arg),
    atom_concat('--debug-break-skip=', Spec, Arg),
    atom_number(Spec, BreakSkip),
    BreakSkip >= 0.

debug_output_arg(Args, OutputPath) :-
    member(Arg, Args),
    atom(Arg),
    atom_concat('--debug-output=', OutputPath, Arg).

normalize_debug_category('source', source).
normalize_debug_category('parse', parse).
normalize_debug_category('compile', compile).
normalize_debug_category('translate', translate).
normalize_debug_category('runtime', runtime).
normalize_debug_category('runtime-leaf', runtime_leaf).
normalize_debug_category('runtime-fail', runtime_fail).
normalize_debug_category('runtime-prolog', runtime_prolog).
normalize_debug_category('space', space).
normalize_debug_category('space-mutation', space_mutation).
normalize_debug_category('space-match-fail', space_match_fail).
normalize_debug_category('space-get-atoms', space_get_atoms).
normalize_debug_category('result', result).
normalize_debug_category('all', all).

debug_category_name(source).
debug_category_name(parse).
debug_category_name(compile).
debug_category_name(translate).
debug_category_name(runtime).
debug_category_name(runtime_leaf).
debug_category_name(runtime_fail).
debug_category_name(runtime_prolog).
debug_category_name(space).
debug_category_name(space_mutation).
debug_category_name(space_match_fail).
debug_category_name(space_get_atoms).
debug_category_name(result).
debug_category_name(all).

debug_category_label(source, 'source').
debug_category_label(parse, 'parse').
debug_category_label(compile, 'compile').
debug_category_label(translate, 'translate').
debug_category_label(runtime, 'runtime').
debug_category_label(runtime_leaf, 'runtime-leaf').
debug_category_label(runtime_fail, 'runtime-fail').
debug_category_label(runtime_prolog, 'runtime-prolog').
debug_category_label(space, 'space').
debug_category_label(space_mutation, 'space-mutation').
debug_category_label(space_match_fail, 'space-match-fail').
debug_category_label(space_get_atoms, 'space-get-atoms').
debug_category_label(result, 'result').
debug_category_label(all, 'all').

debug_enabled(Category) :-
    debug_category(all), !;
    runtime_category_alias(Category), !;
    space_category_alias(Category), !;
    debug_category(Category).

runtime_category_alias(runtime) :-
    debug_category(runtime_leaf),
    !.
runtime_category_alias(runtime) :-
    debug_category(runtime_fail),
    !.
runtime_category_alias(runtime) :-
    debug_category(runtime_prolog),
    !.
runtime_category_alias(runtime) :-
    debug_break_target(_),
    !.
runtime_category_alias(runtime) :-
    debug_break_condition(_, _, _),
    !.
runtime_category_alias(runtime) :-
    debug_break_line(_),
    !.
runtime_category_alias(runtime) :-
    debug_break_once,
    !.
runtime_category_alias(runtime) :-
    debug_step_mode(_),
    !.
runtime_category_alias(runtime) :-
    debug_break_error.

space_category_alias(space) :-
    debug_break_space(_),
    !.
space_category_alias(space) :-
    debug_break_match_fail,
    !.
space_category_alias(space) :-
    debug_category(space_mutation),
    !.
space_category_alias(space) :-
    debug_category(space_match_fail),
    !.
space_category_alias(space) :-
    debug_category(space_get_atoms).

legacy_verbose_enabled(Category) :-
    silent(false),
    \+ debug_enabled(Category).

runtime_event_enabled(_Stage, _Goal) :-
    ( debug_category(all)
    ; debug_enabled(runtime)
    ),
    !.
runtime_event_enabled(_Stage, Goal) :-
    debug_category(runtime_leaf),
    leaf_goal(Goal),
    !.
runtime_event_enabled(fail, _Goal) :-
    debug_category(runtime_fail),
    !.
runtime_event_enabled(_, _) :-
    fail.

runtime_goal_enabled(_) :-
    \+ debug_goal_target(_),
    !.
runtime_goal_enabled(Goal) :-
    goal_head_name(Goal, Head),
    debug_goal_target(Head).

breakpoint_goal_enabled(Goal) :-
    goal_head_name(Goal, Head),
    debug_break_target(Head).

breakpoint_condition_enabled(Stage, Goal) :-
    goal_head_name(Goal, Head),
    debug_break_condition(Head, Condition, _),
    breakpoint_condition_matches(Stage, Goal, Condition).

breakpoint_condition_matches(Stage, Goal, and(Left, Right)) :-
    breakpoint_condition_matches(Stage, Goal, Left),
    breakpoint_condition_matches(Stage, Goal, Right).
breakpoint_condition_matches(Stage, Goal, or(Left, Right)) :-
    ( breakpoint_condition_matches(Stage, Goal, Left)
    ; breakpoint_condition_matches(Stage, Goal, Right)
    ).
breakpoint_condition_matches(_Stage, Goal, condition(ArgIndex, Op, Expected)) :-
    strip_trace_wrappers(Goal, CleanGoal),
    compound(CleanGoal),
    CleanGoal =.. [_|AllArgs],
    append(InputArgs, [_], AllArgs),
    nth1(ArgIndex, InputArgs, Actual),
    compare_break_value(Op, Actual, Expected).
breakpoint_condition_matches(success, Goal, result_condition(Op, Expected)) :-
    strip_trace_wrappers(Goal, CleanGoal),
    compound(CleanGoal),
    CleanGoal =.. [_|AllArgs],
    append(_, [Actual], AllArgs),
    compare_break_value(Op, Actual, Expected).
breakpoint_condition_matches(Stage, _Goal, result_condition(_, _)) :-
    Stage \== success,
    fail.

compare_break_value(=, Actual, Expected) :-
    Actual == Expected.
compare_break_value(\=, Actual, Expected) :-
    Actual \== Expected.
compare_break_value(>, Actual, Expected) :-
    number(Actual),
    number(Expected),
    Actual > Expected.
compare_break_value(<, Actual, Expected) :-
    number(Actual),
    number(Expected),
    Actual < Expected.
compare_break_value(>=, Actual, Expected) :-
    number(Actual),
    number(Expected),
    Actual >= Expected.
compare_break_value(=<, Actual, Expected) :-
    number(Actual),
    number(Expected),
    Actual =< Expected.
compare_break_value(~, Actual, Expected) :-
    % Substring / containment: render both to their MeTTa text and test whether
    % the expected text occurs inside the actual one. Works for atoms, strings,
    % numbers and compound MeTTa expressions.
    break_value_text(Actual, ActualText),
    break_value_text(Expected, ExpectedText),
    sub_atom(ActualText, _, _, _, ExpectedText).

break_value_text(Value, Text) :-
    ( atom(Value) -> Text = Value
    ; number(Value) -> atom_number(Text, Value)
    ; string(Value) -> atom_string(Text, Value)
    ; swrite(Value, S), atom_string(Text, S)
    ).

space_event_enabled(add, _) :-
    ( debug_category(all)
    ; debug_category(space)
    ; debug_category(space_mutation)
    ; debug_break_space(_)
    ),
    !.
space_event_enabled(remove, _) :-
    ( debug_category(all)
    ; debug_category(space)
    ; debug_category(space_mutation)
    ; debug_break_space(_)
    ),
    !.
space_event_enabled(match, fail) :-
    ( debug_category(all)
    ; debug_category(space)
    ; debug_category(space_match_fail)
    ; debug_break_match_fail
    ),
    !.
space_event_enabled(match, success) :-
    ( debug_category(all)
    ; debug_category(space)
    ),
    !.
space_event_enabled(get_atoms, _) :-
    ( debug_category(all)
    ; debug_category(space)
    ; debug_category(space_get_atoms)
    ),
    !.
space_event_enabled(_, _) :-
    fail.

%%% MeTTa-level call stack %%%

% Only track user-defined MeTTa functions, not builtins
metta_user_goal(Goal) :-
    strip_trace_wrappers(Goal, Clean),
    nonvar(Clean),
    compound(Clean),
    functor(Clean, Name, Arity),
    ResultArity is Arity - 1,
    ResultArity >= 1,
    fun(Name),
    \+ predicate_property(Clean, built_in),
    \+ predicate_property(Clean, imported_from(_)).

% Build a MeTTa-style frame string: (funcName arg1 arg2 ...)
metta_frame(Goal, Frame) :-
    strip_trace_wrappers(Goal, Clean),
    compound(Clean),
    Clean =.. [Name|AllArgs],
    ( AllArgs = [] -> Args = []
    ; append(Args, [_], AllArgs)   % drop last arg (the output)
    ),
    ( Args == []
      -> format(atom(Frame), "(~w)", [Name])
      ; maplist(swrite, Args, ArgStrs),
        atomic_list_concat(ArgStrs, ' ', ArgsAtom),
        format(atom(Frame), "(~w ~w)", [Name, ArgsAtom])
    ).

call_stack_push(Goal) :-
    ( metta_user_goal(Goal), metta_frame(Goal, Frame)
      -> retract(metta_call_stack(Stack)),
         assertz(metta_call_stack([Frame|Stack]))
      ; true
    ).

call_stack_pop(Goal) :-
    ( metta_user_goal(Goal)
      -> retract(metta_call_stack([_|Rest])),
         assertz(metta_call_stack(Rest))
      ; true
    ).

print_call_stack :-
    metta_call_stack(Stack),
    Stack \= [],
    !,
    reverse(Stack, Ordered),
    atomic_list_concat(Ordered, ' → ', Chain),
    color_format(user_error, blue, "stack: ~w~n", [Chain]).
print_call_stack.

debug_event(Category, Meta, Payload) :-
    \+ debug_eval_suspended,
    debug_enabled(Category),
    debug_event_allowed,
    !,
    ( debug_format_json
      -> emit_json_event(Category, Meta, Payload)
      ;  dap_mode
      -> maybe_handle_breakpoint(Category, Meta, Payload),
         maybe_handle_step(Category, Meta, Payload)
      ;  maybe_handle_breakpoint(Category, Meta, Payload),
         maybe_handle_step(Category, Meta, Payload),
         render_debug_event(Category, Meta, Payload)
    ).
debug_event(_, _, _).

debug_event_allowed :-
    debug_max_events(MaxEvents),
    !,
    debug_event_count(Count),
    ( Count < MaxEvents
      -> NextCount is Count + 1,
         retractall(debug_event_count(_)),
         assertz(debug_event_count(NextCount))
      ; notify_debug_event_limit_reached,
        fail
    ).
debug_event_allowed :-
    retractall(debug_event_count(_)),
    assertz(debug_event_count(1)).

notify_debug_event_limit_reached :-
    debug_event_limit_notified(true),
    !.
notify_debug_event_limit_reached :-
    assertz(debug_event_limit_notified(true)),
    color_format(user_error, magenta, "[DEBUG limit] event limit reached; suppressing further debug output~n", []).

maybe_handle_breakpoint(runtime, Meta, goal(Stage, GoalIndex, Goal)) :-
    ( breakpoint_event_visible(Stage, Goal)
    ; line_breakpoint_visible(Stage, Meta)
    ),
    !,
    debug_breakpoint(Stage, Meta, GoalIndex, Goal).
maybe_handle_breakpoint(space, Meta, Payload) :-
    space_breakpoint_visible(Payload, Reason),
    !,
    debug_space_breakpoint(Meta, Payload, Reason).
maybe_handle_breakpoint(_, _, _).

% A space event triggers a breakpoint when --debug-break-space matches the
% mutated space, or --debug-break-match-fail is set and a match failed. Shares
% the hit-count / skip / once bookkeeping with runtime breakpoints.
space_breakpoint_visible(Payload, Reason) :-
    \+ breakpoint_disabled_after_first_hit,
    space_breakpoint_reason(Payload, Reason),
    note_breakpoint_hit(HitCount),
    \+ breakpoint_skipped(HitCount).

space_breakpoint_reason(space(add, Space, _Term), Reason) :-
    space_break_armed(Space),
    format(atom(Reason), "space mutation: add to ~w", [Space]).
space_breakpoint_reason(space(remove, Space, _Term, _Removed), Reason) :-
    space_break_armed(Space),
    format(atom(Reason), "space mutation: remove from ~w", [Space]).
space_breakpoint_reason(space(match, fail, Space, _Pattern, _Result), Reason) :-
    debug_break_match_fail,
    format(atom(Reason), "match failed in ~w", [Space]).

space_break_armed(Space) :-
    debug_break_space(Space),
    !.
space_break_armed(_) :-
    debug_break_space('*').

debug_space_breakpoint(meta(_Index, _Line, _Kind), Payload, Reason) :-
    note_breakpoint_fired,
    retractall(debug_break_context(_, _, _, _)),
    space_payload_text(Payload, OpText),
    color_format(user_error, magenta, "[BREAKPOINT space] ~w~n", [OpText]),
    print_breakpoint_hit_count,
    color_format(user_error, magenta, "reason: ~w~n", [Reason]),
    print_call_stack,
    maybe_pause_on_breakpoint.

space_payload_text(space(add, Space, Term), Text) :-
    swrite(Term, TermText),
    format(atom(Text), "add ~w to ~w", [TermText, Space]).
space_payload_text(space(remove, Space, Term, _Removed), Text) :-
    swrite(Term, TermText),
    format(atom(Text), "remove ~w from ~w", [TermText, Space]).
space_payload_text(space(match, Stage, Space, Pattern, _Result), Text) :-
    swrite(Pattern, PatternText),
    format(atom(Text), "match ~w in ~w (~w)", [PatternText, Space, Stage]).

maybe_handle_step(runtime, Meta, goal(Stage, GoalIndex, Goal)) :-
    step_event_visible(Stage, Goal),
    !,
    debug_step_pause(Stage, Meta, GoalIndex, Goal).
maybe_handle_step(_, _, _).

breakpoint_event_visible(Stage, Goal) :-
    \+ breakpoint_disabled_after_first_hit,
    breakpoint_triggered(Stage, Goal),
    note_breakpoint_hit(HitCount),
    \+ breakpoint_skipped(HitCount),
    runtime_depth_visible.

% Break when entering a compiled goal whose source form is on a requested line
% (set via the DAP setBreakpoints request -> debug_break_line/1).
line_breakpoint_visible(enter, meta(_Index, Line, _Kind)) :-
    debug_break_line(Line),
    \+ breakpoint_disabled_after_first_hit,
    note_breakpoint_hit(HitCount),
    \+ breakpoint_skipped(HitCount).

breakpoint_triggered(enter, Goal) :-
    breakpoint_goal_enabled(Goal),
    !.
breakpoint_triggered(success, Goal) :-
    debug_break_error,
    goal_result_is_error(Goal),
    !.
breakpoint_triggered(Stage, Goal) :-
    breakpoint_condition_enabled(Stage, Goal).

% True when a goal succeeded with PeTTa's normalised error representation as its
% result, i.e. an ['Error'|_] term produced by the translator's catch wrapper.
goal_result_is_error(Goal) :-
    breakpoint_frame_result(Goal, Result),
    nonvar(Result),
    Result = ['Error'|_].

breakpoint_disabled_after_first_hit :-
    debug_break_continue,
    !.
breakpoint_disabled_after_first_hit :-
    debug_break_once,
    debug_break_fired.

note_breakpoint_hit(HitCount) :-
    debug_break_hits(Count),
    HitCount is Count + 1,
    retractall(debug_break_hits(_)),
    assertz(debug_break_hits(HitCount)).

breakpoint_skipped(HitCount) :-
    debug_break_skip(BreakSkip),
    HitCount =< BreakSkip.

step_event_visible(_Stage, _Goal) :-
    debug_step_armed,
    !,
    retractall(debug_step_armed),
    fail.
step_event_visible(Stage, Goal) :-
    debug_step_mode(Mode),
    runtime_event_visible(Stage, Goal),
    step_triggered(Mode, Stage),
    current_call_stack_depth(Depth),
    step_depth_matches(Mode, Depth).

step_triggered(into, _).
step_triggered(over(_), _).
step_triggered(out(_), Stage) :-
    Stage \== enter.

step_depth_matches(into, _).
step_depth_matches(over(TargetDepth), Depth) :-
    Depth =< TargetDepth.
step_depth_matches(out(TargetDepth), Depth) :-
    Depth =< TargetDepth.

render_debug_event(source, meta(Index, Line, Kind), FormStr) :-
    debug_header(source, Index, Line, Kind),
    source_text(Kind, FormStr, Display),
    debug_format("~w~n~n", [Display]).
render_debug_event(parse, meta(Index, Line, Kind), parsed(ParsedType, Term)) :-
    debug_header(parse, Index, Line, Kind),
    debug_format("kind: ~w~n", [ParsedType]),
    swrite(Term, PrettyTerm),
    debug_format("~w~n~n", [PrettyTerm]).
render_debug_event(compile, meta(Index, Line, Kind), clause(Clause)) :-
    debug_header(compile, Index, Line, Kind),
    clause_text(Clause, Text),
    debug_format("~s~n", [Text]),
    debug_nl.
render_debug_event(translate, meta(Index, Line, Kind), goals(Goals)) :-
    debug_header(translate, Index, Line, Kind),
    print_goals(Goals),
    debug_nl.
render_debug_event(runtime, _Meta, goal(Stage, _GoalIndex, Goal)) :-
    debug_indent,
    runtime_event_visible(Stage, Goal),
    !,
    print_indented_runtime_line(Stage, Goal).
render_debug_event(runtime, meta(Index, Line, Kind), goal(Stage, GoalIndex, Goal)) :-
    runtime_event_visible(Stage, Goal),
    !,
    debug_header(runtime, Index, Line, Kind),
    goal_path_text(GoalIndex, GoalPathText),
    print_runtime_goal_line(Stage, GoalIndex, GoalPathText, Goal),
    print_call_stack,
    maybe_print_runtime_prolog(GoalIndex, Goal),
    debug_nl.
render_debug_event(runtime, _, _) :-
    !.
render_debug_event(space, meta(Index, Line, Kind), space(add, SpaceName, Term)) :-
    debug_header(space, Index, Line, Kind),
    swrite(Term, PrettyTerm),
    debug_format("op: add space: ~w~n", [SpaceName]),
    debug_format("~w~n~n", [PrettyTerm]).
render_debug_event(space, meta(Index, Line, Kind), space(remove, SpaceName, Term, Removed)) :-
    debug_header(space, Index, Line, Kind),
    swrite(Term, PrettyTerm),
    debug_format("op: remove space: ~w removed: ~w~n", [SpaceName, Removed]),
    debug_format("~w~n~n", [PrettyTerm]).
render_debug_event(space, meta(Index, Line, Kind), space(match, Stage, SpaceName, Pattern, Result)) :-
    debug_header(space, Index, Line, Kind),
    swrite(Pattern, PrettyPattern),
    debug_format("op: match stage: ~w space: ~w~n", [Stage, SpaceName]),
    debug_format("pattern: ~w~n", [PrettyPattern]),
    ( nonvar(Result)
      -> swrite(Result, PrettyResult),
         debug_format("result: ~w~n~n", [PrettyResult])
      ; debug_nl
    ).
render_debug_event(space, meta(Index, Line, Kind), space(get_atoms, SpaceName, Pattern)) :-
    debug_header(space, Index, Line, Kind),
    swrite(Pattern, PrettyPattern),
    debug_format("op: get-atoms space: ~w~n", [SpaceName]),
    debug_format("pattern: ~w~n~n", [PrettyPattern]).
render_debug_event(result, meta(Index, Line, Kind), Results) :-
    debug_header(result, Index, Line, Kind),
    ( Results == []
      -> debug_format("<no results>~n~n", [])
      ; forall(member(Result, Results), print_result_line(Result)),
        debug_nl
    ).

% --- JSON / JSONL machine-readable event stream (--debug-format=json) ---
% One JSON object per line, written to the debug streams. This is the raw event
% feed the DAP server (M7) maps into stopped/output/terminated events.

emit_json_event(runtime, meta(Index, Line, Kind), goal(Stage, _GoalIndex, Goal)) :-
    runtime_event_visible(Stage, Goal),
    !,
    strip_trace_wrappers(Goal, CleanGoal),
    runtime_goal_core_text(CleanGoal, GoalText),
    json_meta_index(Index, IndexJson),
    current_call_stack_depth(Depth),
    kind_vars_dict(Kind, VarsDict),
    ( Stage == success, runtime_goal_result(CleanGoal, ResultText)
      -> ResultPairs = [result-ResultText]
      ;  ResultPairs = []
    ),
    dict_pairs(Dict, json,
        [ event-runtime, stage-Stage, goal-GoalText,
          index-IndexJson, line-Line, depth-Depth, vars-VarsDict | ResultPairs ]),
    write_json_line(Dict).
emit_json_event(runtime, _, _) :-
    !.
emit_json_event(space, _Meta, space(add, Space, Term)) :-
    !,
    swrite(Term, TermText),
    dict_pairs(Dict, json, [event-space, op-add, space-Space, term-TermText]),
    write_json_line(Dict).
emit_json_event(space, _Meta, space(remove, Space, Term, Removed)) :-
    !,
    swrite(Term, TermText),
    json_text(Removed, RemovedText),
    dict_pairs(Dict, json, [event-space, op-remove, space-Space, term-TermText, removed-RemovedText]),
    write_json_line(Dict).
emit_json_event(space, _Meta, space(match, Stage, Space, Pattern, Result)) :-
    !,
    swrite(Pattern, PatternText),
    json_text(Result, ResultText),
    dict_pairs(Dict, json, [event-space, op-match, stage-Stage, space-Space, pattern-PatternText, result-ResultText]),
    write_json_line(Dict).
emit_json_event(space, _Meta, space(get_atoms, Space, Pattern)) :-
    !,
    swrite(Pattern, PatternText),
    dict_pairs(Dict, json, [event-space, op-get_atoms, space-Space, pattern-PatternText]),
    write_json_line(Dict).
emit_json_event(result, _Meta, Results) :-
    is_list(Results),
    !,
    maplist(swrite, Results, ResultTexts),
    dict_pairs(Dict, json, [event-result, results-ResultTexts]),
    write_json_line(Dict).
emit_json_event(_, _, _).

write_json_line(Dict) :-
    forall(debug_stream(Stream),
           ( json_write_dict(Stream, Dict, [width(0)]),
             nl(Stream) )).

% Form index is an integer for source forms but an atom (compiled/eval) for
% synthesized metas; keep integers as numbers and render atoms as strings.
json_meta_index(Index, Index) :-
    integer(Index),
    !.
json_meta_index(Index, Text) :-
    json_text(Index, Text).

% Render an arbitrary value to a JSON-friendly scalar: numbers/atoms/strings pass
% through (json_write_dict encodes them), unbound is null, compound becomes its
% MeTTa text.
json_text(Value, null) :-
    var(Value),
    !.
json_text(Value, Value) :-
    ( number(Value) ; string(Value) ),
    !.
json_text(Value, Text) :-
    atom(Value),
    !,
    Text = Value.
json_text(Value, Text) :-
    swrite(Value, S),
    atom_string(Text, S).

kind_vars_dict(compiled(_SourceExpr, Bindings), VarsDict) :-
    is_list(Bindings),
    !,
    bindings_to_pairs(Bindings, Pairs),
    dict_pairs(VarsDict, json, Pairs).
kind_vars_dict(_, VarsDict) :-
    dict_pairs(VarsDict, json, []).

bindings_to_pairs([], []).
bindings_to_pairs([Name-Value|Rest], [Name-Text|PairsRest]) :-
    json_text(Value, Text),
    bindings_to_pairs(Rest, PairsRest).

debug_breakpoint(Stage, meta(Index, Line, Kind), GoalIndex, Goal) :-
    note_breakpoint_fired,
    set_breakpoint_context(Stage, meta(Index, Line, Kind), GoalIndex, Goal),
    runtime_goal_text(Stage, Goal, GoalText),
    goal_path_text(GoalIndex, GoalPathText),
    meta_kind_label(Kind, KindLabel),
    color_format(user_error, magenta, "[BREAKPOINT #~w line ~w ~w] ~w @ ~w~n", [Index, Line, KindLabel, GoalText, GoalPathText]),
    print_breakpoint_hit_count,
    print_breakpoint_reason(Stage, Goal),
    print_compiled_source_expr(Kind),
    print_compiled_vars(Kind),
    print_call_stack,
    maybe_print_runtime_prolog(GoalIndex, Goal),
    maybe_pause_on_breakpoint.

debug_step_pause(Stage, meta(Index, Line, Kind), GoalIndex, Goal) :-
    retractall(debug_step_mode(_)),
    set_breakpoint_context(Stage, meta(Index, Line, Kind), GoalIndex, Goal),
    runtime_goal_text(Stage, Goal, GoalText),
    goal_path_text(GoalIndex, GoalPathText),
    meta_kind_label(Kind, KindLabel),
    color_format(user_error, magenta, "[STEP #~w line ~w ~w] ~w @ ~w~n", [Index, Line, KindLabel, GoalText, GoalPathText]),
    print_compiled_source_expr(Kind),
    print_compiled_vars(Kind),
    print_call_stack,
    maybe_print_runtime_prolog(GoalIndex, Goal),
    maybe_pause_on_breakpoint.

note_breakpoint_fired :-
    debug_break_once,
    !,
    retractall(debug_break_fired),
    assertz(debug_break_fired).
note_breakpoint_fired.

maybe_pause_on_breakpoint :-
    dap_mode,
    !,
    ( current_predicate(dap_on_pause/0) -> dap_on_pause ; true ).
maybe_pause_on_breakpoint :-
    stream_property(user_input, tty(true)),
    !,
    color_format(user_error, magenta, "break> Enter=continue, l=source, s=stack, f=frame, p=goal, i=step-into, n=step-over, o=step-out, 'e <expr>'=eval MeTTa, ':break <spec>'/':clear'/':info'=manage breakpoints, c=continue without more breaks, q=abort~n", []),
    read_line_to_string(user_input, Input),
    handle_breakpoint_input(Input).
maybe_pause_on_breakpoint.

handle_breakpoint_input("l") :-
    print_breakpoint_source_form,
    maybe_pause_on_breakpoint.
handle_breakpoint_input("i") :-
    arm_step_into.
handle_breakpoint_input("n") :-
    arm_step_over.
handle_breakpoint_input("o") :-
    arm_step_out.
handle_breakpoint_input("f") :-
    print_breakpoint_frame,
    maybe_pause_on_breakpoint.
handle_breakpoint_input("p") :-
    print_breakpoint_goal,
    maybe_pause_on_breakpoint.
handle_breakpoint_input("c") :-
    retractall(debug_step_mode(_)),
    retractall(debug_step_armed),
    retractall(debug_break_continue),
    assertz(debug_break_continue).
handle_breakpoint_input("q") :-
    throw(debug_break_abort).
handle_breakpoint_input("s") :-
    print_call_stack,
    maybe_pause_on_breakpoint.
handle_breakpoint_input(Input) :-
    ( string_concat("e ", Expr, Input)
    ; string_concat(":eval ", Expr, Input)
    ),
    !,
    debug_repl_eval(Expr),
    maybe_pause_on_breakpoint.
handle_breakpoint_input(Input) :-
    string_concat(":break ", Spec, Input),
    !,
    debug_repl_add_break(Spec),
    maybe_pause_on_breakpoint.
handle_breakpoint_input(":clear") :-
    !,
    debug_repl_clear_breaks,
    maybe_pause_on_breakpoint.
handle_breakpoint_input(":info") :-
    !,
    debug_repl_show_breaks,
    maybe_pause_on_breakpoint.
handle_breakpoint_input(_) :-
    true.

% Evaluate a MeTTa expression typed at a breakpoint. Tracing is suspended for the
% duration (debug_eval_suspended) so the evaluation does not recurse into the
% debugger, and the expression's $variables are first unified with the current
% breakpoint's variable bindings so it can refer to them by name.
debug_repl_eval(ExprString) :-
    ( debug_eval_collect(ExprString, Results)
      -> print_repl_results(Results)
      ; true
    ).

% Parse a MeTTa expression string, bind its $variables to the current
% breakpoint's frame values, and collect all solutions with tracing suspended.
% Shared by the interactive REPL (e/:eval) and the DAP evaluate request.
debug_eval_collect(ExprString, Results) :-
    catch(sread(ExprString, Term, Env), Error, ( report_repl_error(Error), fail )),
    apply_break_context_vars(Env),
    setup_call_cleanup(
        assertz(debug_eval_suspended),
        findall(Out, eval(Term, Out), Results),
        retractall(debug_eval_suspended)
    ).

print_repl_results([]) :-
    !,
    color_format(user_error, cyan, "= <no results>~n", []).
print_repl_results(Results) :-
    forall(member(Result, Results),
           ( swrite(Result, Text),
             color_format(user_error, cyan, "= ~w~n", [Text]) )).

report_repl_error(Error) :-
    color_format(user_error, red, "eval error: ~w~n", [Error]).

% Bind the REPL expression's $variables (Name-Var from the fresh parse) to the
% values captured for the current breakpoint, where the names match.
apply_break_context_vars(Env) :-
    ( is_list(Env), break_context_bindings(Bindings)
      -> bind_named_vars(Env, Bindings)
      ; true
    ).

% Explicit recursion (not forall/2, which is \+ \+ and would undo the bindings).
bind_named_vars([], _).
bind_named_vars([Name-Var|Rest], Bindings) :-
    ( member(BName-BValue, Bindings), BName == Name
      -> Var = BValue
      ;  true
    ),
    bind_named_vars(Rest, Bindings).

break_context_bindings(Bindings) :-
    debug_break_context(_Stage, meta(_Index, _Line, compiled(_SourceExpr, Bindings)), _GoalIndex, _Goal),
    is_list(Bindings),
    !.
break_context_bindings([]).

% Live breakpoint management from the prompt. A spec containing ':' is a
% conditional breakpoint (head:condition), otherwise a goal-head breakpoint.
debug_repl_add_break(Spec) :-
    atom_string(SpecAtom, Spec),
    ( sub_atom(SpecAtom, _, 1, _, ':')
      -> ( catch(parse_break_condition_spec(SpecAtom, Head, Condition), _, fail)
           -> assertz(debug_break_condition(Head, Condition, SpecAtom)),
              color_format(user_error, magenta, "added conditional breakpoint ~w~n", [SpecAtom])
           ;  color_format(user_error, red, "could not parse condition ~w~n", [SpecAtom])
         )
      ;  assertz(debug_break_target(SpecAtom)),
         color_format(user_error, magenta, "added breakpoint on goal head ~w~n", [SpecAtom])
    ).

debug_repl_clear_breaks :-
    retractall(debug_break_target(_)),
    retractall(debug_break_condition(_, _, _)),
    retractall(debug_break_space(_)),
    retractall(debug_break_match_fail),
    retractall(debug_break_error),
    color_format(user_error, magenta, "cleared all breakpoints~n", []).

debug_repl_show_breaks :-
    color_format(user_error, magenta, "active breakpoints:~n", []),
    forall(debug_break_target(Head),
           color_format(user_error, magenta, "  head: ~w~n", [Head])),
    forall(debug_break_condition(_, _, Spec),
           color_format(user_error, magenta, "  condition: ~w~n", [Spec])),
    forall(debug_break_space(Space),
           color_format(user_error, magenta, "  space mutation: ~w~n", [Space])),
    ( debug_break_match_fail -> color_format(user_error, magenta, "  match failures~n", []) ; true ),
    ( debug_break_error -> color_format(user_error, magenta, "  error results~n", []) ; true ),
    ( \+ debug_break_target(_), \+ debug_break_condition(_, _, _),
      \+ debug_break_space(_), \+ debug_break_match_fail, \+ debug_break_error
      -> color_format(user_error, magenta, "  (none)~n", [])
      ; true
    ).

arm_step_into :-
    retractall(debug_break_continue),
    retractall(debug_step_mode(_)),
    retractall(debug_step_armed),
    assertz(debug_step_mode(into)),
    assertz(debug_step_armed).

arm_step_over :-
    retractall(debug_break_continue),
    current_call_stack_depth(Depth),
    retractall(debug_step_mode(_)),
    retractall(debug_step_armed),
    assertz(debug_step_mode(over(Depth))),
    assertz(debug_step_armed).

arm_step_out :-
    retractall(debug_break_continue),
    current_call_stack_depth(Depth),
    retractall(debug_step_mode(_)),
    retractall(debug_step_armed),
    assertz(debug_step_mode(out(Depth))),
    assertz(debug_step_armed).

set_breakpoint_context(Stage, Meta, GoalIndex, Goal) :-
    retractall(debug_break_context(_, _, _, _)),
    copy_term(Goal, GoalCopy),
    assertz(debug_break_context(Stage, Meta, GoalIndex, GoalCopy)).

print_breakpoint_frame :-
    debug_break_context(Stage, Meta, GoalIndex, Goal),
    !,
    print_breakpoint_overview(Stage, Meta, GoalIndex, Goal, frame).
print_breakpoint_frame.

print_breakpoint_goal :-
    debug_break_context(Stage, Meta, GoalIndex, Goal),
    !,
    print_breakpoint_overview(Stage, Meta, GoalIndex, Goal, goal).
print_breakpoint_goal.

print_breakpoint_overview(Stage, Meta, GoalIndex, Goal, frame) :-
    goal_path_text(GoalIndex, GoalPathText),
    runtime_goal_core_text(Goal, FrameText),
    color_format(user_error, magenta, "frame: ~w @ ~w~n", [FrameText, GoalPathText]),
    print_breakpoint_location(Meta),
    print_breakpoint_head(Goal),
    print_breakpoint_depth,
    print_breakpoint_frame_details(Stage, Goal),
    print_breakpoint_meta_vars(Meta).
print_breakpoint_overview(Stage, Meta, GoalIndex, Goal, goal) :-
    goal_path_text(GoalIndex, GoalPathText),
    runtime_goal_text(Stage, Goal, GoalText),
    color_format(user_error, magenta, "goal: ~w @ ~w~n", [GoalText, GoalPathText]),
    print_breakpoint_location(Meta),
    print_breakpoint_head(Goal),
    print_breakpoint_depth,
    print_breakpoint_frame_details(Stage, Goal),
    print_breakpoint_meta_vars(Meta).

print_breakpoint_meta_vars(meta(_Index, _Line, Kind)) :-
    print_compiled_vars(Kind).

print_breakpoint_location(meta(Index, Line, Kind)) :-
    meta_kind_label(Kind, KindLabel),
    color_format(user_error, magenta, "location: form #~w line ~w (~w)~n", [Index, Line, KindLabel]).

print_compiled_source_expr(compiled(SourceExpr, _Bindings)) :-
    !,
    swrite(SourceExpr, SourceText),
    color_format(user_error, magenta, "source expr: ~w~n", [SourceText]).
print_compiled_source_expr(compiled(SourceExpr)) :-
    !,
    swrite(SourceExpr, SourceText),
    color_format(user_error, magenta, "source expr: ~w~n", [SourceText]).
print_compiled_source_expr(_).

% Print MeTTa $variable bindings carried in a compiled meta Kind. Shown at
% breakpoints/steps and via the f/p inspection commands.
print_compiled_vars(compiled(_SourceExpr, Bindings)) :-
    !,
    print_var_bindings(Bindings).
print_compiled_vars(_).

print_var_bindings(Bindings) :-
    ( is_list(Bindings) -> true ; fail ),
    !,
    forall(member(Name-Value, Bindings),
           print_var_binding(Name, Value)).
print_var_bindings(_).

print_var_binding(Name, Value) :-
    ( var(Value)
      -> color_format(user_error, magenta, "var $~w = _~n", [Name])
      ;  swrite(Value, ValueText),
         color_format(user_error, magenta, "var $~w = ~w~n", [Name, ValueText])
    ).

print_breakpoint_head(Goal) :-
    goal_head_name(Goal, Head),
    color_format(user_error, magenta, "head: ~w~n", [Head]).

print_breakpoint_depth :-
    current_call_stack_depth(Depth),
    color_format(user_error, magenta, "depth: ~w~n", [Depth]).

print_breakpoint_source_form :-
    debug_break_context(_Stage, Meta, _GoalIndex, _Goal),
    Meta = meta(Index, Line, Kind),
    breakpoint_source_form(Meta, FormStr),
    !,
    meta_kind_label(Kind, KindLabel),
    color_format(user_error, magenta, "source: form #~w line ~w (~w)~n", [Index, Line, KindLabel]),
    debug_format("~w~n", [FormStr]).
print_breakpoint_source_form :-
    debug_break_context(_Stage, meta(Index, Line, Kind), _GoalIndex, _Goal),
    !,
    meta_kind_label(Kind, KindLabel),
    color_format(user_error, magenta, "source: form #~w line ~w (~w) not available~n", [Index, Line, KindLabel]).
print_breakpoint_source_form.

breakpoint_source_form(meta(_Index, _Line, compiled(SourceExpr, _Bindings)), FormStr) :-
    !,
    swrite(SourceExpr, FormStr).
breakpoint_source_form(meta(_Index, _Line, compiled(SourceExpr)), FormStr) :-
    !,
    swrite(SourceExpr, FormStr).
breakpoint_source_form(meta(Index, Line, Kind), FormStr) :-
    debug_source_form(Index, Line, Kind, FormStr).

print_breakpoint_frame_details(Stage, Goal) :-
    breakpoint_frame_args(Goal, Args),
    print_breakpoint_args(1, Args),
    ( Stage == success,
      breakpoint_frame_result(Goal, Result)
      -> color_format(user_error, magenta, "result: ~w~n", [Result])
      ; true
    ).

breakpoint_frame_args(Goal, Args) :-
    strip_trace_wrappers(Goal, CleanGoal),
    compound(CleanGoal),
    CleanGoal =.. [_|AllArgs],
    append(Args, [_], AllArgs),
    !.
breakpoint_frame_args(_, []).

breakpoint_frame_result(Goal, Result) :-
    strip_trace_wrappers(Goal, CleanGoal),
    compound(CleanGoal),
    CleanGoal =.. [_|AllArgs],
    append(_, [Result], AllArgs).

print_breakpoint_args(_, []).
print_breakpoint_args(Index, [Arg|Rest]) :-
    color_format(user_error, magenta, "arg~w: ~w~n", [Index, Arg]),
    NextIndex is Index + 1,
    print_breakpoint_args(NextIndex, Rest).

print_breakpoint_reason(Stage, Goal) :-
    breakpoint_reason(Stage, Goal, Reason, Details),
    !,
    color_format(user_error, magenta, "reason: ~w~n", [Reason]),
    print_breakpoint_details(Details).
print_breakpoint_reason(_, _).

print_breakpoint_details([]).
print_breakpoint_details([Label-Value|Rest]) :-
    color_format(user_error, magenta, "match: ~w = ~w~n", [Label, Value]),
    print_breakpoint_details(Rest).

print_breakpoint_hit_count :-
    debug_break_hits(HitCount),
    color_format(user_error, magenta, "hit: ~w~n", [HitCount]).

breakpoint_reason(Stage, Goal, Reason, Details) :-
    breakpoint_condition_reason(Stage, Goal, Spec, Details),
    !,
    format(atom(Reason), "matched condition ~w", [Spec]).
breakpoint_reason(enter, Goal, Reason, []) :-
    breakpoint_goal_enabled(Goal),
    goal_head_name(Goal, Head),
    !,
    format(atom(Reason), "entered goal head ~w", [Head]).
breakpoint_reason(success, Goal, Reason, []) :-
    debug_break_error,
    goal_result_is_error(Goal),
    breakpoint_frame_result(Goal, Result),
    swrite(Result, ResultText),
    format(atom(Reason), "returned error ~w", [ResultText]).

breakpoint_condition_reason(Stage, Goal, Spec, Details) :-
    goal_head_name(Goal, Head),
    debug_break_condition(Head, Condition, Spec),
    breakpoint_condition_matches(Stage, Goal, Condition),
    breakpoint_condition_details(Stage, Goal, Condition, Details).

breakpoint_condition_details(Stage, Goal, and(Left, Right), Details) :-
    breakpoint_condition_details(Stage, Goal, Left, LeftDetails),
    breakpoint_condition_details(Stage, Goal, Right, RightDetails),
    append(LeftDetails, RightDetails, Details).
breakpoint_condition_details(Stage, Goal, or(Left, Right), Details) :-
    ( breakpoint_condition_matches(Stage, Goal, Left)
      -> breakpoint_condition_details(Stage, Goal, Left, Details)
      ; breakpoint_condition_details(Stage, Goal, Right, Details)
    ).
breakpoint_condition_details(_Stage, Goal, condition(ArgIndex, _Op, _Expected), [Label-Actual]) :-
    strip_trace_wrappers(Goal, CleanGoal),
    compound(CleanGoal),
    CleanGoal =.. [_|AllArgs],
    append(InputArgs, [_], AllArgs),
    nth1(ArgIndex, InputArgs, Actual),
    format(atom(Label), "arg~w", [ArgIndex]).
breakpoint_condition_details(success, Goal, result_condition(_Op, _Expected), ['result'-Actual]) :-
    strip_trace_wrappers(Goal, CleanGoal),
    compound(CleanGoal),
    CleanGoal =.. [_|AllArgs],
    append(_, [Actual], AllArgs).

debug_header(Category, Index, Line, Kind) :-
    debug_header_label(Index, Line, Kind, Label),
    color_format(user_error, cyan, "[DEBUG ~w ~w]~n", [Category, Label]).

debug_header_label(space, 0, space, "space") :-
    !.
debug_header_label(Index, Line, Kind, Label) :-
    meta_kind_label(Kind, KindLabel),
    format(atom(Label), "#~w line ~w ~w", [Index, Line, KindLabel]).

meta_kind_label(compiled(_, _), compiled) :-
    !.
meta_kind_label(compiled(_), compiled) :-
    !.
meta_kind_label(Kind, Kind).

source_text(runnable, FormStr, Display) :-
    !,
    format(atom(Display), "!~w", [FormStr]).
source_text(_, FormStr, FormStr).

print_result_line(Result) :-
    swrite(Result, PrettyResult),
    debug_format("~w~n", [PrettyResult]).

print_indented_block(Indent, Text) :-
    split_string(Text, "\n", "", Lines),
    forall(member(Line, Lines), print_indented_line(Indent, Line)).

print_indented_line(_, "") :-
    debug_nl.
print_indented_line(Indent, Line) :-
    debug_format("~w~s~n", [Indent, Line]).

clause_text(Clause, Text) :-
    with_output_to(string(Text), portray_clause(current_output, Clause)).

goal_text(Goal, Text) :-
    strip_trace_wrappers(Goal, CleanGoal),
    clause_text((:- CleanGoal), Text).

runtime_goal_line(Stage, GoalIndex, GoalPathText, Goal, Line) :-
    runtime_goal_text(Stage, Goal, GoalText),
    runtime_stage_label(Stage, StageLabel),
    runtime_goal_prefix(GoalIndex, GoalPathText, Prefix),
    runtime_color(Stage, Color),
    format(atom(BaseLine), "~w~w~w", [Prefix, StageLabel, GoalText]),
    colorize_atom(Color, BaseLine, Line).

print_runtime_goal_line(Stage, GoalIndex, GoalPathText, Goal) :-
    runtime_goal_text(Stage, Goal, GoalText),
    runtime_stage_label(Stage, StageLabel),
    runtime_goal_prefix(GoalIndex, GoalPathText, Prefix),
    runtime_color(Stage, Color),
    format(atom(BaseLine), "~w~w~w", [Prefix, StageLabel, GoalText]),
    forall(debug_stream(Stream), print_runtime_goal_line_on(Stream, Color, BaseLine)).

print_runtime_goal_line_on(Stream, Color, BaseLine) :-
    stream_property(Stream, tty(true)),
    !,
    color_format(Stream, Color, "~w~n", [BaseLine]).
print_runtime_goal_line_on(Stream, _Color, BaseLine) :-
    format(Stream, "~w~n", [BaseLine]).

% Compact "watch" rendering: one indented ENTER/OK/REDO/FAIL line per event,
% nested by call-stack depth, with no per-event header or stack dump.
print_indented_runtime_line(Stage, Goal) :-
    current_call_stack_depth(Depth),
    Spaces is max(0, (Depth - 1) * 2),
    format(atom(Pad), "~*c", [Spaces, 0' ]),
    runtime_goal_text(Stage, Goal, GoalText),
    runtime_stage_label(Stage, StageLabel),
    runtime_color(Stage, Color),
    format(atom(BaseLine), "~w~w~w", [Pad, StageLabel, GoalText]),
    forall(debug_stream(Stream), print_runtime_goal_line_on(Stream, Color, BaseLine)).

runtime_event_visible(_, Goal) :-
    ( debug_category(all)
    ; debug_category(runtime_prolog)
    ),
    !,
    nonvar(Goal),
    runtime_depth_visible.
runtime_event_visible(_, Goal) :-
    strip_trace_wrappers(Goal, CleanGoal),
    runtime_user_visible_goal(CleanGoal),
    runtime_depth_visible.

runtime_depth_visible :-
    debug_max_depth(MaxDepth),
    !,
    current_call_stack_depth(Depth),
    Depth =< MaxDepth.
runtime_depth_visible.

runtime_user_visible_goal(Goal) :-
    metta_debug_goal(Goal),
    \+ runtime_internal_goal(Goal).

metta_debug_goal(Goal) :-
    var(Goal),
    !,
    fail.
metta_debug_goal(Goal) :-
    compound(Goal),
    Goal =.. [Name|AllArgs],
    append(_, [_], AllArgs),
    fun(Name).

runtime_internal_goal(findall(_, _, _)).
runtime_internal_goal((_,_)).
runtime_internal_goal((_;_)).
runtime_internal_goal((_->_)).
runtime_internal_goal(once(_)).
runtime_internal_goal(true).
runtime_internal_goal(Goal) :-
    compound(Goal),
    Goal =.. [=|_].

runtime_goal_text(Stage, Goal, Text) :-
    strip_trace_wrappers(Goal, CleanGoal),
    runtime_goal_core_text(CleanGoal, CoreText),
    ( Stage == success,
      runtime_goal_result(CleanGoal, ResultText)
      -> format(atom(Text), "~w => ~w", [CoreText, ResultText])
      ; Text = CoreText
    ).

runtime_goal_core_text(Goal, Text) :-
    var(Goal),
    !,
    Text = '<goal>'.
runtime_goal_core_text(findall(_, InnerGoal, _), Text) :-
    !,
    runtime_goal_core_text(InnerGoal, InnerText),
    format(atom(Text), "(collect ~w)", [InnerText]).
runtime_goal_core_text((_,_), Text) :-
    !,
    Text = '(sequence)'.
runtime_goal_core_text((_;_), Text) :-
    !,
    Text = '(choice)'.
runtime_goal_core_text((_->_), Text) :-
    !,
    Text = '(branch)'.
runtime_goal_core_text(once(InnerGoal), Text) :-
    !,
    runtime_goal_core_text(InnerGoal, InnerText),
    format(atom(Text), "(once ~w)", [InnerText]).
runtime_goal_core_text(true, 'true') :-
    !.
runtime_goal_core_text(Goal, Text) :-
    compound(Goal),
    Goal =.. [Name|AllArgs],
    ( AllArgs = [] -> Args = []
    ; append(Args, [_], AllArgs)
    ),
    metta_expr_text(Name, Args, Text),
    !.
runtime_goal_core_text(Goal, Text) :-
    swrite(Goal, Text).

runtime_goal_result(Goal, ResultText) :-
    compound(Goal),
    Goal =.. [_|AllArgs],
    append(_, [Result], AllArgs),
    swrite(Result, ResultText).

runtime_stage_label(enter, 'ENTER  ').
runtime_stage_label(success, 'OK     ').
runtime_stage_label(redo, 'REDO   ').
runtime_stage_label(fail, 'FAIL   ').

runtime_color(enter, yellow).
runtime_color(success, green).
runtime_color(redo, blue).
runtime_color(fail, red).

runtime_goal_prefix(GoalIndex, GoalPathText, Prefix) :-
    ( debug_category(all)
    ; debug_category(runtime_prolog)
    ),
    !,
    goal_indent(GoalIndex, Indent),
    format(atom(Prefix), "~w[~w] ", [Indent, GoalPathText]).
runtime_goal_prefix(GoalIndex, _, Prefix) :-
    goal_indent(GoalIndex, Indent),
    Prefix = Indent.

color_code(cyan, '36').
color_code(blue, '34').
color_code(yellow, '33').
color_code(green, '32').
color_code(red, '31').
color_code(magenta, '35').

color_format(_Stream, Color, Format, Args) :-
    forall(debug_stream(Stream), color_format_one(Stream, Color, Format, Args)).

color_format_one(Stream, Color, Format, Args) :-
    stream_property(Stream, tty(true)),
    !,
    color_code(Color, Code),
    format(Stream, '\e[~wm', [Code]),
    format(Stream, Format, Args),
    format(Stream, '\e[0m', []).
color_format_one(Stream, _Color, Format, Args) :-
    format(Stream, Format, Args).

colorize_atom(Color, Text, Colored) :-
    color_code(Color, Code),
    format(atom(Colored), '\e[~wm~w\e[0m', [Code, Text]).

metta_expr_text(Name, Args, Text) :-
    runtime_head_label(Name, Head),
    ( Args == []
      -> format(atom(Text), "(~w)", [Head])
      ; maplist(swrite, Args, ArgStrs),
        atomic_list_concat(ArgStrs, ' ', ArgsAtom),
        format(atom(Text), "(~w ~w)", [Head, ArgsAtom])
    ).

runtime_head_label(=, 'unify') :- !.
runtime_head_label(Name, Name).

maybe_print_runtime_prolog(GoalIndex, Goal) :-
    ( debug_category(all)
    ; debug_category(runtime_prolog)
    ),
    !,
    goal_indent(GoalIndex, Indent),
    goal_text(Goal, Text),
    print_indented_block(Indent, Text).
maybe_print_runtime_prolog(_, _).

goal_head_name(Goal, Head) :-
    strip_trace_wrappers(Goal, CleanGoal),
    nonvar(CleanGoal),
    ( atomic(CleanGoal)
      -> Head = CleanGoal
      ; compound(CleanGoal)
        -> functor(CleanGoal, Head, _)
    ).

goal_path_text(Path, Text) :-
    ( integer(Path)
      -> format(atom(Text), "#~w", [Path])
      ; is_list(Path)
        -> atomic_list_concat(Path, '.', Segments),
           format(atom(Text), "#~w", [Segments])
      ; format(atom(Text), "#~w", [Path])
    ).

goal_indent(Path, Indent) :-
    goal_depth(Path, Depth),
    Spaces is max(0, (Depth - 1) * 2),
    format(atom(Indent), "~*c", [Spaces, 0' ]).

goal_depth(Path, Depth) :-
    integer(Path),
    !,
    Depth = 1.
goal_depth(Path, Depth) :-
    is_list(Path),
    !,
    length(Path, Depth).
goal_depth(_, 1).

current_call_stack_depth(Depth) :-
    metta_call_stack(Stack),
    length(Stack, Depth).

strip_trace_wrappers(Goal, CleanGoal) :-
    var(Goal),
    !,
    CleanGoal = Goal.
strip_trace_wrappers(trace_goal_execution(_, _, InnerGoal), CleanGoal) :-
    !,
    strip_trace_wrappers(InnerGoal, CleanGoal).
strip_trace_wrappers((A,B), (CleanA,CleanB)) :-
    !,
    strip_trace_wrappers(A, CleanA),
    strip_trace_wrappers(B, CleanB).
strip_trace_wrappers((A;B), (CleanA;CleanB)) :-
    !,
    strip_trace_wrappers(A, CleanA),
    strip_trace_wrappers(B, CleanB).
strip_trace_wrappers((A->B), (CleanA->CleanB)) :-
    !,
    strip_trace_wrappers(A, CleanA),
    strip_trace_wrappers(B, CleanB).
strip_trace_wrappers(findall(Template, Goal, Bag), findall(Template, CleanGoal, Bag)) :-
    !,
    strip_trace_wrappers(Goal, CleanGoal).
strip_trace_wrappers(once(Goal), once(CleanGoal)) :-
    !,
    strip_trace_wrappers(Goal, CleanGoal).
strip_trace_wrappers(Goal, Goal).

leaf_goal(Goal) :-
    strip_trace_wrappers(Goal, CleanGoal),
    \+ composite_goal(CleanGoal).

composite_goal((_,_)).
composite_goal((_;_)).
composite_goal((_->_)).
composite_goal(findall(_, _, _)).
composite_goal(once(_)).

print_goals([]) :-
    debug_format("<no goals>~n", []).
print_goals([Goal]) :-
    !,
    clause_text((:- Goal), Text),
    debug_format("~s", [Text]).
print_goals([Goal|Goals]) :-
    clause_text((:- Goal), Text),
    debug_format("~s", [Text]),
    print_goals(Goals).

runtime_option(Arg) :-
    memberchk(Arg, ['--debug-help', '--help-debug']),
    !.
runtime_option(Arg) :-
    memberchk(Arg, [silent, '--silent', '-s', mork, '--debug', '--debug-all']),
    !.
runtime_option(Arg) :-
    atom(Arg),
    atom_concat('--debug-goal=', _, Arg),
    !.
runtime_option(Arg) :-
    atom(Arg),
    atom_concat('--debug-break=', _, Arg),
    !.
runtime_option(Arg) :-
    atom(Arg),
    atom_concat('--debug-break-if=', _, Arg),
    !.
runtime_option(Arg) :-
    atom(Arg),
    atom_concat('--debug-break-space=', _, Arg),
    !.
runtime_option(Arg) :-
    memberchk(Arg, ['--debug-break-match-fail', '--debug-break-error', '--debug-jsonl', '--debug-indent']),
    !.
runtime_option(Arg) :-
    atom(Arg),
    atom_concat('--debug-format=', _, Arg),
    !.
runtime_option(Arg) :-
    memberchk(Arg, ['--debug-break-once']),
    !.
runtime_option(Arg) :-
    atom(Arg),
    atom_concat('--debug-break-skip=', _, Arg),
    !.
runtime_option(Arg) :-
    atom(Arg),
    atom_concat('--debug-output=', _, Arg),
    !.
runtime_option(Arg) :-
    atom(Arg),
    atom_concat('--debug-depth=', _, Arg),
    !.
runtime_option(Arg) :-
    atom(Arg),
    atom_concat('--debug-max-events=', _, Arg),
    !.
runtime_option(Arg) :-
    atom(Arg),
    atom_concat('--debug=', _, Arg).

file_argument(Args, File) :-
    member(File, Args),
    \+ runtime_option(File),
    \+ ( atom(File), atom_concat('--', _, File) ),
    !.

mork_enabled(Args) :-
    memberchk(mork, Args).

debug_help_requested(Args) :-
    member(Arg, Args),
    memberchk(Arg, ['--debug-help', '--help-debug']).

print_debug_help :-
    format("PeTTa Debugger~n~n", []),
    format("Usage:~n", []),
    format("  sh run.sh <file.metta> [debug options]~n", []),
    format("  sh debug.sh <file.metta> [debug options]~n~n", []),
    format("Quick start (new to PeTTa? start here):~n", []),
    format("  sh debug.sh watch <file.metta>             Trace what your program does, nested by call.~n", []),
    format("  sh debug.sh watch <file.metta> <function>  Trace just one function's calls (less noise).~n", []),
    format("  sh debug.sh <file.metta> --debug-break=<function>   Stop at a function to inspect it.~n", []),
    format("Then reach for the options below when you need finer control.~n~n", []),
    format("Debug options:~n", []),
    format("  --debug                 Enable all debug categories~n", []),
    format("  --debug-all             Same as --debug~n", []),
    format("  --debug=<cats>          Comma-separated categories~n", []),
    format("  --debug-goal=<heads>    Restrict runtime events to goal head names~n", []),
    format("  --debug-break=<heads>   Break when entering matching goal head names~n", []),
    format("  --debug-break-if=<spec> Break on conditional match, e.g. fib:arg1<0 or fib:arg1=2&result=0 or fib:arg1<0|result=0~n", []),
    format("                          Operators: = != > < >= <= and ~~ (substring/structural contains), e.g. match:arg3~~set or f:arg1=(a b)~n", []),
    format("  --debug-break-space=<spaces>  Break on add/remove mutation of the named space(s), e.g. &self (comma-separated, * = any)~n", []),
    format("  --debug-break-match-fail      Break when a space match fails~n", []),
    format("  --debug-break-error           Break when a goal returns an Error term~n", []),
    format("  --debug-break-once      Disable further breakpoints after the first hit~n", []),
    format("  --debug-break-skip=<n>  Skip the first n breakpoint hits before stopping~n", []),
    format("  --debug-output=<file>   Write a plain-text copy of debugger output to file~n", []),
    format("  --debug-format=json     Emit events as machine-readable JSON lines (alias: --debug-jsonl)~n", []),
    format("  --debug-depth=<n>       Limit runtime trace output to call-stack depth n~n", []),
    format("  --debug-max-events=<n>  Stop debug output after n emitted events~n", []),
    format("  --debug-indent          Compact trace: one indented line per event, nested by call (used by 'watch')~n", []),
    format("  --silent                Suppress the legacy compile/run pretty-print output~n", []),
    format("  --debug-help            Show this help text~n~n", []),
    format("TTY breakpoint commands:~n", []),
    format("  l  show source form for current pause~n", []),
    format("  s  show stack~n", []),
    format("  f  show current frame~n", []),
    format("  p  show current goal~n", []),
    format("  i  step into~n", []),
    format("  n  step over~n", []),
    format("  o  step out~n", []),
    format("  e <expr>      evaluate a MeTTa expression in the current context~n", []),
    format("  :break <spec>  add a breakpoint live (head, or head:condition)~n", []),
    format("  :clear         clear all breakpoints~n", []),
    format("  :info          list active breakpoints~n", []),
    format("  c  continue without more breakpoint pauses~n", []),
    format("  q  abort~n~n", []),
    format("Categories:~n", []),
    forall((debug_category_name(Category), debug_category_label(Category, Label)), format("  ~w~n", [Label])),
    format("~nExamples:~n", []),
    format("  sh debug.sh watch tests/debugger/fixtures/fib_buggy.metta fib~n", []),
    format("  sh debug.sh examples/fib.metta --debug=runtime~n", []),
    format("  sh debug.sh examples/fib.metta --debug=runtime-leaf --debug-goal=fib~n", []),
    format("  sh debug.sh examples/fib.metta --debug=runtime-prolog --debug-goal=fib --silent~n", []),
    format("  sh debug.sh examples/fib.metta --debug=runtime --debug-depth=3 --debug-max-events=20 --silent~n", []),
    format("  sh debug.sh examples/fib.metta --debug-break=fib --debug-goal=fib --silent~n", []),
    format("  sh debug.sh tests/debugger/fixtures/fib_buggy.metta --debug-break-if=fib:arg1<0 --debug-goal=fib --silent~n", []),
    format("  sh debug.sh tests/debugger/fixtures/fib_buggy.metta --debug-break-if=fib:result=0 --debug-break-once --debug-goal=fib --silent~n", []),
    format("  sh debug.sh tests/debugger/fixtures/fib_buggy.metta --debug-break-if='fib:arg1<0|result=0' --debug-break-skip=1 --debug-goal=fib --silent~n", []),
    format("  sh debug.sh tests/debugger/fixtures/fib_buggy.metta --debug-break-if=fib:arg1=2&result=0 --debug-break-once --debug-goal=fib --silent~n", []),
    format("  sh debug.sh examples/fib.metta --debug=runtime --debug-output=trace.log --silent~n", []),
    format("  sh debug.sh examples/fib.metta --debug=source,parse,translate,result --silent~n", []),
    format("  sh debug.sh examples/spaces.metta --debug=space-match-fail~n", []).

register_debug_source_form(Index, Line, Kind, FormStr) :-
    retractall(debug_source_form(Index, Line, Kind, _)),
    assertz(debug_source_form(Index, Line, Kind, FormStr)).

debug_stream(user_error).
debug_stream(Stream) :-
    debug_output_stream(Stream).

debug_format(Format, Args) :-
    forall(debug_stream(Stream), format(Stream, Format, Args)).

debug_nl :-
    forall(debug_stream(Stream), nl(Stream)).

close_debug_output_streams :-
    forall(retract(debug_output_stream(Stream)), close(Stream)).
