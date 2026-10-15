% PeTTa Debug Adapter Protocol (DAP) server.
%
% Speaks DAP JSON over stdin/stdout (Content-Length framed). Single-threaded:
% the interpreter runs synchronously and, when a breakpoint is hit, enters a
% nested message loop (dap_stopped_loop) that answers stackTrace / scopes /
% variables / evaluate requests until a continue / step / disconnect request
% resumes execution.
%
% It maps DAP requests onto the debugger primitives built in M1-M6:
%   setBreakpoints          -> debug_break_line/1
%   setFunctionBreakpoints  -> debug_break_target/1
%   setExceptionBreakpoints -> debug_break_error/0
%   launch + configurationDone -> load_metta_file/2 with tracing
%   stackTrace              -> metta_call_stack/1
%   scopes / variables      -> break_context_bindings/1 (M3 var inspection)
%   evaluate                -> debug_eval_collect/2     (M5 REPL eval)
%   continue/next/stepIn/stepOut -> arm_step_* (M-step machinery)
% Breakpoint pauses arrive here through the dap_on_pause/0 hook called by
% maybe_pause_on_breakpoint/0 when dap_mode/0 is set.

:- ensure_loaded('../metta').
:- use_module(library(http/json)).

:- dynamic working_dir/1.   % also asserted by main.pl; declare so dap_run can test it
:- dynamic dap_seq/1.
:- dynamic dap_launch_file/1.
:- dynamic dap_configured/0.
:- dynamic dap_launched/0.
:- dynamic dap_started/0.
:- dynamic dap_terminated/0.

% --- Entry point ---------------------------------------------------------

dap_server :-
    current_output(Out),
    set_stream(Out, encoding(utf8)),
    set_stream(user_input, encoding(utf8)),
    set_stream(user_error, encoding(utf8)),
    nb_setval(dap_channel, Out),
    % Keep the protocol channel clean: program/debug output goes to stderr.
    set_output(user_error),
    retractall(silent(_)), assertz(silent(true)),
    retractall(dap_seq(_)), assertz(dap_seq(1)),
    retractall(dap_terminated),
    assertz(dap_mode),
    catch(dap_loop, Error, ( print_message(error, Error), true )),
    halt.

dap_loop :-
    ( dap_read_message(Msg)
      -> dap_handle(Msg),
         ( dap_terminated -> true ; dap_loop )
      ;  true   % EOF on stdin: stop serving
    ).

dap_handle(Msg) :-
    ( get_dict(command, Msg, Cmd) -> true ; Cmd = "" ),
    ( dap_request(Cmd, Msg) -> true ; dap_respond(Msg, _{}) ).

% --- Pre-run / configuration requests ------------------------------------

dap_request("initialize", Msg) :-
    !,
    dap_respond(Msg, _{ supportsConfigurationDoneRequest: true,
                        supportsFunctionBreakpoints: true,
                        supportsEvaluateForHovers: true,
                        supportsTerminateRequest: true }),
    dap_event(initialized, _{}).
dap_request("launch", Msg) :-
    !,
    ( dap_arg(Msg, program, Prog)
      -> atom_string(ProgAtom, Prog),
         retractall(dap_launch_file(_)),
         assertz(dap_launch_file(ProgAtom))
      ; true
    ),
    assertz(dap_launched),
    dap_respond(Msg, _{}),
    dap_maybe_start.
dap_request("setBreakpoints", Msg) :-
    !,
    retractall(debug_break_line(_)),
    ( dap_arg(Msg, breakpoints, BPs), is_list(BPs)
      -> forall(member(BP, BPs),
                ( get_dict(line, BP, L) -> assertz(debug_break_line(L)) ; true ))
      ; ( dap_arg(Msg, lines, Lines), is_list(Lines)
          -> forall(member(L, Lines), assertz(debug_break_line(L)))
          ;  true )
    ),
    findall(_{verified:true, line:L}, debug_break_line(L), Verified),
    dap_respond(Msg, _{breakpoints: Verified}).
dap_request("setFunctionBreakpoints", Msg) :-
    !,
    retractall(debug_break_target(_)),
    ( dap_arg(Msg, breakpoints, BPs), is_list(BPs)
      -> forall(member(BP, BPs),
                ( get_dict(name, BP, N) -> atom_string(NA, N), assertz(debug_break_target(NA)) ; true ))
      ; true
    ),
    findall(_{verified:true}, debug_break_target(_), Verified),
    dap_respond(Msg, _{breakpoints: Verified}).
dap_request("setExceptionBreakpoints", Msg) :-
    !,
    retractall(debug_break_error),
    ( dap_arg(Msg, filters, Filters), is_list(Filters), Filters \== []
      -> assertz(debug_break_error)
      ; true
    ),
    dap_respond(Msg, _{}).
dap_request("configurationDone", Msg) :-
    !,
    assertz(dap_configured),
    dap_respond(Msg, _{}),
    dap_maybe_start.
dap_request("threads", Msg) :-
    !,
    dap_respond(Msg, _{threads: [ _{id:1, name:"metta-main"} ]}).
dap_request("disconnect", Msg) :-
    !,
    dap_respond(Msg, _{}),
    assertz(dap_terminated).
dap_request("terminate", Msg) :-
    !,
    dap_respond(Msg, _{}),
    assertz(dap_terminated).
dap_request(_, Msg) :-
    dap_respond(Msg, _{}).

% Start running once both launch and configurationDone have arrived.
dap_maybe_start :-
    ( dap_launched, dap_configured, \+ dap_started
      -> assertz(dap_started),
         dap_run
      ; true
    ).

dap_run :-
    ( dap_launch_file(File)
      -> file_directory_name(File, Dir),
         ( ( Dir == '' ; working_dir(_) ) -> true ; assertz(working_dir(Dir)) ),
         catch(load_metta_file(File, _Results), debug_break_abort, true)
      ; true
    ),
    ( dap_terminated
      -> true
      ; dap_event(terminated, _{}),
        dap_event(exited, _{exitCode: 0}),
        assertz(dap_terminated)
    ).

% --- Stopped (breakpoint) message loop -----------------------------------

% Called from maybe_pause_on_breakpoint/0 (debugger.pl) when a breakpoint hits.
dap_on_pause :-
    dap_event(stopped, _{reason: "breakpoint", threadId: 1, allThreadsStopped: true}),
    dap_stopped_loop.

dap_stopped_loop :-
    ( dap_read_message(Msg)
      -> ( get_dict(command, Msg, Cmd) -> true ; Cmd = "" ),
         dap_stopped(Cmd, Msg, Resume),
         ( Resume == resume -> true ; dap_stopped_loop )
      ;  true   % EOF: resume so the run can finish
    ).

dap_stopped("stackTrace", Msg, stay) :-
    !,
    dap_stack_frames(Frames),
    length(Frames, N),
    dap_respond(Msg, _{stackFrames: Frames, totalFrames: N}).
dap_stopped("scopes", Msg, stay) :-
    !,
    dap_respond(Msg, _{scopes: [ _{name:"Locals", variablesReference:1, expensive:false} ]}).
dap_stopped("variables", Msg, stay) :-
    !,
    dap_variables(Vars),
    dap_respond(Msg, _{variables: Vars}).
dap_stopped("evaluate", Msg, stay) :-
    !,
    ( dap_arg(Msg, expression, Expr0)
      -> dap_to_string(Expr0, Expr),
         ( debug_eval_collect(Expr, Results) -> dap_results_text(Results, Text) ; Text = "error" )
      ; Text = ""
    ),
    dap_respond(Msg, _{result: Text, variablesReference: 0}).
dap_stopped("continue", Msg, resume) :-
    !,
    retractall(debug_step_mode(_)),
    retractall(debug_step_armed),
    dap_respond(Msg, _{allThreadsContinued: true}).
dap_stopped("next", Msg, resume) :-
    !,
    dap_respond(Msg, _{}),
    arm_step_over.
dap_stopped("stepIn", Msg, resume) :-
    !,
    dap_respond(Msg, _{}),
    arm_step_into.
dap_stopped("stepOut", Msg, resume) :-
    !,
    dap_respond(Msg, _{}),
    arm_step_out.
dap_stopped("threads", Msg, stay) :-
    !,
    dap_respond(Msg, _{threads: [ _{id:1, name:"metta-main"} ]}).
dap_stopped("setFunctionBreakpoints", Msg, stay) :-
    !,
    dap_request("setFunctionBreakpoints", Msg).
dap_stopped("setBreakpoints", Msg, stay) :-
    !,
    dap_request("setBreakpoints", Msg).
dap_stopped("disconnect", Msg, resume) :-
    !,
    dap_respond(Msg, _{}),
    assertz(dap_terminated),
    throw(debug_break_abort).
dap_stopped("terminate", Msg, resume) :-
    !,
    dap_respond(Msg, _{}),
    assertz(dap_terminated),
    throw(debug_break_abort).
dap_stopped(_, Msg, stay) :-
    dap_respond(Msg, _{}).

% --- DAP body builders ---------------------------------------------------

dap_stack_frames(Frames) :-
    ( metta_call_stack(Stack) -> true ; Stack = [] ),
    dap_break_line(Line),
    dap_program_source(Source),
    dap_frames(Stack, Line, Source, 1, Frames).

dap_frames([], _, _, _, []).
dap_frames([Goal|Rest], Line, Source, Id, [Frame|Frames]) :-
    dap_to_string(Goal, Name),
    Frame = _{ id: Id, name: Name, line: Line, column: 1, source: Source },
    NextId is Id + 1,
    dap_frames(Rest, 0, Source, NextId, Frames).

dap_break_line(Line) :-
    debug_break_context(_, meta(_, Line, _), _, _),
    integer(Line),
    !.
dap_break_line(0).

dap_program_source(Source) :-
    ( dap_launch_file(File)
      -> file_base_name(File, Base),
         dap_to_string(Base, Name),
         dap_to_string(File, Path),
         Source = _{name: Name, path: Path}
      ; Source = _{name: "metta"}
    ).

dap_variables(Vars) :-
    ( break_context_bindings(Bindings) -> true ; Bindings = [] ),
    dap_binding_vars(Bindings, BindingVars),
    ( dap_break_result(ResultText)
      -> Vars = [ _{name:"result", value:ResultText, variablesReference:0} | BindingVars ]
      ; Vars = BindingVars
    ).

dap_binding_vars([], []).
dap_binding_vars([Name-Value|Rest], [Var|Vars]) :-
    format(atom(NameAtom), "$~w", [Name]),
    dap_value_text(Value, ValueText),
    Var = _{name: NameAtom, value: ValueText, variablesReference: 0},
    dap_binding_vars(Rest, Vars).

dap_break_result(Text) :-
    debug_break_context(success, _, _, Goal),
    breakpoint_frame_result(Goal, Result),
    nonvar(Result),
    dap_value_text(Result, Text).

dap_value_text(Value, Text) :-
    ( var(Value) -> Text = "_"
    ; swrite(Value, S), dap_to_string(S, Text)
    ).

dap_results_text([], "<no results>") :- !.
dap_results_text(Results, Text) :-
    maplist(dap_result_atom, Results, Atoms),
    atomic_list_concat(Atoms, ' ', Joined),
    dap_to_string(Joined, Text).

dap_result_atom(Result, Atom) :-
    swrite(Result, S),
    atom_string(Atom, S).

% --- Protocol I/O --------------------------------------------------------

next_seq(Seq) :-
    retract(dap_seq(Seq)),
    Next is Seq + 1,
    assertz(dap_seq(Next)).

dap_respond(Request, Body) :-
    ( get_dict(seq, Request, ReqSeq) -> true ; ReqSeq = 0 ),
    ( get_dict(command, Request, Cmd) -> true ; Cmd = "" ),
    next_seq(Seq),
    dap_write_message(_{ seq: Seq, type: "response", request_seq: ReqSeq,
                         success: true, command: Cmd, body: Body }).

dap_event(Event, Body) :-
    next_seq(Seq),
    dap_write_message(_{ seq: Seq, type: "event", event: Event, body: Body }).

dap_write_message(Dict) :-
    with_output_to(string(Json), json_write_dict(current_output, Dict, [width(0)])),
    string_length(Json, Len),
    nb_getval(dap_channel, Out),
    format(Out, "Content-Length: ~d\r\n\r\n~w", [Len, Json]),
    flush_output(Out).

dap_read_message(Dict) :-
    dap_read_headers(Len),
    Len > 0,
    read_string(user_input, Len, Json),
    atom_json_dict(Json, Dict, []).

dap_read_headers(Len) :-
    read_line_to_string(user_input, Line),
    Line \== end_of_file,
    split_string(Line, ":", " \t\r\n", Parts),
    ( Parts = ["Content-Length", NumString]
      -> number_string(Len, NumString),
         read_line_to_string(user_input, _Blank)   % consume the blank separator line
      ; dap_read_headers(Len)
    ).

% --- Helpers -------------------------------------------------------------

dap_arg(Msg, Key, Value) :-
    get_dict(arguments, Msg, Args),
    get_dict(Key, Args, Value).

dap_to_string(Value, String) :-
    ( string(Value) -> String = Value
    ; atom(Value) -> atom_string(Value, String)
    ; number(Value) -> ( atom_number(A, Value), atom_string(A, String) )
    ; term_string(Value, String)
    ).
