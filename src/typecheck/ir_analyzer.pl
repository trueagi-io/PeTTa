:- module(ir_analyzer,
          [ analyze_ir/3,
            analyze_ir/4,
            analysis_result/2,
            analysis_card/2,
            analysis_state/2,
            analysis_effects/2,
            analysis_diagnostics/2,
            analysis_trace/2,
            analysis_result_has_fact/2,
            analysis_edge_state/4,
            analysis_node_card/3,
            analysis_node_state/3
          ]).

/** <module> Flow-sensitive interpretation of the relational checker IR

The analyzer is deliberately independent of PeTTa's attributed-variable
checker and dynamic declaration store.  Embedders supply the few pieces of
program knowledge which cannot be read from the IR itself through options:

  * `resolve_type(Closure)` calls Closure(Descriptor, Type).  It resolves
    descriptors such as `declared_arg(F, Arity, Index)`.
  * `constructor_signature(Closure)` calls
    Closure(Tag, Arity, ArgTypes, ResultType).
  * `resolve_call(Closure)` calls
    Closure(F, ArgIds, State, Resolution), where Resolution is
    `summary(Posts, Card, Effects)`, `data`, or `unknown`.

Builtin summaries are consulted before the external call resolver.  Unknown
applications stay `card(0,many)` with an `unknown_call/1` diagnostic; absence
of metadata is never interpreted as determinism.

An analysis is the closed record

    analysis(ResultId, Card, State, Effects, Diagnostics, Trace)

Trace entries expose the exact states on the true/false edges of tests and
the cardinality of each node.  Translation consumes them: a branch consumes
the same analysis which proved its cardinality and result facts instead of
running another source walker.
*/

:- use_module(abstract_domain).
:- use_module(call_summaries).
:- use_module(relational_ir).
:- use_module(library(lists)).


%!  analyze_ir(+IR, +State0, -Analysis) is det.

analyze_ir(IR, State0, Analysis) :-
    analyze_ir(IR, State0, [], Analysis).

%!  analyze_ir(+IR, +State0, +Options, -Analysis) is det.

analyze_ir(IR, State0, Options,
           analysis(Result, Card, State, Effects, Diagnostics, Trace)) :-
    close_state(State0, InitialState),
    ir_nodes(IR, Nodes),
    index_nodes(Nodes, Definitions),
    Ctx = context(Options, Definitions),
    ( consistent_flow(InitialState, reachable(ConsistentState))
      -> analyze_node(IR, ConsistentState, Ctx,
                      out(Reach, Result0, Card0, State1, Effects0, Diagnostics0, Trace0))
    ; Reach = no, ir_result(IR, Result0), card_zero(Card0),
      State1 = InitialState, Effects0 = [],
      Diagnostics0 = [inconsistent_initial_state], Trace0 = [] ),
    ( Reach == yes
      -> Result = Result0, Card = Card0, State = State1
    ; ir_result(IR, RootResult),
      Result = RootResult, card_zero(Card), State = InitialState ),
    sort(Effects0, Effects),
    sort(Diagnostics0, Diagnostics),
    reverse(Trace0, Trace), !.

analysis_result(analysis(Result, _, _, _, _, _), Result).
analysis_card(analysis(_, Card, _, _, _, _), Card).
analysis_state(analysis(_, _, State, _, _, _), State).
analysis_effects(analysis(_, _, _, Effects, _, _), Effects).
analysis_diagnostics(analysis(_, _, _, _, Diagnostics, _), Diagnostics).
analysis_trace(analysis(_, _, _, _, _, Trace), Trace).

analysis_result_has_fact(Analysis, Fact) :-
    analysis_result(Analysis, Result),
    analysis_state(Analysis, State),
    once(state_fact_matches(State, Result, Fact)).

analysis_edge_state(Analysis, TestId, Truth, State) :-
    analysis_trace(Analysis, Trace),
    member(edge(TestId, Truth, State), Trace), !.

analysis_node_state(Analysis, NodeId, State) :-
    analysis_trace(Analysis, Trace),
    member(node(NodeId, _, State), Trace), !.

analysis_node_card(Analysis, NodeId, Card) :-
    analysis_trace(Analysis, Trace),
    member(node(NodeId, Card, _), Trace), !.

% -- Node interpretation --------------------------------------------------

analyze_node(value(Id, Flavor), State, _,
             out(yes, Id, card(1,1), State, [], [],
                 [node(Id, card(1,1), State)])) :-
    ( Flavor == source_var ; Flavor = pattern_var(_) ), !.
analyze_node(value(Id, literal(Value)), State0, _, Out) :- !,
    literal_facts(Value, Facts),
    add_facts_flow(reachable(State0), Id, Facts, Flow),
    flow_out(Flow, Id, card(1,1), [], [], Out0),
    trace_node(Out0, Id, Out).

analyze_node(opaque(_, clause(_, _, BodyId),
                    [construct(_, head_patterns, Patterns), Body]),
             State0, Ctx, Out) :- !,
    assume_head_patterns(Patterns, State0, Ctx, HeadFlow, HeadDiagnostics),
    ( HeadFlow = reachable(HeadState)
      -> analyze_node(Body, HeadState, Ctx,
                      out(Reach, _, Card, State, Effects, Diagnostics0, Trace)),
         append(HeadDiagnostics, Diagnostics0, Diagnostics),
         Out = out(Reach, BodyId, Card, State, Effects, Diagnostics, Trace)
    ; card_zero(Zero),
      Out = out(no, BodyId, Zero, State0, [], HeadDiagnostics, []) ).

analyze_node(sequence(Nodes, Result), State0, Ctx, Out) :- !,
    analyze_sequence(Nodes, State0, Ctx,
                     out(Reach, _, Card, State, Effects,
                         Diagnostics, Trace)),
    Out0 = out(Reach, Result, Card, State, Effects,
               Diagnostics, Trace),
    trace_node(Out0, Result, Out).

analyze_node(branch(TestId, Then, Else, Result), State0, Ctx, Out) :- !,
    refine_test(TestId, true, State0, Ctx, TrueFlow),
    refine_test(TestId, false, State0, Ctx, FalseFlow),
    analyze_flow_node(TrueFlow, Then, Ctx, ThenOut0),
    analyze_flow_node(FalseFlow, Else, Ctx, ElseOut0),
    alias_out_result(ThenOut0, Result, ThenOut),
    alias_out_result(ElseOut0, Result, ElseOut),
    merge_exclusive_outs(ThenOut, ElseOut, Result, Out0),
    edge_trace(TestId, true, TrueFlow, TTrace),
    edge_trace(TestId, false, FalseFlow, FTrace),
    prepend_trace(TTrace, Out0, Out1),
    prepend_trace(FTrace, Out1, Out2),
    trace_node(Out2, Result, Out).

analyze_node(try_match(ValueId, Pattern, Then, Else, Result),
             State0, Ctx, Out) :- !,
    match_edges(ValueId, Pattern, State0, Ctx, SuccessFlow, FailureFlow),
    analyze_flow_node(SuccessFlow, Then, Ctx, ThenOut0),
    analyze_flow_node(FailureFlow, Else, Ctx, ElseOut0),
    alias_out_result(ThenOut0, Result, ThenOut),
    alias_out_result(ElseOut0, Result, ElseOut),
    merge_exclusive_outs(ThenOut, ElseOut, Result, Out0),
    trace_node(Out0, Result, Out).

analyze_node(call(Id, F, Args), State0, Ctx, Out) :- !,
    call_owned_children(F, Args, OwnedChildren),
    analyze_sequence(OwnedChildren, State0, Ctx, ArgsOut),
    analyze_call_after_args(Id, F, Args, ArgsOut, Ctx, Out0),
    trace_node(Out0, Id, Out).

analyze_node(reify(Id, _), State0, _, Out) :- !,
    add_facts_flow(reachable(State0), Id,
                   [type('Bool'), proper_bool, ground, nonvar], Flow),
    flow_out(Flow, Id, card(1,1), [pure], [], Out0),
    trace_node(Out0, Id, Out).

analyze_node(construct(Id, Kind, Children), State0, Ctx, Out) :- !,
    analyze_sequence(Children, State0, Ctx, ChildrenOut),
    construct_after_children(Id, Kind, Children, ChildrenOut, Ctx, Out0),
    trace_node(Out0, Id, Out).

analyze_node(once(Expr, Result), State0, Ctx, Out) :- !,
    analyze_node(Expr, State0, Ctx,
                 out(Reach, InnerResult, InnerCard, State0a, Effects,
                     Diagnostics, Trace)),
    card_once(InnerCard, Card),
    ( Reach == yes
      -> alias_one_way(State0a, InnerResult, Result, State)
    ; State = State0a ),
    Out0 = out(Reach, Result, Card, State, Effects, Diagnostics,
               Trace),
    trace_node(Out0, Result, Out).

analyze_node(collect(Expr, Result), State0, Ctx, Out) :- !,
    analyze_node(Expr, State0, Ctx,
                 out(_, _, _, _, Effects, Diagnostics, Trace)),
    % Collection contains copies of inner bindings; they do not refine the
    % surrounding variables.  Only effects and the constructed list escape.
    add_facts_flow(reachable(State0), Result,
                   [expr, proper_list, nonvar], reachable(State)),
    Out0 = out(yes, Result, card(1,1), State, Effects,
               Diagnostics, Trace),
    trace_node(Out0, Result, Out).

analyze_node(opaque(Id, no_match, []), State, _,
             out(no, Id, card(0,0), State, [], [],
                 [node(Id, card(0,0), State)])) :- !.
analyze_node(opaque(Id, no_else, []), State, _,
             out(no, Id, card(0,0), State, [], [],
                 [node(Id, card(0,0), State)])) :- !.
analyze_node(opaque(Id, quote(Payload), []), State0, _, Out) :- !,
    literal_facts(Payload, Facts),
    add_facts_flow(reachable(State0), Id, Facts, Flow),
    flow_out(Flow, Id, card(1,1), [pure], [], Out0),
    trace_node(Out0, Id, Out).
analyze_node(opaque(Id, Tag, _Children), State0, _Ctx, Out) :-
    % An unsupported form's children are syntax, not an evaluation-order
    % promise (a lambda body below |-> must not refine the enclosing
    % sequence), so the input state passes through unchanged.
    Out0 = out(yes, Id, card(0,many), State0, [opaque],
               [unsupported(Tag)], []),
    trace_node(Out0, Id, Out).


analyze_sequence([], State, _,
                 out(yes, none, card(1,1), State, [], [], [])) :- !.
analyze_sequence([Node|Nodes], State0, Ctx, Out) :-
    analyze_node(Node, State0, Ctx, First),
    sequence_tail(First, Nodes, Ctx, Out).

call_owned_children(F, Args, [F|Args]) :-
    nonvar(F), ir_result(F, _), !.
call_owned_children(_, Args, Args).

sequence_tail(out(no, Result, Card, State, Effects,
                  Diagnostics, Trace), _, _,
              out(no, Result, Card, State, Effects,
                  Diagnostics, Trace)) :- !.
sequence_tail(First, [], _, First) :- !.
sequence_tail(out(yes, _, CardA, StateA, EffectsA,
                  DiagnosticsA, TraceA), Nodes, Ctx,
              out(Reach, Result, Card, State, Effects,
                  Diagnostics, Trace)) :-
    analyze_sequence(Nodes, StateA, Ctx,
                     out(Reach, Result, CardB, State, EffectsB, DiagnosticsB, TraceB)),
    card_seq(CardA, CardB, Card),
    append(EffectsA, EffectsB, Effects),
    append(DiagnosticsA, DiagnosticsB, Diagnostics),
    append(TraceB, TraceA, Trace).


% -- Calls ---------------------------------------------------------------

analyze_call_after_args(Id, _, _,
                        out(no, _, Card, State, Effects,
                            Diagnostics, Trace), _,
                        out(no, Id, Card, State, Effects,
                            Diagnostics, Trace)) :- !.
analyze_call_after_args(Id, F, Args,
                        out(yes, _, ArgsCard, State0, Effects0, Diagnostics0, Trace), Ctx, Out) :-
    maplist(ir_result, Args, ArgIds),
    resolve_application(F, ArgIds, State0, Ctx, Resolution),
    apply_call_resolution(Resolution, Id, F, ArgIds, State0,
                          CallCard, State, CallEffects, CallDiagnostics),
    card_seq(ArgsCard, CallCard, Card),
    append(Effects0, CallEffects, Effects),
    append(Diagnostics0, CallDiagnostics, Diagnostics),
    ( Card = card(0,0) -> Reach = no ; Reach = yes ),
    Out = out(Reach, Id, Card, State, Effects, Diagnostics, Trace).

resolve_application(F, ArgIds, State, _, summary(Posts, Card, Effects)) :-
    atom(F), length(ArgIds, N),
    select_builtin_mode(F, N, has_argument_fact(State, ArgIds),
                        Posts, Card, Effects), !.
resolve_application(F, ArgIds, State, context(Options, _), Resolution) :-
    option_closure(resolve_call, Options, Resolver),
    catch(call(Resolver, F, ArgIds, State, Resolution), _, fail), !.
resolve_application(_, _, _, _, unknown).

has_argument_fact(State, ArgIds, Index, Fact) :-
    nth0(Index, ArgIds, Id),
    state_fact_matches(State, Id, Fact).

apply_call_resolution(summary(Posts, Card, Effects), Id, _, ArgIds, State0,
                      Card, State, Effects, []) :-
    valid_call_summary(Posts, ArgIds, Card, Effects),
    catch(apply_posts(Posts, Id, ArgIds, State0, Candidate), _, fail),
    consistent_flow(Candidate, reachable(State)), !.
apply_call_resolution(Resolution, _, F, ArgIds, State,
                      card(0,many), State, [opaque],
                      [invalid_call_summary(F/N, Resolution)]) :-
    Resolution = summary(_, _, _), !,
    length(ArgIds, N).
apply_call_resolution(data, Id, F, ArgIds, State0,
                      card(1,1), State, [pure], []) :- !,
    data_application_facts(F, ArgIds, State0, Facts),
    add_facts_flow(reachable(State0), Id, Facts, reachable(State)).
apply_call_resolution(unknown, _, F, ArgIds, State,
                      card(0,many), State, [opaque], [unknown_call(F/N)]) :- !,
    length(ArgIds, N).
apply_call_resolution(Resolution, _, F, ArgIds, State,
                      card(0,many), State, [opaque],
                      [invalid_call_summary(F/N, Resolution)]) :-
    length(ArgIds, N).

valid_call_summary(Posts, ArgIds, Card, Effects) :-
    length(ArgIds, Arity),
    is_list(Posts), maplist(valid_call_post(Arity), Posts),
    once(card_level(Card, _)),
    is_list(Effects).

valid_call_post(_, ensure(result, Fact)) :- nonvar(Fact).
valid_call_post(Arity, ensure(arg(Index), Fact)) :-
    integer(Index), Index >= 0, Index < Arity, nonvar(Fact).

apply_posts([], _, _, State, State).
apply_posts([ensure(Target, Fact)|Posts], Result, ArgIds, State0, State) :-
    post_target_id(Target, Result, ArgIds, Id),
    add_facts_flow(reachable(State0), Id, [Fact], reachable(State1)),
    apply_posts(Posts, Result, ArgIds, State1, State).

post_target_id(result, Result, _, Result).
post_target_id(arg(Index), _, ArgIds, Id) :- nth0(Index, ArgIds, Id).

data_application_facts(F, ArgIds, State, Facts) :-
    Base = [expr, proper_list, nonempty_list, nonvar],
    ( maplist(id_literal(State), ArgIds, Values)
      -> Full = [F|Values],
         literal_list_derived_facts(Full, LiteralFacts),
         append(Base, LiteralFacts, Facts)
    ; all_ids_have(State, ArgIds, ground)
      -> Facts = [ground|Base]
    ; Facts = Base ).


% -- Constructors and patterns ------------------------------------------

construct_after_children(Id, Kind, _Children,
                         out(no, _, Card, State, Effects,
                             Diagnostics, Trace), _,
                         out(no, Id, Card, State, Effects,
                             Diagnostics, Trace)) :- !,
    Kind = Kind.
construct_after_children(Id, Kind, Children,
                         out(yes, _, ChildrenCard, State0, Effects, Diagnostics, Trace), Ctx, Out) :-
    maplist(ir_result, Children, ChildIds),
    constructor_facts(Kind, ChildIds, State0, Ctx, Facts),
    add_facts_flow(reachable(State0), Id, Facts, Flow),
    ( Flow = reachable(State)
      -> Out = out(yes, Id, ChildrenCard, State, Effects,
                   Diagnostics, Trace)
    ; Out = out(no, Id, card(0,0), State0, Effects,
                Diagnostics, Trace) ).

constructor_facts(data, ChildIds, State, _, Facts) :- !,
    list_constructor_facts([], ChildIds, State, Facts).
constructor_facts(list, ChildIds, State, _, Facts) :- !,
    list_constructor_facts([], ChildIds, State, Facts).
constructor_facts(cons, [_, Tail], State, _, Facts) :- !,
    ( state_fact_matches(State, Tail, proper_list)
      -> Facts = [expr, proper_list, nonempty_list, nonvar]
    ; Facts = [nonvar] ).
constructor_facts(pattern_cons, _, _, _,
                  [expr, proper_list, nonempty_list, nonvar]) :- !.
constructor_facts(positional_pattern, ChildIds, _, _, Facts) :- !,
    length(ChildIds, Length),
    Facts = [proper_list_length(Length), expr, proper_list, nonvar].
constructor_facts(pattern(Tag), ChildIds, State, _, Facts) :- !,
    list_constructor_facts([Tag], ChildIds, State, Facts).
constructor_facts(typed_pattern(_), _, _, _, [nonvar]) :- !.
constructor_facts(_, _, _, _, [nonvar]).

list_constructor_facts(Prefix, ChildIds, State, Facts) :-
    length(Prefix, PrefixLength),
    length(ChildIds, ChildLength),
    Length is PrefixLength + ChildLength,
    append(Prefix, Values, Full),
    ( maplist(id_literal(State), ChildIds, Values)
      -> literal_list_derived_facts(Full, Derived),
         append([expr, proper_list, nonvar], Derived, Facts0)
    ; all_ids_have(State, ChildIds, ground)
      -> Facts0 = [expr, proper_list, ground, nonvar]
    ; Facts0 = [expr, proper_list, nonvar] ),
    ( Full = [_|_] -> ShapeFacts = [nonempty_list|Facts0]
    ; ChildIds = [_|_] -> ShapeFacts = [nonempty_list|Facts0]
    ; ShapeFacts = Facts0 ),
    Facts = [proper_list_length(Length)|ShapeFacts].

assume_head_patterns([], State, _, reachable(State), []).
assume_head_patterns([Pattern|Patterns], State0, Ctx, Flow, Diagnostics) :-
    assume_head_pattern(Pattern, State0, Ctx, FirstFlow, FirstDiagnostics),
    ( FirstFlow = reachable(State1)
      -> assume_head_patterns(Patterns, State1, Ctx, Flow, RestDiagnostics),
         append(FirstDiagnostics, RestDiagnostics, Diagnostics)
    ; Flow = unreachable,
      Diagnostics = FirstDiagnostics ).

assume_head_pattern(construct(ValueId, typed_pattern(TypeRef), [Pattern]),
                    State0, Ctx, Flow, Diagnostics) :- !,
    ( resolve_type_ref(TypeRef, Ctx, Type)
      -> add_facts_flow(reachable(State0), ValueId, [type(Type)], Typed),
         flow_pattern_success(Typed, ValueId, Pattern, Ctx, Flow),
         Diagnostics = []
    ; Flow = reachable(State0),
      Diagnostics = [unresolved_type(TypeRef)] ).
assume_head_pattern(Pattern, State0, Ctx, Flow, []) :-
    ir_result(Pattern, ValueId),
    pattern_success(ValueId, Pattern, State0, Ctx, Flow).

resolve_type_ref(TypeRef, context(Options, _), Type) :-
    TypeRef = declared_arg(_, _, _), !,
    option_closure(resolve_type, Options, Resolver),
    call(Resolver, TypeRef, Type).
resolve_type_ref(Type, _, Type).

match_edges(ValueId, Pattern, State0, Ctx, Success, Failure) :-
    pattern_reachability(ValueId, Pattern, State0, CanSucceed, CanFail),
    ( CanSucceed == yes
      -> pattern_success(ValueId, Pattern, State0, Ctx, Success)
    ; Success = unreachable ),
    ( CanFail == yes
      -> pattern_failure(ValueId, Pattern, State0, Failure)
    ; Failure = unreachable ).

pattern_reachability(_, value(_, pattern_var(fresh)), _, yes, no) :- !.
pattern_reachability(ValueId, value(PatternId, pattern_var(existing)), _,
                     yes, CanFail) :- !,
    ( ValueId == PatternId -> CanFail = no ; CanFail = yes ).
pattern_reachability(ValueId, value(_, literal(Value)), State,
                     CanSucceed, CanFail) :- !,
    literal_match_reachability(ValueId, Value, State, CanSucceed, CanFail).
pattern_reachability(ValueId, construct(_, typed_pattern(_), [Pattern]), State,
                     CanSucceed, yes) :- !,
    % A type annotation is a runtime restriction unless compatibility with the
    % scrutinee has been proved.  The inner wildcard alone is not exhaustive.
    pattern_reachability(ValueId, Pattern, State, CanSucceed, _).
pattern_reachability(ValueId,
                     construct(_, positional_pattern, Children), State,
                     CanSucceed, CanFail) :- !,
    length(Children, Length),
    ( state_fact_matches(State, ValueId, proper_list_length(KnownLength)),
      integer(KnownLength)
      -> ( KnownLength =:= Length
           -> CanSucceed = yes,
              ( distinct_variable_patterns(Children)
                -> CanFail = no
              ; CanFail = yes )
         ; CanSucceed = no, CanFail = yes )
    ; CanSucceed = yes, CanFail = yes ).
pattern_reachability(_, _, _, yes, yes).

distinct_variable_patterns(Children) :-
    maplist(variable_pattern_id, Children, Ids),
    pairwise_distinct_ids(Ids).

variable_pattern_id(value(Id, pattern_var(fresh)), Id).

pairwise_distinct_ids([]).
pairwise_distinct_ids([Id|Ids]) :-
    \+ ( member(Other, Ids), Other == Id ),
    pairwise_distinct_ids(Ids).

literal_match_reachability(ValueId, Value, State, CanSucceed, CanFail) :-
    ( id_literal(State, ValueId, Known)
      -> ( Known =@= Value
           -> CanSucceed = yes, CanFail = no
         ; CanSucceed = no, CanFail = yes )
    ; state_fact_matches(State, ValueId, excluded_literal(Value))
      -> CanSucceed = no, CanFail = yes
    ; state_domain_excludes(State, ValueId, Value)
      -> CanSucceed = no, CanFail = yes
    ; CanSucceed = yes,
      ( domain_exhausted_after(State, ValueId, [Value])
        -> CanFail = no
      ; CanFail = yes ) ).

pattern_success(ValueId, value(PatternId, source_var), State0, _, Flow) :- !,
    alias_ids(State0, ValueId, PatternId, State),
    consistent_flow(State, Flow).
pattern_success(ValueId, value(PatternId, pattern_var(_)), State0, _, Flow) :- !,
    alias_ids(State0, ValueId, PatternId, State),
    consistent_flow(State, Flow).
pattern_success(ValueId, value(_, literal(Value)), State0, _, Flow) :- !,
    literal_facts(Value, Facts),
    add_facts_flow(reachable(State0), ValueId, Facts, Flow).
pattern_success(ValueId,
                construct(_, typed_pattern(TypeRef), [Pattern]),
                State0, Ctx, Flow) :- !,
    ( resolve_type_ref(TypeRef, Ctx, Type)
      -> add_facts_flow(reachable(State0), ValueId, [type(Type)], Typed),
         flow_pattern_success(Typed, ValueId, Pattern, Ctx, Flow)
    ; pattern_success(ValueId, Pattern, State0, Ctx, Flow) ).
pattern_success(ValueId, construct(_, pattern_cons, [Head, Tail]),
                State0, Ctx, Flow) :- !,
    ( state_fact_matches(State0, ValueId, proper_list)
      -> ValueFacts = [expr, proper_list, nonempty_list, nonvar],
         TailFacts = [proper_list]
    ; ValueFacts = [nonvar], TailFacts = [] ),
    add_facts_flow(reachable(State0), ValueId, ValueFacts, Shaped),
    ( Shaped = reachable(State1)
      -> ir_result(Head, HeadId), ir_result(Tail, TailId),
         add_facts_flow(reachable(State1), TailId, TailFacts, TailFlow),
         flow_pattern_success(TailFlow, TailId, Tail, Ctx, TailMatched),
         flow_pattern_success(TailMatched, HeadId, Head, Ctx, Flow)
    ; Flow = unreachable ).
pattern_success(ValueId, construct(_, positional_pattern, Children),
                State0, Ctx, Flow) :- !,
    length(Children, Length),
    add_facts_flow(reachable(State0), ValueId,
                   [proper_list_length(Length)], Shaped),
    flow_patterns_self(Shaped, Children, Ctx, Flow).
pattern_success(ValueId, construct(_, pattern(Tag), Children),
                State0, Ctx, Flow) :- !,
    add_facts_flow(reachable(State0), ValueId,
                   [expr, proper_list, nonempty_list, nonvar], ShapeFlow),
    apply_constructor_signature(Tag, Children, ValueId, ShapeFlow, Ctx,
                                TypedFlow),
    flow_patterns_self(TypedFlow, Children, Ctx, Flow).
pattern_success(_, _, State, _, reachable(State)).

flow_pattern_success(unreachable, _, _, _, unreachable) :- !.
flow_pattern_success(reachable(State), ValueId, Pattern, Ctx, Flow) :-
    pattern_success(ValueId, Pattern, State, Ctx, Flow).

flow_patterns_self(unreachable, _, _, unreachable) :- !.
flow_patterns_self(reachable(State), [], _, reachable(State)).
flow_patterns_self(reachable(State0), [Pattern|Patterns], Ctx, Flow) :-
    ir_result(Pattern, Id),
    pattern_success(Id, Pattern, State0, Ctx, First),
    flow_patterns_self(First, Patterns, Ctx, Flow).

apply_constructor_signature(Tag, Children, ValueId, Flow0,
                            context(Options, _), Flow) :-
    length(Children, Arity),
    ( option_closure(constructor_signature, Options, Resolver),
      catch(call(Resolver, Tag, Arity, ArgTypes, ResultType), _, fail),
      same_length(Children, ArgTypes)
      -> add_flow_fact(Flow0, ValueId, type(ResultType), Flow1),
         add_child_types(Children, ArgTypes, Flow1, Flow)
    ; Flow = Flow0 ).

add_child_types([], [], Flow, Flow).
add_child_types([Child|Children], [Type|Types], Flow0, Flow) :-
    ir_result(Child, Id),
    add_flow_fact(Flow0, Id, type(Type), Flow1),
    add_child_types(Children, Types, Flow1, Flow).

pattern_failure(ValueId, value(_, literal(Value)), State0, Flow) :- !,
    add_facts_flow(reachable(State0), ValueId,
                   [excluded_literal(Value)], Flow).
pattern_failure(ValueId, construct(_, typed_pattern(_), [Pattern]),
                State0, Flow) :- !,
    pattern_failure(ValueId, Pattern, State0, Flow).
pattern_failure(_, _, State, reachable(State)).


% -- Result-edge refinement ---------------------------------------------

refine_test(TestId, Truth, State0, Ctx, Flow) :-
    Ctx = context(_, Definitions),
    ( definition_for_id(Definitions, TestId, Node)
      -> refine_node_result(Node, Truth, State0, Ctx, Flow)
    ; add_facts_flow(reachable(State0), TestId,
                     [literal(Truth)], Flow) ).

refine_node_result(value(_, literal(Value)), Truth, State, _, Flow) :- !,
    ( Value == Truth -> Flow = reachable(State) ; Flow = unreachable ).
refine_node_result(reify(_, Relation), Truth, State0, Ctx, Flow) :- !,
    refine_relation(Relation, Truth, State0, Ctx, Flow).
refine_node_result(call(_, F, Args), Truth, State0, Ctx, Flow) :- !,
    maplist(ir_result, Args, ArgIds),
    refine_call(F, ArgIds, Truth, State0, Ctx, Flow).
refine_node_result(Node, Truth, State0, _, Flow) :-
    ir_result(Node, Id),
    add_facts_flow(reachable(State0), Id, [literal(Truth)], Flow).

refine_relation(Relation, true, State0, Ctx, Flow) :-
    compound(Relation), compound_name_arguments(Relation, Name, [A, B]),
    ( Name == unify
      -> refine_unify_success(A, B, State0, Ctx, Flow)
    ; Name == identical
      -> refine_identical_success(A, B, State0, Flow)
    ; Flow = reachable(State0) ), !.
refine_relation(Relation, false, State0, _, Flow) :-
    compound(Relation), compound_name_arguments(Relation, Name, [A, B]),
    ( Name == unify ; Name == identical ; Name == unifiable ), !,
    refine_disequality(A, B, State0, Flow).
refine_relation(_, _, State, _, reachable(State)).

refine_unify_success(A, B, State0, context(Options, Definitions), Flow) :-
    Ctx = context(Options, Definitions),
    ( definition_for_id(Definitions, A, ANode), structural_pattern_node(ANode)
      -> pattern_success(B, ANode, State0, Ctx, Flow)
    ; definition_for_id(Definitions, B, BNode), structural_pattern_node(BNode)
      -> pattern_success(A, BNode, State0, Ctx, Flow)
    ; definition_for_id(Definitions, A, ANode), pattern_node(ANode)
      -> pattern_success(B, ANode, State0, Ctx, Flow)
    ; definition_for_id(Definitions, B, BNode), pattern_node(BNode)
      -> pattern_success(A, BNode, State0, Ctx, Flow)
    ; alias_ids(State0, A, B, State), consistent_flow(State, Flow) ).

structural_pattern_node(construct(_, pattern(_), _)).
structural_pattern_node(construct(_, pattern_cons, _)).
structural_pattern_node(construct(_, positional_pattern, _)).
structural_pattern_node(construct(_, typed_pattern(_), _)).

pattern_node(construct(_, pattern(_), _)).
pattern_node(construct(_, pattern_cons, _)).
pattern_node(construct(_, positional_pattern, _)).
pattern_node(construct(_, typed_pattern(_), _)).
pattern_node(value(_, pattern_var(_))).
pattern_node(value(_, source_var)).
pattern_node(value(_, literal(_))).

refine_identical_success(A, B, State0, Flow) :-
    ( id_literal(State0, A, Value)
      -> literal_facts(Value, Facts),
         add_facts_flow(reachable(State0), B, Facts, Flow)
    ; id_literal(State0, B, Value)
      -> literal_facts(Value, Facts),
         add_facts_flow(reachable(State0), A, Facts, Flow)
    ; alias_ids_without_types(State0, A, B, State),
      consistent_flow(State, Flow) ).

refine_disequality(A, B, State0, Flow) :-
    ( id_literal(State0, A, Value)
      -> add_facts_flow(reachable(State0), B,
                        [excluded_literal(Value)], Flow)
    ; id_literal(State0, B, Value)
      -> add_facts_flow(reachable(State0), A,
                        [excluded_literal(Value)], Flow)
    ; Flow = reachable(State0) ).

refine_call(not, [Arg], Truth, State0, Ctx, Flow) :- !,
    opposite_bool(Truth, Inner),
    refine_test(Arg, Inner, State0, Ctx, Flow).
refine_call(and, Args, true, State0, Ctx, Flow) :- !,
    refine_all(Args, true, State0, Ctx, Flow).
refine_call(or, Args, false, State0, Ctx, Flow) :- !,
    refine_all(Args, false, State0, Ctx, Flow).
refine_call('is-expr', [Arg], true, State0, _, Flow) :- !,
    add_facts_flow(reachable(State0), Arg,
                   [expr, proper_list, nonvar], Flow).
refine_call('is-var', [Arg], true, State0, _, Flow) :- !,
    add_facts_flow(reachable(State0), Arg, [variable], Flow).
refine_call('is-var', [Arg], false, State0, _, Flow) :- !,
    add_facts_flow(reachable(State0), Arg, [nonvar], Flow).
refine_call('is-ground', [Arg], true, State0, _, Flow) :- !,
    add_facts_flow(reachable(State0), Arg, [ground, nonvar], Flow).
refine_call(_, _, _, State, _, reachable(State)).

refine_all([], _, State, _, reachable(State)).
refine_all([Id|Ids], Truth, State0, Ctx, Flow) :-
    refine_test(Id, Truth, State0, Ctx, First),
    ( First = reachable(State1)
      -> refine_all(Ids, Truth, State1, Ctx, Flow)
    ; Flow = unreachable ).

opposite_bool(true, false).
opposite_bool(false, true).


% -- Flow/result combination --------------------------------------------

analyze_flow_node(unreachable, Node, _,
                  out(no, Result, card(0,0), unreachable, [], [], [])) :-
    ir_result(Node, Result), !.
analyze_flow_node(reachable(State), Node, Ctx, Out) :-
    analyze_node(Node, State, Ctx, Out).

alias_out_result(out(no, _, Card, State, Effects,
                     Diagnostics, Trace), Result,
                 out(no, Result, Card, State, Effects,
                     Diagnostics, Trace)) :- !.
alias_out_result(out(yes, From, Card, State0, Effects,
                     Diagnostics, Trace), Result,
                 out(yes, Result, Card, State, Effects,
                     Diagnostics, Trace)) :-
    alias_one_way(State0, From, Result, State).

merge_exclusive_outs(out(no, _, _, _, EffectsA,
                         DiagnosticsA, TraceA),
                     out(no, _, _, _, EffectsB,
                         DiagnosticsB, TraceB), Result,
                     out(no, Result, card(0,0), state([]), Effects, Diagnostics, Trace)) :- !,
    append(EffectsA, EffectsB, Effects),
    append(DiagnosticsA, DiagnosticsB, Diagnostics),
    append(TraceB, TraceA, Trace).
merge_exclusive_outs(out(no, _, _, unreachable, EffectsA,
                         DiagnosticsA, TraceA),
                     Right, Result, Out) :- !,
    merge_unreachable_metadata(Right, Result, EffectsA, DiagnosticsA, TraceA,
                               Out).
merge_exclusive_outs(Left,
                     out(no, _, _, unreachable, EffectsB,
                         DiagnosticsB, TraceB), Result, Out) :- !,
    merge_unreachable_metadata(Left, Result, EffectsB, DiagnosticsB, TraceB,
                               Out).
merge_exclusive_outs(out(yes, _, Card, State, EffectsA,
                         DiagnosticsA, TraceA),
                     out(no, _, _, _, EffectsB,
                         DiagnosticsB, TraceB), Result,
                     out(yes, Result, JoinedCard, State, Effects,
                         Diagnostics, Trace)) :- !,
    card_join(Card, card(0,0), JoinedCard),
    append(EffectsA, EffectsB, Effects),
    append(DiagnosticsA, DiagnosticsB, Diagnostics),
    append(TraceB, TraceA, Trace).
merge_exclusive_outs(Left, Right, Result, Out) :-
    Left = out(no, _, _, _, _, _, _), !,
    merge_exclusive_outs(Right, Left, Result, Out).
merge_exclusive_outs(out(yes, _, CardA, StateA, EffectsA,
                         DiagnosticsA, TraceA),
                     out(yes, _, CardB, StateB, EffectsB,
                         DiagnosticsB, TraceB), Result,
                     out(yes, Result, Card, State, Effects,
                         Diagnostics, Trace)) :-
    card_join(CardA, CardB, Card),
    state_join(StateA, StateB, State),
    append(EffectsA, EffectsB, Effects),
    append(DiagnosticsA, DiagnosticsB, Diagnostics),
    append(TraceB, TraceA, Trace).

merge_unreachable_metadata(
    out(Reach, _, Card, State, Effects0, Diagnostics0, Trace0),
    Result, Effects1, Diagnostics1, Trace1,
    out(Reach, Result, Card, State, Effects, Diagnostics, Trace)) :-
    append(Effects0, Effects1, Effects),
    append(Diagnostics0, Diagnostics1, Diagnostics),
    append(Trace1, Trace0, Trace).

flow_out(unreachable, Result, _, Effects, Diagnostics,
         out(no, Result, card(0,0), state([]), Effects,
             Diagnostics, [])) :- !.
flow_out(reachable(State), Result, Card, Effects, Diagnostics,
         out(yes, Result, Card, State, Effects,
             Diagnostics, [])).

trace_node(out(Reach, Result, Card, State, Effects,
               Diagnostics, Trace0), Id,
           out(Reach, Result, Card, State, Effects,
               Diagnostics, [node(Id, Card, State)|Trace0])).

prepend_trace([], Out, Out).
prepend_trace([Entry|Entries],
              out(R, I, C, S, E, D, Trace), Out) :-
    prepend_trace(Entries, out(R, I, C, S, E, D, [Entry|Trace]), Out).

edge_trace(_, _, unreachable, []).
edge_trace(Id, Truth, reachable(State), [edge(Id, Truth, State)]).

% -- Fact closure and consistency ---------------------------------------

close_state(state(Entries), State) :-
    state_empty(Empty),
    close_entries(Entries, Empty, State).

close_entries([], State, State).
close_entries([entry(Id, Facts)|Entries], State0, State) :-
    expand_facts(Facts, Expanded),
    add_state_facts(Expanded, Id, State0, State1),
    close_entries(Entries, State1, State).

add_flow_fact(unreachable, _, _, unreachable) :- !.
add_flow_fact(reachable(State0), Id, Fact, Flow) :-
    add_facts_flow(reachable(State0), Id, [Fact], Flow).

add_facts_flow(unreachable, _, _, unreachable) :- !.
add_facts_flow(reachable(State0), Id, Facts, Flow) :-
    expand_facts(Facts, Expanded),
    add_state_facts(Expanded, Id, State0, State),
    consistent_value_flow(State, Id, Flow).

add_state_facts([], _, State, State).
add_state_facts(Facts, Id, State0, State) :-
    state_add_facts(State0, Id, Facts, State1),
    close_contextual_value_facts(Facts, Id, State1, State).

% Unary implications such as nonempty_list -> proper_list live in
% fact_implications/2.  The converse needs two facts: a proper list which is
% known not to be [] must have at least one cell.  Run this closure whenever
% either premise is newly added so it is independent of refinement order and
% also applies to facts copied through aliases.
close_contextual_value_facts(Added, Id, State0, State) :-
    ( may_complete_nonempty_list(Added),
      state_has_fact(State0, Id, proper_list),
      state_has_fact(State0, Id, excluded_literal([]))
      -> state_add_fact(State0, Id, nonempty_list, State)
    ; State = State0 ).

may_complete_nonempty_list(Facts) :- memberchk(proper_list, Facts), !.
may_complete_nonempty_list(Facts) :- memberchk(excluded_literal([]), Facts).

expand_facts(Facts, Expanded) :-
    expand_facts_(Facts, [], Expanded0),
    variant_dedup(Expanded0, Expanded).

expand_facts_([], Tail, Tail).
expand_facts_([Fact|Facts], Acc0, Acc) :-
    once(fact_implications(Fact, Implied)),
    append([Fact|Implied], Acc0, Acc1),
    expand_facts_(Facts, Acc1, Acc).

fact_implications(proper_bool,
                  [type('Bool'), domain([true,false]), ground, nonvar]).
fact_implications(type(Type), [domain([true,false])]) :-
    nonvar(Type), Type == 'Bool', !.
fact_implications(number, [type('Number'), ground, nonvar]).
fact_implications(proper_list, [expr, nonvar]).
fact_implications(nonempty_list, [proper_list, expr, nonvar]).
fact_implications(proper_list_length(0),
                  [literal([]), literal_list([]), proper_list, duplicate_free,
                   expr, ground, nonvar]) :- !.
fact_implications(proper_list_length(Length),
                  [nonempty_list, proper_list, expr, nonvar]) :-
    integer(Length), Length > 0, !.
fact_implications(ground, [nonvar]).
fact_implications(literal(true),
                  [type('Bool'), proper_bool, domain([true,false]),
                   ground, nonvar]).
fact_implications(literal(false),
                  [type('Bool'), proper_bool, domain([true,false]),
                   ground, nonvar]).
fact_implications(literal(Value), [number, type('Number'), ground, nonvar]) :-
    number(Value), !.
fact_implications(literal(Value), [type('String'), ground, nonvar]) :-
    string(Value), !.
fact_implications(literal([]),
                  [literal_list([]), proper_list_length(0), duplicate_free,
                   expr, proper_list, ground, nonvar]).
fact_implications(literal(Value), [ground, nonvar]) :- atomic(Value), !.
fact_implications(literal(Value), Facts) :-
    is_list(Value), !,
    list_literal_implications(Value, literal_list(Value), Facts).
fact_implications(literal_list(Value), Facts) :-
    is_list(Value),
    list_literal_implications(Value, literal(Value), Facts).
fact_implications(_, []).

list_literal_implications(Value, Peer, Facts) :-
    length(Value, Length),
    Base0 = [Peer, proper_list_length(Length), expr, proper_list, nonvar],
    ( ground(Value) -> Base = [ground|Base0] ; Base = Base0 ),
    ( Value == [] -> Nonempty = [] ; Nonempty = [nonempty_list] ),
    ( duplicate_free_list(Value) -> Unique = [duplicate_free] ; Unique = [] ),
    append([Nonempty, Unique, Base], Facts).

literal_facts(Value, Facts) :-
    ( is_list(Value)
      -> literal_list_derived_facts(Value, Derived),
         Facts = [literal(Value)|Derived]
    ; Facts = [literal(Value)] ).

literal_list_derived_facts(Values, Facts) :-
    length(Values, Length),
    Base0 = [proper_list_length(Length), literal_list(Values), literal(Values),
             expr, proper_list, nonvar],
    ( ground(Values) -> Base = [ground|Base0] ; Base = Base0 ),
    ( Values == [] -> Nonempty = [] ; Nonempty = [nonempty_list] ),
    ( duplicate_free_list(Values) -> Unique = [duplicate_free] ; Unique = [] ),
    append([Nonempty, Unique, Base], Facts).

duplicate_free_list(Values) :-
    ground(Values), sort(Values, Unique), same_length(Values, Unique).

consistent_flow(State, Flow) :-
    ( inconsistent_state(State) -> Flow = unreachable
    ; Flow = reachable(State) ).

% The input state is consistent and only one value entry changed, so only that
% entry can become contradictory; whole-state checks happen at input, joins
% and alias operations.
consistent_value_flow(State, Id, Flow) :-
    ( inconsistent_value(State, Id) -> Flow = unreachable
    ; Flow = reachable(State) ).

inconsistent_state(State) :-
    state_value_id(State, Id),
    inconsistent_value(State, Id), !.

inconsistent_value(State, Id) :-
    state_fact_matches(State, Id, literal(Value)),
    state_fact_matches(State, Id, excluded_literal(Value)), !.
inconsistent_value(State, Id) :-
    findall(Length,
            ( state_fact_matches(State, Id, proper_list_length(Length)),
              integer(Length) ),
            Lengths0),
    sort(Lengths0, Lengths),
    Lengths = [_,_|_], !.
inconsistent_value(State, Id) :-
    findall(Value, state_fact_matches(State, Id, literal(Value)), Values0),
    variant_dedup(Values0, Values),
    Values = [_,_|_], !.
inconsistent_value(State, Id) :-
    state_fact_matches(State, Id, domain(Domain)),
    findall(V, state_fact_matches(State, Id, excluded_literal(V)), Excluded),
    domain_all_excluded(Domain, Excluded), !.
inconsistent_value(State, Id) :-
    state_fact_matches(State, Id, variable),
    state_fact_matches(State, Id, nonvar), !.
inconsistent_value(State, Id) :-
    state_fact_matches(State, Id, literal(Value)),
    state_fact_matches(State, Id, domain(Domain)),
    ground(Value), ground(Domain),
    \+ variant_member(Value, Domain), !.

state_value_id(state(Entries), Id) :- member(entry(Id, _), Entries).

domain_all_excluded([], _).
domain_all_excluded([Value|Values], Excluded) :-
    variant_member(Value, Excluded),
    domain_all_excluded(Values, Excluded).

state_domain_excludes(State, Id, Value) :-
    state_fact_matches(State, Id, domain(Domain)),
    ground(Domain),
    \+ variant_member(Value, Domain).

domain_exhausted_after(State, Id, ExtraExcluded) :-
    state_fact_matches(State, Id, domain(Domain)),
    ground(Domain),
    findall(V, state_fact_matches(State, Id, excluded_literal(V)), Existing),
    append(ExtraExcluded, Existing, Excluded),
    domain_all_excluded(Domain, Excluded).

state_fact_matches(State, Id, Pattern) :-
    state_has_fact(State, Id, Stored),
    subsumes_term(Pattern, Stored),
    Pattern = Stored.

id_literal(State, Id, Value) :-
    state_fact_matches(State, Id, literal(Value)),
    ground(Value), !.

all_ids_have(_, [], _).
all_ids_have(State, [Id|Ids], Fact) :-
    state_fact_matches(State, Id, Fact),
    all_ids_have(State, Ids, Fact).

alias_ids(State0, A, B, State) :-
    alias_one_way(State0, A, B, State1),
    alias_one_way(State1, B, A, State).

% A branded Proof can be identical to the Atom it was branded from, so ==
% shares only value/shape facts, not type facts; unification shares both.
alias_ids_without_types(State0, A, B, State) :-
    alias_one_way_without_types(State0, A, B, State1),
    alias_one_way_without_types(State1, B, A, State).

alias_one_way_without_types(State0, From, To, State) :-
    state_facts(State0, From, Facts0),
    exclude(type_fact, Facts0, Facts),
    add_state_facts(Facts, To, State0, State).

type_fact(type(_)).

alias_one_way(State0, From, To, State) :-
    state_facts(State0, From, Facts),
    add_state_facts(Facts, To, State0, State).



% -- Context and definition helpers -------------------------------------

option_closure(Name, Options, Closure) :-
    member(Option, Options),
    nonvar(Option),
    Option =.. [Name, Closure], !.

index_nodes([], []).
index_nodes([Node|Nodes], Definitions) :-
    ( ir_result(Node, Id)
      -> Definitions = [definition(Id, Node)|Rest]
    ; Definitions = Rest ),
    index_nodes(Nodes, Rest).

definition_for_id([definition(Stored, Node)|_], Id, Node) :-
    Stored == Id,
    refinable_definition(Node), !.
definition_for_id([_|Definitions], Id, Node) :-
    definition_for_id(Definitions, Id, Node).
definition_for_id([definition(Stored, Node)|_], Id, Node) :-
    Stored == Id, !.

refinable_definition(value(_, literal(_))).
refinable_definition(call(_, _, _)).
refinable_definition(reify(_, _)).
refinable_definition(construct(_, _, _)).


% Module-local resolver fixtures are intentionally outside begin_tests/1 so
% the analyzer invokes the same closure shape an embedding module supplies.
test_constructor_signature(:, 3,
                           ['Proof', 'Statement', 'TV'],
                           'AnnotatedStatement').

test_call_resolver(F, _, _, data) :-
    memberchk(F, [alpha]), !.
test_call_resolver('statement-accepted?', _, _,
                   summary([ensure(result, type('Bool')),
                            ensure(result, proper_bool)],
                           card(1,1), [pure])).
test_call_resolver(invalid_summary, _, _,
                   summary([], card(2,2), [pure])).
test_call_resolver(invalid_target, _, _,
                   summary([ensure(arg(9), nonvar)], card(1,1), [pure])).


:- begin_tests(ir_analyzer).

test(literals_separate_type_from_instantiation) :-
    lower_expr(true, IR, _, _),
    state_empty(S0),
    analyze_ir(IR, S0, Analysis),
    analysis_card(Analysis, card(1,1)),
    analysis_result_has_fact(Analysis, type('Bool')),
    analysis_result_has_fact(Analysis, proper_bool),
    analysis_result_has_fact(Analysis, ground).

test(bool_type_alone_is_not_proper_bool) :-
    lower_expr(X, IR, Id, _),
    state_empty(S0), state_add_fact(S0, Id, type('Bool'), S1),
    analyze_ir(IR, S1, Analysis),
    analysis_result_has_fact(Analysis, type('Bool')),
    \+ analysis_result_has_fact(Analysis, proper_bool),
    var(X).

test(open_type_fact_does_not_claim_bool) :-
    lower_expr(X, IR, Id, _),
    state_empty(S0), state_add_fact(S0, Id, type(_), S1),
    analyze_ir(IR, S1, Analysis),
    \+ analysis_result_has_fact(Analysis, type('Bool')),
    var(X).

test(proper_list_excluding_empty_is_nonempty) :-
    lower_expr(X, IR, Id, _),
    state_empty(S0),
    state_add_fact(S0, Id, proper_list, S1),
    state_add_fact(S1, Id, excluded_literal([]), S2),
    analyze_ir(IR, S2, Analysis),
    analysis_state(Analysis, State),
    state_has_fact(State, Id, nonempty_list),
    var(X).

test(excluding_empty_then_proper_list_is_nonempty) :-
    state_empty(S0),
    add_facts_flow(reachable(S0), value_id,
                   [excluded_literal([])], reachable(S1)),
    add_facts_flow(reachable(S1), value_id,
                   [proper_list], reachable(State)),
    state_has_fact(State, value_id, nonempty_list).

test(nonempty_context_requires_both_exact_premises) :-
    state_empty(S0),
    add_facts_flow(reachable(S0), proper_only,
                   [proper_list], reachable(S1)),
    \+ state_has_fact(S1, proper_only, nonempty_list),
    add_facts_flow(reachable(S1), exclusion_only,
                   [excluded_literal([])], reachable(S2)),
    \+ state_has_fact(S2, exclusion_only, nonempty_list),
    add_facts_flow(reachable(S2), other_exclusion,
                   [proper_list, excluded_literal(foo)], reachable(S3)),
    \+ state_has_fact(S3, other_exclusion, nonempty_list).

test(inconsistent_initial_state_is_unreachable) :-
    lower_expr(X, IR, Id, _),
    state_empty(S0),
    state_add_fact(S0, Id, literal(true), S1),
    state_add_fact(S1, Id, excluded_literal(true), S2),
    analyze_ir(IR, S2, Analysis),
    analysis_card(Analysis, card(0,0)),
    analysis_diagnostics(Analysis, [inconsistent_initial_state]),
    var(X).

test(source_variable_bool_edge_is_recorded) :-
    lower_expr([if, X, a, b], IR, _, Env),
    env_var_id(Env, X, Id),
    state_empty(S0),
    analyze_ir(IR, S0, Analysis),
    analysis_edge_state(Analysis, Id, true, State),
    state_has_fact(State, Id, literal(true)),
    state_has_fact(State, Id, proper_bool),
    var(X).

test(once_preserves_inner_result_facts) :-
    lower_expr([once, true], IR, _, _),
    state_empty(S0), analyze_ir(IR, S0, Analysis),
    analysis_card(Analysis, card(1,1)),
    analysis_result_has_fact(Analysis, proper_bool).

test(open_quoted_list_is_not_ground) :-
    lower_expr([quote, [X]], IR, _, _),
    state_empty(S0), analyze_ir(IR, S0, Analysis),
    analysis_result_has_fact(Analysis, proper_list),
    \+ analysis_result_has_fact(Analysis, ground),
    var(X).

test(typed_wildcard_is_not_assumed_exhaustive) :-
    Source = [case, X, [[[':', Y, 'Bool'], true]]],
    lower_expr(Source, IR, _, _),
    state_empty(S0),
    analyze_ir(IR, S0, Analysis),
    analysis_card(Analysis, card(0,1)),
    var(X), var(Y).

test(cons_pattern_does_not_make_improper_tail_proper) :-
    Source = [let, [cons, H, T], [cons, 1, 2], [cons, 0, T]],
    lower_expr(Source, IR, _, _),
    state_empty(S0), analyze_ir(IR, S0, Analysis),
    \+ analysis_result_has_fact(Analysis, proper_list),
    var(H), var(T).

test(is_var_true_edge_rejects_known_nonvar) :-
    Source = [if, ['is-var', X], a, b],
    lower_expr(Source, IR, _, Env),
    env_var_id(Env, X, XId),
    state_empty(S0), state_add_fact(S0, XId, nonvar, S1),
    analyze_ir(IR, S1, Analysis),
    analysis_trace(Analysis, Trace),
    \+ memberchk(edge(_, true, _), Trace),
    memberchk(edge(_, false, _), Trace).

test(dynamic_head_failure_short_circuits_call) :-
    lower_expr([[empty], 1], IR, _, _),
    state_empty(S0), analyze_ir(IR, S0, Analysis),
    analysis_card(Analysis, card(0,0)).

test(zero_card_call_makes_following_code_unreachable) :-
    lower_expr([progn, [empty], true], IR, _, _),
    state_empty(S0), analyze_ir(IR, S0, Analysis),
    analysis_card(Analysis, card(0,0)),
    \+ analysis_result_has_fact(Analysis, proper_bool).

test(opaque_child_facts_do_not_escape) :-
    Source = [progn, ['|->', [], [let, X, true, true]], X],
    lower_expr(Source, IR, _, Env),
    env_var_id(Env, X, XId),
    state_empty(S0), analyze_ir(IR, S0, Analysis),
    analysis_card(Analysis, card(0,many)),
    analysis_diagnostics(Analysis, [unsupported('|->')]),
    \+ analysis_result_has_fact(Analysis, proper_bool),
    analysis_state(Analysis, State),
    \+ state_has_fact(State, XId, literal(true)),
    var(X).

test(literal_outside_closed_domain_is_inconsistent) :-
    lower_expr(X, IR, Id, _),
    state_empty(S0),
    state_add_fact(S0, Id, literal(1), S1),
    state_add_fact(S1, Id, type('Bool'), S2),
    analyze_ir(IR, S2, Analysis),
    analysis_card(Analysis, card(0,0)),
    analysis_diagnostics(Analysis, [inconsistent_initial_state]),
    var(X).

test(exhaustive_bool_case_is_det_and_proper_bool) :-
    Source = [case, X, [[true, true], [false, false]]],
    lower_expr(Source, IR, _, Env),
    env_var_id(Env, X, XId),
    state_empty(S0), state_add_fact(S0, XId, type('Bool'), S1),
    analyze_ir(IR, S1, Analysis),
    analysis_card(Analysis, card(1,1)),
    analysis_result_has_fact(Analysis, proper_bool).

test(nonexhaustive_bool_case_is_semidet) :-
    Source = [case, X, [[true, true]]],
    lower_expr(Source, IR, _, Env),
    env_var_id(Env, X, XId),
    state_empty(S0), state_add_fact(S0, XId, type('Bool'), S1),
    analyze_ir(IR, S1, Analysis),
    analysis_card(Analysis, card(0,1)).

test(unify_true_edge_propagates_constructor_field_type) :-
    Source = ['and-then',
              [=, Annotated, [(:), Proof, Statement, TV]],
              ['statement-accepted?', Statement]],
    lower_expr(Source, IR, _, Env),
    env_var_id(Env, Statement, StatementId),
    state_empty(S0),
    analyze_ir(IR, S0,
               [constructor_signature(test_constructor_signature),
                resolve_call(test_call_resolver)],
               Analysis),
    analysis_trace(Analysis, Trace),
    once(( member(edge(_, true, TrueState), Trace),
           state_fact_matches(TrueState, StatementId, type('Statement')) )),
    var(Annotated), var(Proof), var(TV).

test(identity_true_edge_does_not_copy_nominal_type) :-
    Source = [if, [==, Left, Right], true, false],
    lower_expr(Source, IR, _, Env),
    env_var_id(Env, Left, LeftId),
    env_var_id(Env, Right, RightId),
    state_empty(S0),
    state_add_fact(S0, LeftId, type('Proof'), S1),
    state_add_fact(S1, RightId, type('Atom'), S2),
    analyze_ir(IR, S2, Analysis),
    analysis_trace(Analysis, Trace),
    once(member(edge(_, true, TrueState), Trace)),
    state_has_fact(TrueState, LeftId, type('Proof')),
    \+ state_has_fact(TrueState, LeftId, type('Atom')),
    state_has_fact(TrueState, RightId, type('Atom')),
    \+ state_has_fact(TrueState, RightId, type('Proof')).

test(is_member_literal_atom_mode_is_det) :-
    Source = ['is-member', X, [alpha, beta, gamma]],
    lower_expr(Source, IR, _, Env),
    env_var_id(Env, X, XId),
    state_empty(S0),
    state_add_fact(S0, XId, nonvar, S1),
    analyze_ir(IR, S1, [resolve_call(test_call_resolver)], Analysis),
    analysis_card(Analysis, card(1,1)),
    analysis_result_has_fact(Analysis, proper_bool).

test(is_member_duplicate_literal_is_multi) :-
    Source = ['is-member', X, [alpha, alpha]],
    lower_expr(Source, IR, _, Env),
    env_var_id(Env, X, XId),
    state_empty(S0), state_add_fact(S0, XId, nonvar, S1),
    analyze_ir(IR, S1, [resolve_call(test_call_resolver)], Analysis),
    analysis_card(Analysis, card(1,many)).

test(guarded_decons_positional_destructure_is_det) :-
    Source = [if,
              [and, ['is-expr', Term], [not, [==, Term, []]]],
              [let, [Head, Tail], [decons, Term],
               [and, [not, ['is-var', Head]],
                [not, [==, Tail, []]]]],
              false],
    lower_expr(Source, IR, _, _),
    state_empty(S0), analyze_ir(IR, S0, Analysis),
    analysis_card(Analysis, card(1,1)),
    analysis_result_has_fact(Analysis, proper_bool),
    var(Term), var(Head), var(Tail).

test(decons_result_rejects_wrong_positional_width) :-
    Source = [let, [A, B, C], [decons, Term], true],
    lower_expr(Source, IR, _, Env),
    env_var_id(Env, Term, TermId),
    state_empty(S0),
    state_add_fact(S0, TermId, nonempty_list, S1),
    analyze_ir(IR, S1, Analysis),
    analysis_card(Analysis, card(0,0)),
    var(Term), var(A), var(B), var(C).

test(repeated_positional_binder_remains_fallible) :-
    Source = [let, [A, A], [decons, Term], true],
    lower_expr(Source, IR, _, Env),
    env_var_id(Env, Term, TermId),
    state_empty(S0),
    state_add_fact(S0, TermId, nonempty_list, S1),
    analyze_ir(IR, S1, Analysis),
    analysis_card(Analysis, card(0,1)),
    var(Term), var(A).

test(existing_positional_variable_remains_fallible) :-
    Source = [let, [Head, Term], [decons, Term], true],
    lower_expr(Source, IR, _, Env),
    env_var_id(Env, Term, TermId),
    state_empty(S0),
    state_add_fact(S0, TermId, nonempty_list, S1),
    analyze_ir(IR, S1, Analysis),
    analysis_card(Analysis, card(0,1)),
    var(Term), var(Head).

test(unknown_call_is_not_deterministic) :-
    lower_expr([missing, 1], IR, _, _),
    state_empty(S0), analyze_ir(IR, S0, Analysis),
    analysis_card(Analysis, card(0,many)),
    analysis_diagnostics(Analysis, [unknown_call(missing/1)]).

test(invalid_resolver_summary_is_conservative) :-
    lower_expr([invalid_summary], IR, _, _),
    state_empty(S0),
    analyze_ir(IR, S0, [resolve_call(test_call_resolver)], Analysis),
    analysis_card(Analysis, card(0,many)),
    analysis_diagnostics(
        Analysis,
        [invalid_call_summary(invalid_summary/0,
                              summary([], card(2,2), [pure]))]).

test(invalid_summary_target_is_conservative) :-
    lower_expr([invalid_target, value], IR, _, _),
    state_empty(S0),
    analyze_ir(IR, S0, [resolve_call(test_call_resolver)], Analysis),
    analysis_card(Analysis, card(0,many)),
    analysis_diagnostics(
        Analysis,
        [invalid_call_summary(invalid_target/1,
                              summary([ensure(arg(9), nonvar)],
                                      card(1,1), [pure]))]).

test(local_consistency_matches_full_check_after_fact_addition) :-
    state_empty(Empty),
    state_add_facts(Empty, id(untouched), [literal(ok)], State0),
    add_facts_flow(
        reachable(State0), id(changed),
        [literal(true), excluded_literal(true)], LocalFlow),
    assertion(LocalFlow == unreachable),
    state_add_facts(
        State0, id(changed),
        [literal(true), excluded_literal(true)], Contradictory),
    consistent_flow(Contradictory, FullFlow),
    assertion(FullFlow == LocalFlow).

test(local_consistency_preserves_unrelated_valid_entries) :-
    state_empty(Empty),
    state_add_facts(Empty, id(first), [literal(true)], State0),
    add_facts_flow(
        reachable(State0), id(second), [literal(false)], Flow),
    assertion(Flow = reachable(_)).

:- end_tests(ir_analyzer).
