:- module(relational_ir,
          [ lower_expr/4,
            lower_expr_with_origins/5,
            lower_clause_with_origins/5,
            ir_result/2,
            ir_children/2,
            ir_node/2,
            ir_nodes/2,
            env_var_id/3,
            origin_result_id/3
          ]).

/** <module> Standalone relational intermediate representation

This module deliberately has no dependency on PeTTa's translator or legacy
checker.  It only classifies source syntax and preserves the identity of source
Prolog variables.  IDs are deterministic for a given source term and traversal
order; repeated occurrences of the same source variable share one ID.

IR ownership is explicit.  A node which evaluates or structurally lowers one
source occurrence appears exactly once in the tree.  Control edges and reified
relations refer to those nodes by result ID; they do not embed a second copy of
the defining subtree.  In particular, a relation is represented as
`reify(ResultId, relation(LeftId, RightId))` inside a sequence which owns the
two operand nodes.

`call(Id, SurfaceHead, Args)` records unresolved surface application syntax;
it is not a claim that SurfaceHead is callable.  An analyzer with declaration
context decides whether `(alpha beta gamma)` is a call or inert list data.  The
head and every argument remain present as IR, so that decision never needs the
original source term or PeTTa's mutable `fun/1` registry.

The public environment is a first-occurrence-ordered list of
`binding(Id, SourceVar)` terms.  SourceVar is intentionally retained by
identity, rather than copied or used as an ordinary unification key.
*/

%!  lower_expr(+Source, -IR, -ResultId, -Environment) is det.

lower_expr(Source, IR, Result, Env) :-
    lower_expr_with_origins(Source, IR, Result, Env, _).

lower_expr_with_origins(Source, IR, Result, Env, Origins) :-
    with_origin_scope(
        lower_expr_(Source, IR, Result, state(1, []), state(_, RevEnv)),
        Origins),
    reverse(RevEnv, Env).

%!  lower_clause_with_origins(+SourceClause, -IR, -ClauseId, -Environment,
%!                            -Origins) is det.
%
%   A clause is a compilation boundary, not an expression node.  It is kept in
%   the permitted opaque node, while its argument patterns and body are normal
%   relational IR children.

lower_clause_with_origins(Source, IR, ClauseId, Env, Origins) :-
    with_origin_scope(lower_clause_core(Source, IR, ClauseId, Env), Origins).

lower_clause_core(Source, IR, ClauseId, Env) :-
    ( Source = [Eq, Head, Body], Eq == (=),
      nonvar(Head), Head = [F|Args], atom(F), is_list(Args)
    -> fresh_id(ClauseId, state(1, []), S1),
       fresh_id(HeadId, S1, S2),
       length(Args, Arity),
       lower_clause_patterns(F, Arity, Args, 0, ArgIRs, ArgIds, S2, S3),
       lower_expr_(Body, BodyIR, BodyId, S3, state(_, RevEnv)),
       IR = opaque(ClauseId, clause(F, ArgIds, BodyId),
                   [construct(HeadId, head_patterns, ArgIRs), BodyIR]),
       reverse(RevEnv, Env)
    ; throw(error(domain_error(metta_clause, Source), lower_clause/4))
    ).

% A clause argument receives the declared expected type before its structural
% pattern is lowered.  Resolution remains an analyzer concern: the IR records
% `declared_arg(F, Arity, Index)` rather than consulting any declaration store
% here.  Arity is part of the identity: the same symbol may have declarations
% at several arities.
lower_clause_patterns(_, _, [], _, [], [], S, S).
lower_clause_patterns(F, Arity, [P|Ps], Index,
                      [construct(Id, typed_pattern(declared_arg(F, Arity, Index)),
                                 [PatternIR])|IRs],
                      [Id|Ids], S0, S) :-
    fresh_id(Id, S0, S1),
    lower_pattern(P, PatternIR, _, S1, S2),
    Next is Index + 1,
    lower_clause_patterns(F, Arity, Ps, Next, IRs, Ids, S2, S).

% Every expression occurrence is lowered by the core exactly once and records
% its result ID against the original source subterm.  The mapping is separate
% from the IR tree so control/reference ownership remains explicit.
lower_expr_(Source, IR, Result, S0, S) :-
    lower_expr_core(Source, IR, Result, S0, S),
    record_origin(Source, Result).

% Atomic values and source variables.
lower_expr_core(Source, value(Id, source_var), Id, S0, S) :-
    var(Source), !,
    intern_source_var(Source, Id, S0, S).
lower_expr_core([], value(Id, literal([])), Id, S0, S) :- !,
    fresh_id(Id, S0, S).
lower_expr_core(Source, value(Id, literal(Source)), Id, S0, S) :-
    atomic(Source), !,
    fresh_id(Id, S0, S).

% Control forms.
lower_expr_core([If, Cond, Then], sequence([CondIR,
                                       branch(CondId, ThenIR, ElseIR, Result)],
                                      Result),
            Result, S0, S) :-
    If == if, !,
    fresh_id(Result, S0, S1),
    lower_expr_(Cond, CondIR, CondId, S1, S2),
    lower_expr_(Then, ThenIR, _, S2, S3),
    opaque_leaf(no_else, ElseIR, S3, S).
lower_expr_core([If, Cond, Then, Else], sequence([CondIR,
                                             branch(CondId, ThenIR, ElseIR,
                                                    Result)],
                                            Result),
            Result, S0, S) :-
    If == if, !,
    fresh_id(Result, S0, S1),
    lower_expr_(Cond, CondIR, CondId, S1, S2),
    lower_expr_(Then, ThenIR, _, S2, S3),
    lower_expr_(Else, ElseIR, _, S3, S).
lower_expr_core([And, A, B], sequence([AIR,
                                  branch(AId, ThenIR, ElseIR, Result)],
                                 Result),
            Result, S0, S) :-
    And == 'and-then', !,
    fresh_id(Result, S0, S1),
    lower_expr_(A, AIR, AId, S1, S2),
    lower_expr_(B, ThenIR, _, S2, S3),
    literal_node(false, ElseIR, S3, S).
lower_expr_core([Or, A, B], sequence([AIR,
                                 branch(AId, ThenIR, ElseIR, Result)],
                                Result),
            Result, S0, S) :-
    Or == 'or-else', !,
    fresh_id(Result, S0, S1),
    lower_expr_(A, AIR, AId, S1, S2),
    literal_node(true, ThenIR, S2, S3),
    lower_expr_(B, ElseIR, _, S3, S).

% First-match case and pattern bindings.
lower_expr_core([Case, Value, Pairs], sequence([ValueIR, MatchIR], Result),
            Result, S0, S) :-
    Case == case, is_list(Pairs), !,
    fresh_id(Result, S0, S1),
    lower_expr_(Value, ValueIR, _, S1, S2),
    ir_result(ValueIR, ValueId),
    lower_case(Pairs, ValueId, Result, MatchIR, S2, S).
lower_expr_core([Let, Pattern, Value, In],
            sequence([ValueIR,
                      try_match(ValueId, PatternIR, ThenIR, ElseIR, Result)],
                     Result),
            Result, S0, S) :-
    ( Let == let ; Let == chain ), !,
    fresh_id(Result, S0, S1),
    lower_expr_(Value, ValueIR, _, S1, S2),
    ir_result(ValueIR, ValueId),
    lower_pattern(Pattern, PatternIR, _, S2, S3),
    lower_expr_(In, ThenIR, _, S3, S4),
    opaque_leaf(no_match, ElseIR, S4, S).
lower_expr_core([LetStar, Binds, Body], IR, Result, S0, S) :-
    LetStar == 'let*', is_list(Binds), !,
    letstar_source(Binds, Body, Nested),
    lower_expr_(Nested, IR, Result, S0, S).

% Sequencing.  sequence/2's Result is the selected child's result ID.
lower_expr_core([Progn|Exprs], sequence(IRs, Result), Result, S0, S) :-
    Progn == progn, !,
    lower_exprs(Exprs, IRs, Ids, S0, S),
    ( last(Ids, Result) -> true
    ; throw(error(domain_error(nonempty_sequence, [Progn|Exprs]), lower_expr/4))
    ).
lower_expr_core([Prog1|Exprs], sequence(IRs, Result), Result, S0, S) :-
    Prog1 == prog1, !,
    lower_exprs(Exprs, IRs, Ids, S0, S),
    ( Ids = [Result|_] -> true
    ; throw(error(domain_error(nonempty_sequence, [Prog1|Exprs]), lower_expr/4))
    ).

% Reified relations.  Operand definitions belong to the surrounding sequence;
% the relation itself contains stable references only.  Besides preventing a
% duplicated traversal, this lets an analyzer index definitions by ID and
% attach success-edge refinements to the result of `unify/2`.
lower_expr_core([Op, A, B], sequence([AIR, BIR, reify(Id, Relation)], Id),
            Id, S0, S) :-
    nonvar(Op), relation_operator(Op, RelationName), !,
    fresh_id(Id, S0, S1),
    lower_relation_operand(RelationName, A, AIR, AId, S1, S2),
    lower_relation_operand(RelationName, B, BIR, BId, S2, S),
    Relation =.. [RelationName, AId, BId].

% Explicit constructors.
lower_expr_core([Data|Fields], construct(Id, data, Children), Id, S0, S) :-
    Data == data, !,
    fresh_id(Id, S0, S1),
    lower_exprs(Fields, Children, _, S1, S).
lower_expr_core([MakeList|Elements], construct(Id, list, Children), Id, S0, S) :-
    MakeList == 'make-list', !,
    fresh_id(Id, S0, S1),
    lower_exprs(Elements, Children, _, S1, S).
lower_expr_core([Cons, Head, Tail], construct(Id, cons, [HeadIR, TailIR]),
            Id, S0, S) :-
    ( Cons == cons ; Cons == 'cons-atom' ), !,
    fresh_id(Id, S0, S1),
    lower_expr_(Head, HeadIR, _, S1, S2),
    lower_expr_(Tail, TailIR, _, S2, S).

% Cardinality-control forms.
lower_expr_core([Once, Expr], once(ExprIR, Result), Result, S0, S) :-
    Once == once, !,
    fresh_id(Result, S0, S1),
    lower_expr_(Expr, ExprIR, _, S1, S).
lower_expr_core([Collapse, Expr], collect(ExprIR, Result), Result, S0, S) :-
    Collapse == collapse, !,
    fresh_id(Result, S0, S1),
    lower_expr_(Expr, ExprIR, _, S1, S).

% Forms whose evaluation/binding rules are intentionally outside this first IR
% slice.  quote is not traversed: its payload is source data, not an expression.
lower_expr_core([Quote, Payload], opaque(Id, quote(Payload), []), Id, S0, S) :-
    Quote == quote, !,
    fresh_id(Id, S0, S).
lower_expr_core([Tag|Args], opaque(Id, Tag, Children), Id, S0, S) :-
    atom(Tag), opaque_source_form(Tag), !,
    fresh_id(Id, S0, S1),
    lower_exprs(Args, Children, _, S1, S).

% Calls do not consult fun/1, declarations, builtin tables, or checker state.
lower_expr_core([F|Args], call(Id, F, Children), Id, S0, S) :-
    atom(F), is_list(Args), !,
    fresh_id(Id, S0, S1),
    lower_exprs(Args, Children, _, S1, S).
lower_expr_core([Head|Args], call(Id, HeadIR, Children), Id, S0, S) :-
    is_list(Args), !,
    fresh_id(Id, S0, S1),
    lower_expr_(Head, HeadIR, _, S1, S2),
    lower_exprs(Args, Children, _, S2, S).

% Non-list compounds can be introduced by embedders.  Preserve their shape but
% make no claim about their evaluation.
lower_expr_core(Source, opaque(Id, compound(Name), Children), Id, S0, S) :-
    compound(Source),
    compound_name_arguments(Source, Name, Args),
    fresh_id(Id, S0, S1),
    lower_exprs(Args, Children, _, S1, S).

lower_case([], _, Result, opaque(Result, no_match, []), S, S).
lower_case([[Pattern, Then]|Rest], ValueIR, Result,
           try_match(ValueIR, PatternIR, ThenIR, ElseIR, Result), S0, S) :-
    !,
    lower_pattern(Pattern, PatternIR, _, S0, S1),
    lower_expr_(Then, ThenIR, _, S1, S2),
    ( Rest == []
    -> opaque_leaf(no_match, ElseIR, S2, S)
    ;  fresh_id(ElseResult, S2, S3),
       lower_case(Rest, ValueIR, ElseResult, ElseIR, S3, S)
    ).
lower_case([Bad|_], _, _, _, _, _) :-
    throw(error(domain_error(case_pair, Bad), lower_expr/4)).

% Patterns are inert unless a typed-pattern annotation explicitly wraps them.
lower_pattern(Source, value(Id, pattern_var(Role)), Id, S0, S) :-
    var(Source), !,
    pattern_variable_role(Source, S0, Role),
    intern_source_var(Source, Id, S0, S).
lower_pattern([], value(Id, literal([])), Id, S0, S) :- !,
    fresh_id(Id, S0, S).
lower_pattern(Source, value(Id, literal(Source)), Id, S0, S) :-
    atomic(Source), !,
    fresh_id(Id, S0, S).
lower_pattern([Colon, Pattern, Type],
              construct(Id, typed_pattern(Type), [PatternIR]), Id, S0, S) :-
    Colon == (:), !,
    fresh_id(Id, S0, S1),
    lower_pattern(Pattern, PatternIR, _, S1, S).
lower_pattern([Cons, Head, Tail], construct(Id, pattern_cons, [HeadIR, TailIR]),
              Id, S0, S) :-
    ( Cons == cons ; Cons == 'cons-atom' ), !,
    fresh_id(Id, S0, S1),
    lower_pattern(Head, HeadIR, _, S1, S2),
    lower_pattern(Tail, TailIR, _, S2, S).
% A variable in the first position is a field binder, not a constructor tag.
% Preserve every field of such a fixed-width positional pattern so matching
% can use exact list-shape facts and later body occurrences share the same IDs.
lower_pattern([Head|Args],
              construct(Id, positional_pattern, Children), Id, S0, S) :-
    var(Head), is_list(Args), !,
    fresh_id(Id, S0, S1),
    lower_patterns([Head|Args], Children, _, S1, S).
lower_pattern([Head|Args], construct(Id, pattern(Head), Children), Id, S0, S) :-
    is_list(Args), !,
    fresh_id(Id, S0, S1),
    lower_patterns(Args, Children, _, S1, S).
lower_pattern(Source, opaque(Id, pattern_compound(Name), Children), Id, S0, S) :-
    compound(Source),
    compound_name_arguments(Source, Name, Args),
    fresh_id(Id, S0, S1),
    lower_patterns(Args, Children, _, S1, S).

pattern_variable_role(Var, state(_, Env), existing) :-
    env_var_id(Env, Var, _), !.
pattern_variable_role(_, _, fresh).

% `=/2` is the sole relation in this family whose successful result commits
% bindings.  Definite structural syntax is therefore lowered as a pattern for
% that relation, so its field variables can receive success-edge facts.  The
% colon form covers both typed-pattern notation `(: P T)` and constructor
% tuples such as `(: Proof Statement TV)`.  Explicit cons syntax similarly
% denotes a pair pattern.  Other list-headed operands remain expressions here:
% deciding whether an arbitrary atom names a constructor or a function needs a
% declaration resolver and belongs in the analyzer, not this standalone pass.
lower_relation_operand(Relation, Source, IR, Id, S0, S) :-
    structural_relation(Relation),
    definite_unification_pattern(Source), !,
    lower_pattern(Source, IR, Id, S0, S).
lower_relation_operand(_, Source, IR, Id, S0, S) :-
    lower_expr_(Source, IR, Id, S0, S).

structural_relation(unify).
structural_relation(unifiable).
structural_relation(variant).

definite_unification_pattern([Head|Args]) :-
    is_list(Args),
    ( Head == (:)
    ; Head == cons
    ; Head == 'cons-atom'
    ).

lower_exprs([], [], [], S, S).
lower_exprs([E|Es], [IR|IRs], [Id|Ids], S0, S) :-
    lower_expr_(E, IR, Id, S0, S1),
    lower_exprs(Es, IRs, Ids, S1, S).

lower_patterns([], [], [], S, S).
lower_patterns([P|Ps], [IR|IRs], [Id|Ids], S0, S) :-
    lower_pattern(P, IR, Id, S0, S1),
    lower_patterns(Ps, IRs, Ids, S1, S).

letstar_source([], Body, Body).
letstar_source([[Pattern, Value]|Rest], Body,
               [let, Pattern, Value, Nested]) :-
    letstar_source(Rest, Body, Nested).
letstar_source([Bad|_], _, _) :-
    throw(error(domain_error(let_binding, Bad), lower_expr/4)).

relation_operator(=, unify).
relation_operator('=?', unifiable).
relation_operator('==', identical).
relation_operator('!=', not_identical).
relation_operator('=alpha', variant).
relation_operator('=@=', variant).

opaque_source_form('|->').
opaque_source_form(match).
opaque_source_form(foldall).
opaque_source_form(forall).
opaque_source_form(hyperpose).
opaque_source_form(with_mutex).
opaque_source_form(transaction).
opaque_source_form(sealed).
opaque_source_form(eval).
opaque_source_form(reduce).
opaque_source_form(call).
opaque_source_form(translatePredicate).
opaque_source_form(catch).

literal_node(Value, value(Id, literal(Value)), S0, S) :-
    fresh_id(Id, S0, S).

opaque_leaf(Tag, opaque(Id, Tag, []), S0, S) :-
    fresh_id(Id, S0, S).

fresh_id(id(N), state(N, Env), state(N1, Env)) :-
    N1 is N + 1.

intern_source_var(Var, Id, state(N, Env), state(N, Env)) :-
    env_var_id(Env, Var, Id), !.
intern_source_var(Var, Id, state(N, Env), state(N1, [binding(Id, Var)|Env])) :-
    Id = id(N),
    N1 is N + 1.


%!  env_var_id(+Environment, +SourceVar, -Id) is semidet.

env_var_id([binding(Id, Stored)|_], Var, Id) :- Stored == Var, !.
env_var_id([_|Bindings], Var, Id) :- env_var_id(Bindings, Var, Id).

%!  origin_result_id(+Origins, +SourceSubterm, -ResultId) is nondet.
%
%   Relate an occurrence recorded during lowering to its result ID.  Variable
%   identity is preserved; ground duplicate subterms may intentionally yield
%   more than one occurrence and callers which care about position keep the
%   surrounding control-node ID as context.

origin_result_id([origin(Id, Stored)|_], Source, Id) :- Stored == Source.
origin_result_id([_|Origins], Source, Id) :-
    origin_result_id(Origins, Source, Id).

with_origin_scope(Goal, Origins) :-
    ( catch(b_getval('$relational_ir_origins', Saved), _, fail)
      -> HadSaved = yes
    ; Saved = [], HadSaved = no ),
    setup_call_cleanup(
        b_setval('$relational_ir_origins', []),
        ( call(Goal),
          b_getval('$relational_ir_origins', Reversed),
          reverse(Reversed, Origins) ),
        ( HadSaved == yes
          -> b_setval('$relational_ir_origins', Saved)
        ; b_setval('$relational_ir_origins', inactive) )).

record_origin(Source, Id) :-
    catch(b_getval('$relational_ir_origins', Origins), _, Origins = inactive),
    ( is_list(Origins)
      -> b_setval('$relational_ir_origins', [origin(Id, Source)|Origins])
    ; true ).

%!  ir_result(+IR, -ResultId) is semidet.

ir_result(value(Id, _), Id).
ir_result(construct(Id, _, _), Id).
ir_result(call(Id, _, _), Id).
ir_result(reify(Id, _), Id).
ir_result(sequence(_, Result), Result).
ir_result(branch(_, _, _, Result), Result).
ir_result(try_match(_, _, _, _, Result), Result).
ir_result(once(_, Result), Result).
ir_result(collect(_, Result), Result).
ir_result(opaque(Id, _, _), Id).

%!  ir_children(+IR, -Children) is det.

ir_children(value(_, _), []).
ir_children(construct(_, _, Children), Children).
ir_children(call(_, F, Args), Children) :-
    ( ir_term(F) -> Children = [F|Args] ; Children = Args ).
% Relations and control edges carry ID references.  Only legacy/manually-built
% IR terms embedded in those positions are treated as owned children; IDs are
% deliberately not traversed a second time.
ir_children(reify(_, Relation), Children) :-
    compound_name_arguments(Relation, _, Args),
    include_ir_terms(Args, Children).
ir_children(sequence(Exprs, _), Exprs).
ir_children(branch(Test, Then, Else, _), Children) :-
    include_ir_terms([Test, Then, Else], Children).
ir_children(try_match(Value, Pattern, Then, Else, _), Children) :-
    include_ir_terms([Value, Pattern, Then, Else], Children).
ir_children(once(Expr, _), [Expr]).
ir_children(collect(Expr, _), [Expr]).
ir_children(opaque(_, _, Children), Children).

ir_term(value(_, _)).
ir_term(construct(_, _, _)).
ir_term(call(_, _, _)).
ir_term(reify(_, _)).
ir_term(sequence(_, _)).
ir_term(branch(_, _, _, _)).
ir_term(try_match(_, _, _, _, _)).
ir_term(once(_, _)).
ir_term(collect(_, _)).
ir_term(opaque(_, _, _)).

include_ir_terms([], []).
include_ir_terms([A|As], IRs) :-
    ( ir_term(A) -> IRs = [A|Rest] ; IRs = Rest ),
    include_ir_terms(As, Rest).

%!  ir_node(+IR, -Node) is nondet.

ir_node(IR, IR).
ir_node(IR, Node) :-
    ir_children(IR, Children),
    member(Child, Children),
    ir_node(Child, Node).

%!  ir_nodes(+IR, -Nodes) is det.

ir_nodes(IR, Nodes) :- ir_nodes_(IR, Nodes, []).

ir_nodes_(IR, [IR|Rest], Tail) :-
    ir_children(IR, Children),
    ir_nodes_children(Children, Rest, Tail).

ir_nodes_children([], Tail, Tail).
ir_nodes_children([IR|IRs], Nodes, Tail) :-
    ir_nodes_(IR, Nodes, Mid),
    ir_nodes_children(IRs, Mid, Tail).


:- begin_tests(relational_ir).

test(source_variable_identity) :-
    lower_expr([progn, X, X], IR, Result, Env),
    IR = sequence([value(Id, source_var), value(Id, source_var)], Result),
    Result == Id,
    env_var_id(Env, X, EnvId),
    EnvId == Id,
    Env = [binding(Id, Stored)],
    Stored == X.

test(and_then_is_branch_on_left_result) :-
    lower_expr(['and-then', [p], [q]], IR, _, []),
    IR = sequence([call(PId, p, []),
                   branch(PId,
                          call(_, q, []), value(_, literal(false)), _)], _).

test(reified_relation_owns_operands_once) :-
    lower_expr(['==', [left, 1], [right, 2]], IR, Result, []),
    IR = sequence([call(LeftId, left, [value(_, literal(1))]),
                   call(RightId, right, [value(_, literal(2))]),
                   reify(Result, identical(LeftId, RightId))], Result),
    ir_children(reify(Result, identical(LeftId, RightId)), []),
    ir_nodes(IR, Nodes),
    findall(1, member(reify(_, _), Nodes), Reifies),
    Reifies == [1].

test(higher_order_head_is_not_instantiated_by_relation_recognition) :-
    Source = [F, A, B],
    lower_expr(Source, IR, _, Env),
    var(F),
    IR = call(_, value(FId, source_var),
              [value(AId, source_var), value(BId, source_var)]),
    env_var_id(Env, F, FId),
    env_var_id(Env, A, AId),
    env_var_id(Env, B, BId).

test(origin_map_preserves_source_variable_identity) :-
    Source = [if, Test, Test, false],
    lower_expr_with_origins(Source, _, _, Env, Origins),
    env_var_id(Env, Test, TestId),
    findall(Id, origin_result_id(Origins, Test, Id), OccurrenceIds),
    OccurrenceIds == [TestId, TestId].

test(and_then_unify_exposes_structural_pattern_on_true_edge) :-
    Source = ['and-then',
              [=, Annotated, [(:), Proof, Statement, TV]],
              ['statement-accepted?', Statement]],
    lower_expr(Source, IR, _, Env),
    IR = sequence([
             sequence([value(AnnotatedId, source_var),
                       construct(PatternId, pattern(:),
                                 [value(ProofId, pattern_var(fresh)),
                                  value(StatementId, pattern_var(fresh)),
                                  value(TVId, pattern_var(fresh))]),
                       reify(EqualId, unify(AnnotatedId, PatternId))], EqualId),
             branch(EqualId,
                    call(_, 'statement-accepted?',
                         [value(StatementId, source_var)]),
                    value(_, literal(false)), _)], _),
    env_var_id(Env, Annotated, AnnotatedId),
    env_var_id(Env, Proof, ProofId),
    env_var_id(Env, Statement, StatementId),
    env_var_id(Env, TV, TVId),
    ir_children(reify(EqualId, unify(AnnotatedId, PatternId)), []).

test(surface_application_preserves_call_or_data_evidence) :-
    lower_expr(['is-member', X, [alpha, beta, gamma]], IR, _, Env),
    IR = call(_, 'is-member',
              [value(XId, source_var),
               call(_, alpha,
                    [value(_, literal(beta)), value(_, literal(gamma))])]),
    env_var_id(Env, X, XId).

test(case_is_nested_try_match_with_typed_pattern) :-
    Source = [case, X,
              [[[(:), Y, 'Number'], [number_case, Y]],
               [other, [fallback, X]]]],
    lower_expr(Source, IR, _, Env),
    IR = sequence([value(XId, source_var),
                   try_match(XId,
                             construct(_, typed_pattern('Number'),
                                       [value(YId, pattern_var(fresh))]),
                             call(_, number_case, [value(YId, source_var)]),
                             try_match(XId,
                                       value(_, literal(other)),
                                       call(_, fallback, [value(XId, source_var)]),
                                       opaque(_, no_match, []), _), _)], _),
    env_var_id(Env, X, XId),
    env_var_id(Env, Y, YId),
    XId \== YId, !.

test(let_star_is_nested_match) :-
    lower_expr(['let*', [[X, 1], [Y, X]], [pair, X, Y]], IR, _, _),
    IR = sequence([value(_, literal(1)),
                   try_match(_, value(XId, pattern_var(fresh)),
                             sequence([value(XId, source_var),
                                       try_match(XId,
                                                 value(YId, pattern_var(fresh)),
                                                 call(_, pair,
                                                      [value(XId, source_var),
                                                       value(YId, source_var)]),
                                                 opaque(_, no_match, []), _)], _),
                             opaque(_, no_match, []), _)], _), !.

test(variable_head_list_pattern_is_positional) :-
    Source = [let, [Head, Tail], [decons, Value], [pair, Head, Tail]],
    lower_expr(Source, IR, _, Env),
    IR = sequence([
             call(ValueId, decons, [value(SourceValueId, source_var)]),
             try_match(ValueId,
                       construct(_, positional_pattern,
                                 [value(HeadId, pattern_var(fresh)),
                                  value(TailId, pattern_var(fresh))]),
                       call(_, pair,
                            [value(HeadId, source_var),
                             value(TailId, source_var)]),
                       opaque(_, no_match, []), _)], _),
    env_var_id(Env, Value, SourceValueId),
    env_var_id(Env, Head, HeadId),
    env_var_id(Env, Tail, TailId),
    HeadId \== TailId.

test(clause_shape) :-
    lower_clause_with_origins([=, [f, X], [data, X, 1]], IR, ClauseId, Env, _),
    IR = opaque(ClauseId, clause(f, [XId], BodyId),
                [construct(_, head_patterns,
                           [construct(XId, typed_pattern(declared_arg(f, 1, 0)),
                                      [value(SourceXId, pattern_var(fresh))])]),
                 construct(BodyId, data,
                           [value(SourceXId, source_var), value(_, literal(1))])]),
    env_var_id(Env, X, SourceXId).

test(inspection_helpers) :-
    lower_expr([once, [collapse, [f, 1]]], IR, Result, _),
    ir_result(IR, Result),
    IR = once(_, _),
    ir_nodes(IR, Nodes),
    member(collect(_, _), Nodes),
    member(call(_, f, _), Nodes), !.

:- end_tests(relational_ir).
