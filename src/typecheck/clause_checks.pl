%%% Clause-level helpers %%%
%
% Owns clause-head declaration selection, parameter/pattern binding,
% contextual call results, and declared-output certification.
% Consumes declaration/type-language queries, value checking and guards,
% inference, oracle hooks, and proof-event publication.
% Boundary: every predicate defined here is wholly owned by this file; callers
% use these predicates as an ordinary interface, never by clause interleaving.

%Bind declared parameter types onto clause-head variables. For an overloaded
%function, the clause's head patterns filter the declarations: a clause whose
%head selects exactly one overload is checked against it, a clause no overload
%can produce is rejected, and a genuinely ambiguous clause (e.g. all-variable
%head serving every overload) stays unchecked as before.
clause_param_types(F, Args, DeclOut) :- length(Args, N),
                                        findall(ATs-OTx, fn_decl_arity(F, N, ATs, OTx), Decls),
                                        ( Decls == [] -> DeclOut = none
                                        ; Decls = [ATs1-OT] -> maplist(bind_param_type, Args, ATs1), DeclOut = out(OT, ATs1)
                                        ; include(clause_head_survives(Args), Decls, Survivors),
                                          ( Survivors == [] -> throw(error(no_matching_overload(F), typecheck))
                                          ; Survivors = [ATs1-OT] -> maplist(bind_param_type, Args, ATs1),
                                                                     DeclOut = out(OT, ATs1)
                                          ; DeclOut = none ) ).

clause_head_survives(Args, ATs-_) :- \+ \+ maplist(head_arg_soft, Args, ATs).
head_arg_soft(A, T) :- ( var(A) -> true
                       ; check_value(A, T, St) -> St \== mismatch
                       ; true ).

bind_param_type(Arg, T) :- ( functional_pattern_application(Arg, _, _)
                           -> ( functional_pattern_signature(Arg, T, PatternArgs, ArgTypes)
                                -> maplist(bind_param_type, PatternArgs, ArgTypes)
                              ; true )
                           ; var(Arg) -> ( nonvar(T) -> ( \+ wildcard_type_t(T) -> add_known_type(Arg, T)
                                                                                  ; true )
                                           %a variable type is the declaration instance: recording it
                                           %lets identical unknowns be recognized (e.g. rcons's $a):
                                           ; add_known_type(Arg, T) )
                           ; list_type(T, ET), Arg = [H|Rest]
                             -> bind_param_type(H, ET),          %type element vars of list patterns
                                bind_param_type(Rest, ['List', ET])
                           ; is_union(T)                        %clause heads narrow union params
                             -> bind_pattern_typed(Arg, T)
                           ; structural_pattern_fields(Arg, T, Fields, FieldTs)
                             -> maplist(bind_param_type, Fields, FieldTs)
                           ; atom(T), declared_newtype(T, R), \+ wildcard_type_t(R)
                             -> bind_param_type(Arg, R)
                           ; is_list(Arg), is_list(T), same_length(Arg, T),
                             \+ is_arrow_type(T)                 %untagged tuple types: ($v Number)
                             -> maplist(bind_param_type, Arg, T)
                           ; check_value(Arg, T, St),
                             ( St == mismatch -> throw(error(literal_type_mismatch(Arg, T), typecheck))
                                               ; true ) ).

%Resolve a registered function-headed pattern from one fresh declaration
%instance.  Its result is the value occupying the surrounding pattern slot;
%unifying that result with the slot type instantiates any shared declaration
%variables before its argument patterns are bound.
functional_pattern_signature(Pattern, Expected, Args, ArgTypes) :-
    functional_pattern_application(Pattern, F, Args),
    length(Args, N),
    findall(sig(ATs0, OT0), fn_decl_arity(F, N, ATs0, OT0),
            [sig(ArgTypes, OutType)]),
    type_unify(OutType, Expected),
    refine_functional_pattern_aliases(F, N, Expected, ArgTypes).

%A relational function may make an otherwise independent input equal to its
%result on every successful clause.  This is ordinary source semantics, not a
%named as-pattern rule: a direct returned parameter and variable-to-variable
%let/chain unifications form a small alias graph.  Intersecting the positions
%over every clause is conservative; an unrecognized body contributes no
%aliases.  The clause-set dependency keeps later function mutation honest.
refine_functional_pattern_aliases(F, N, Expected, ArgTypes) :-
    functional_output_alias_positions(F, N, Positions),
    Positions \== [], !,
    analysis_emit(dependency(clause_set(F/N))),
    refine_alias_arg_types(Positions, ArgTypes, Expected).
refine_functional_pattern_aliases(_, _, _, _).

refine_alias_arg_types([], _, _).
refine_alias_arg_types([I|Is], ArgTypes, Expected) :-
    nth0(I, ArgTypes, ArgType),
    type_unify(ArgType, Expected),
    refine_alias_arg_types(Is, ArgTypes, Expected).

functional_output_alias_positions(F, N, Positions) :-
    findall(source(Args, Body),
            ( translated_from(_, [=, [F|Args], Body]),
              length(Args, N) ),
            Sources),
    Sources \== [],
    maplist(source_clause_output_aliases, Sources, PerClause),
    PerClause = [First|Rest],
    foldl(intersection, Rest, First, Positions).

source_clause_output_aliases(source(Args0, Body0), Positions) :-
    copy_term_nat(Args0-Body0, Args-Body),
    ( source_success_result_alias(Body, Result)
      -> findall(I,
                 ( nth0(I, Args, Arg), var(Arg), Arg == Result ),
                 Positions)
    ; Positions = [] ).

source_success_result_alias(Body, Body) :- var(Body), !.
source_success_result_alias([Kind, Pattern, Value, In], Result) :-
    ( Kind == let ; Kind == chain ),
    var(Pattern), var(Value), !,
    Pattern = Value,
    source_success_result_alias(In, Result).

%Which union member does a pattern's shape select?
pattern_selects_member(P, M) :- nonvar(M), nonvar(P),
                                ( list_type(M, _) -> ( P == [] ; P = [C|_], C == cons ; is_list(P) )
                                ; atom(M) -> \+ \+ structural_pattern_fields(P, M, _, _)
                                ; is_list(M), is_list(P)
                                  -> ( tagged_tuple_type(M, Tag, FTs)
                                       -> P = [Tag2|Fs], Tag2 == Tag, same_length(Fs, FTs)
                                        ; same_length(P, M) )
                                ; fail ).

%Tag evidence outranks shape: (box $pair) also parses as a plain list, but a
%head atom that is a declared constructor of (or the tag of) exactly one
%member makes that member the selection - nominal tags are the idiomatic
%union discriminator (see strict_tuple_types.metta):
pattern_selects_member_tagged(P, M) :- nonvar(M), nonvar(P), P = [Tag|Fs], atom(Tag),
                                       ( atom(M) -> \+ \+ structural_pattern_fields(P, M, _, _)
                                       ; tagged_tuple_type(M, Tag2, FTs), Tag2 == Tag,
                                         same_length(Fs, FTs) ).

%A tagged pattern (Tag P1 ... Pn) against either the structural tuple type
%(Tag T1 ... Tn) or a nominal type produced by Tag's constructor declaration:
structural_pattern_fields(Arg, T, Fields, FieldTs) :- is_list(Arg), Arg = [Tag|Fields], atom(Tag), nonvar(T),
                                                      ( tagged_tuple_type(T, Tag2, FieldTs), Tag2 == Tag,
                                                        same_length(Fields, FieldTs) -> true
                                                      ; atom(T), length(Fields, N),
                                                        findall(ATs-OT, fn_decl_arity(Tag, N, ATs, OT), [FieldTs-OT1]),
                                                        type_compat_soft(OT1, T) ).

%Contextual output typing for deliberately-undeclared builtins (one clause per
%builtin; the translator consults this after translating an undeclared call):
untyped_call_out(F, Args, Out) :-
        length(Args, N),
        builtin_contextual_typing(F, N, Rule),
        builtin_contextual_output_rule(Rule, Args, Out).

builtin_contextual_typing_rule_defined(cons_list).
builtin_contextual_typing_rule_defined(union_list).
builtin_contextual_typing_rule_defined(first_list).
builtin_contextual_typing_rule_defined(list_element).
builtin_contextual_typing_rule_defined(list_tail).

builtin_contextual_output_rule(cons_list, [H, Tl], Out) :-
        cons_out_type(H, Tl, Out).
builtin_contextual_output_rule(union_list, [A, B], Out) :-
        union_atom_out_type(A, B, Out).
builtin_contextual_output_rule(first_list, [A, _], Out) :-
        first_list_out_type(A, Out).
builtin_contextual_output_rule(first_list, [A], Out) :-
        first_list_out_type(A, Out).
%The ACCESSORS, which nobody typed: only the constructors above were here, so
%a (List Choice) went in and an untyped value came out. That is what forced a
%runtime guard on every element use and made --strict reject
%(probe (car-atom $xs)) - the loss was at the accessor, not at match or
%collapse. first/2 is a lib_roman pair helper, a different function; only the
%one-argument builtin is typed here.
builtin_contextual_output_rule(list_element, [A], Out) :-
        list_elem_out_type(A, Out).
builtin_contextual_output_rule(list_tail, [A], Out) :-
        cdr_atom_out_type(A, Out).

%(List T) -> T. The element type is what the accessor projects; whether the
%call SUCCEEDS is a separate question and belongs to the determinism table
%(where car-atom is det because its second clause answers () for anything the
%first does not match, and last is semidet-or-worse for the empty list).
list_elem_out_type(A, Out) :- ( var(Out), list_source_elem(A, T), nonvar(T),
                                \+ wildcard_type_t(T)
                                -> set_out_type(Out, T) ; true ).

%An expression's tail is always a sequence, so cdr-atom keeps the (List ...)
%floor its old declaration gave it, and narrows the element type whenever the
%argument's own type supplies one:
cdr_atom_out_type(A, Out) :- ( var(Out), list_source_elem(A, T), nonvar(T)
                               -> set_out_type(Out, ['List', T])
                                ; set_out_type(Out, ['List', '%Undefined%']) ).

%Element-filtering builtins preserve their first argument's list type; the
%other operand may be any expression:
first_list_out_type(A, Out) :- ( var(Out), list_source_elem(A, T)
                                 -> set_out_type(Out, ['List', T]) ; true ).

%cons stays undeclared (a global (List $a) signature would reject legal
%heterogeneous expressions). When the head provably fits the tail's list type
%the result is that list type; when it provably does NOT - both types known,
%no fit - the result is still a proper list, of the WIDENED element type: the
%union of what the tail holds and what the head is, exactly how collapse
%records disagreeing branches. (cons () (cons (item 1) ())) is a
%(List (| Item (List ...))), which fits a declared (List Atom) - every member
%fits Atom - while against (List Number) the non-fitting member still costs
%the guard, so nothing is discharged that the value cannot honour. A head of
%UNKNOWN type still yields no claim: unknown is not evidence of anything, a
%union member included.
cons_out_type(H, Tl, Out) :- ( var(Out), list_source_elem(Tl, T)
                               -> ( ( wildcard_type_t(T) -> true    %(List %Undefined%): any head fits
                                    ; var(H) -> known_singleton(H, K), type_unify(K, T)
                                              ; check_value(H, T, St), St == ok )
                                    -> set_out_type(Out, ['List', T])
                                  ; nonvar(T), cons_head_type(H, KH)
                                    -> union_widen(T, KH, U),
                                       set_out_type(Out, ['List', U])
                                     ; true )
                                ; true ).

cons_head_type(H, KH) :- ( var(H) -> known_singleton(H, KH0), nonvar(KH0), KH = KH0
                                   ; value_single_type(H, KH) ).

%The union of two element types, flattening existing unions and deduplicating
%by variant so repeated widening stays small:
union_widen(T, KH, U) :- ( is_union(T) -> T = ['|'|Ms] ; Ms = [T] ),
                         ( is_union(KH) -> KH = ['|'|Ks] ; Ks = [KH] ),
                         variant_union(Ks, Ms, U0),
                         ( U0 = [Single] -> U = Single ; U = ['|'|U0] ).

%union-atom likewise stays undeclared, but concatenating two provably
%compatible lists yields that list type:
union_atom_out_type(A, B, Out) :- ( var(Out),
                                    list_source_elem(A, TA),
                                    list_source_elem(B, TB)
                                    -> ( type_unify(TA, TB)
                                         -> set_out_type(Out, ['List', TA])
                                       %incompatible element types widen, as in cons_out_type/3 -
                                       %the concatenation is still a proper list of both:
                                       ; nonvar(TA), nonvar(TB)
                                         -> union_widen(TA, TB, U),
                                            set_out_type(Out, ['List', U])
                                          ; true )
                                     ; true ).

%The element type carried by a list-valued SOURCE expression, the one question
%behind every list-shape output typer above: a cons TAIL, a union/concat OPERAND,
%an accessor's list ARGUMENT all ask it (cons_tail_elem/2 was a second, identical
%copy). A bound variable answers from its known (List T); a literal () carries no
%element constraint (T stays open, so any head fits); a literal list is read
%element-wise (list_elem_type/2). The callers differ only in how they re-wrap the
%answer - the element itself (car-atom), or (List T) again (cdr-atom, concat):
list_source_elem(X, T) :- ( var(X) -> known_singleton(X, K), list_type(K, T)
                          ; X == [] -> true
                          ; list_elem_type(X, T) ).

%Destructuring bindings: type a pattern's variables from the bound value's
%known type, e.g. (let (Stats $sum $sq $n) (make-stats) ...). With no type for
%the value, a pattern headed by a uniquely declared constructor still knows
%what its own fields are - that is the constructor's declaration talking, not
%the scrutinee's:
bind_pattern_from(Pat, Val) :- ( nonvar(Pat)
                                 -> ( ( var(Val) -> known_singleton(Val, KT)
                                                  ; value_single_type(Val, KT) ),
                                      nonvar(KT)                %an open assumption type says nothing yet
                                      -> bind_pattern_typed(Pat, KT)
                                       ; ctor_pattern_field_types(Pat) )
                                  ; true ).

%Tolerant variant used where a non-matching pattern must not fail or throw
%(case branches: a wrong pattern just never matches at runtime):
bind_pattern_typed(P, T) :- bind_pattern_typed(P, T, []).

%bind_pattern_typed(+Pattern, +Type, +PriorPatterns). PriorPatterns are the
%patterns of EARLIER branches of the same case, in source order, and are empty
%for every other caller (clause heads, let destructuring, meta typing) - those
%are not first-match. They are consulted only for the top-level pattern; field
%patterns recurse with [], since what an earlier branch matched at the top says
%nothing about a nested field.
bind_pattern_typed(P, T, Prior) :-
                            ( functional_pattern_application(P, _, _)
                              -> ( functional_pattern_signature(P, T, PatternArgs, ArgTypes)
                                   -> maplist(bind_pattern_typed, PatternArgs, ArgTypes)
                                 ; true )
                            ; var(P) -> ( nonvar(T), \+ wildcard_type_t(T)
                                         -> variable_fallthrough_type(T, Prior, PT),
                                            add_known_type(P, PT)
                                          ; true )
                            ; is_union(T), T = ['|'|Ms]        %a pattern narrows to the member it selects
                              -> ( findall(M, ( member(M, Ms), pattern_selects_member(P, M) ), [M1]),
                                   narrowing_sound(P, Ms, M1, Prior)
                                   -> bind_pattern_typed(P, M1, Prior)
                                 ; findall(M, ( member(M, Ms), pattern_selects_member_tagged(P, M) ), [M2])
                                   -> bind_pattern_typed(P, M2, Prior) ; true )
                            ; list_type(T, ET), P = [C, H, R], C == cons
                              -> bind_pattern_typed(H, ET),    %source-form (cons H R) destructuring
                                 bind_pattern_typed(R, ['List', ET])
                            ; list_type(T, ET), P = [H|Rest]
                              -> bind_pattern_typed(H, ET),
                                 bind_pattern_typed(Rest, ['List', ET])
                            ; structural_pattern_fields(P, T, Fields, FieldTs)
                              -> maplist(bind_pattern_typed, Fields, FieldTs)
                            ; atom(T), declared_newtype(T, R), \+ wildcard_type_t(R)
                              -> bind_pattern_typed(P, R, Prior)
                            ; is_list(P), is_list(T), same_length(P, T),
                              \+ is_arrow_type(T)
                              -> maplist(bind_pattern_typed, P, T)
                            ; true ).

%A variable case branch is the committed fallthrough: values wholly consumed
%by earlier branches cannot reach it.  Subtract only union members for which
%that statement has a positive, closed proof.  In particular, primitive,
%wildcard, list, arrow, newtype/open-representation and otherwise structural
%members stay in the union.  Other bind_pattern_typed/3 callers pass Prior=[],
%so their variable bindings retain the declared type unchanged.
variable_fallthrough_type(T, Prior, NT) :-
    nonempty_prior(Prior),
    is_union(T),
    T = ['|'|Ms],
    subtract_consumed_union_members(Ms, Prior, Kept, Removed),
    Removed == yes,
    Kept \== [],
    !,
    ( Kept = [Only] -> NT = Only ; NT = ['|'|Kept] ).
variable_fallthrough_type(T, _, T).

nonempty_prior([_|_]).

subtract_consumed_union_members([], _, [], no).
subtract_consumed_union_members([M|Ms], Prior, Kept, Removed) :-
    subtract_consumed_union_members(Ms, Prior, Rest, TailRemoved),
    ( fallthrough_member_consumed(M, Prior)
      -> Kept = Rest,
         Removed = yes
      ; Kept = [M|Rest],
        Removed = TailRemoved ).

%A tagged tuple denotes one exact constructor shape.  A nominal type is
%subtractable only when its current, nonempty constructor set is completely
%covered by earlier unconstrained constructor patterns.  Publishing ctor_set
%after the proof succeeds makes that closed-world snapshot visible to the
%dependency graph without retaining dependencies for members we did not
%exclude.
fallthrough_member_consumed(M, Prior) :-
    nonvar(M),
    is_list(M),
    tagged_tuple_type(M, Tag, FieldTs),
    !,
    length(FieldTs, K),
    prior_consumed_ctor(Prior, Tag, K).
fallthrough_member_consumed(M, Prior) :-
    atom(M),
    \+ wildcard_type(M),
    \+ primitive_type(M),
    \+ declared_newtype(M, _),
    findall(C-K, member_ctor(M, K, C), Keys0),
    sort(Keys0, Keys),
    Keys = [_|_],
    prior_consumed_ctor_keys(Keys, Prior),
    analysis_emit(dependency(ctor_set(M))).

prior_consumed_ctor_keys([], _).
prior_consumed_ctor_keys([Ctor-K|Keys], Prior) :-
    prior_consumed_ctor(Prior, Ctor, K),
    prior_consumed_ctor_keys(Keys, Prior).

%%% Soundness gate on union narrowing by shape.
%
%A pattern that carries TAG evidence for the member it selected - its head is
%that member's tag, or a declared constructor of it - says something real
%about the value, and narrows as it always has.
%
%Without that evidence (a variable head: ($_type ($_kbid $_ctx $_vars) $prf
%$tv), or an atom head that is nobody's constructor) the pattern selected the
%member purely by element count, which alone is unsound: another member's
%constructor may build a value of exactly that count (a Goal is
%(CPU $f $a $r), also four elements). Narrowing is then admissible only when
%every OTHER member is ruled out - see union_member_excluded/3.
narrowing_sound(P, _, M1, _) :- pattern_selects_member_tagged(P, M1), !.
narrowing_sound(P, Ms, _, Prior) :- is_list(P), length(P, N),
                                    narrowing_other_members(Ms, P, N, Prior).

%Do not express this walk with forall/2: forall is double negation, so proof
%events emitted by a successful exclusion are backtracked away.  The explicit
%recursion preserves constructor-set dependencies as returned proof data.
narrowing_other_members([], _, _, _).
narrowing_other_members([M|Ms], P, N, Prior) :-
    ( pattern_selects_member(P, M) -> true
    ; union_member_excluded(M, N, Prior) ),
    narrowing_other_members(Ms, P, N, Prior).

%union_member_excluded(+Member, +N, +PriorPatterns): no value of Member can be
%an N-element expression here. Either
%  (a) by ARITY - Member has no constructor that builds N elements, or
%  (b) because every constructor of Member that does was already consumed by an
%      EARLIER branch of the same case. case is first-match/committed
%      (translate_case compiles to nested if-then-else), so such a value can
%      never reach this branch.
%(a) reads the constructor set as it stands right now, so the verdict is a
%SNAPSHOT. The proof publishes ctor_set(Member), so a later declaration that
%changes the set invalidates and recompiles exactly the graph consumers.
union_member_excluded(M, _, _) :- var(M), !, fail.
union_member_excluded(M, _, _) :- is_arrow_type(M), !.       %a closure is not an expression
union_member_excluded(M, _, _) :- list_type(M, _), !, fail.  %(List T) admits every length
union_member_excluded(M, N, Prior) :- is_union(M), !, M = ['|'|Ms],
                                      union_members_excluded(Ms, N, Prior).
union_member_excluded(M, N, Prior) :- is_list(M), !,
        ( tagged_tuple_type(M, Tag, FieldTs)
          -> ( length(M, N) -> length(FieldTs, K), prior_consumed_ctor(Prior, Tag, K) ; true )
           ; length(M, LM), LM =\= N ).      %positional member of a different width
union_member_excluded(M, N, Prior) :- atom(M), !,
        ( wildcard_type(M) -> fail           %Atom/Expression admit anything
        ; declared_newtype(M, R) -> union_member_excluded(R, N, Prior)
        ; primitive_type(M) -> true          %an expression is not a Number/String/Bool
        ; N =:= 0 -> true                    %() is no constructor application
        ; K is N - 1,
          analysis_emit(dependency(ctor_set(M))),
          forall(member_ctor(M, K, C), prior_consumed_ctor(Prior, C, K)) ).
union_member_excluded(_, _, _) :- fail.

union_members_excluded([], _, _).
union_members_excluded([M|Ms], N, Prior) :-
    union_member_excluded(M, N, Prior),
    union_members_excluded(Ms, N, Prior).

%A CONSTRUCTOR of the nominal type M taking K arguments, so its applications
%have K+1 elements. A declared symbol with NO equations stays literal data,
%while one with equations is always
%rewritten at the call site and never survives as a value. So a declaration
%alone is not enough - \+ fun(C) is what makes C data. Counting reducible
%helpers here would only ever BLOCK an exclusion, never grant a wrong one, but
%it blocks far too much: any (= (make-goal $f $a $r) (CPU $f $a $r)) would
%stop CPU/3 from being Goal's only constructor. A wildcard output claims
%nothing, so it does not block either.
%The definedness flag is set early enough: parse_form/2 (filereader.pl)
%register_fun's every (= (F ...) ...) of a file in the parse prepass, before
%any clause of that file is compiled, so definition-below-use is fine. A
%definition arriving from a LATER file invalidates the compiled clause's
%symbol/effect dependency and the graph revisits it.
member_ctor(M, K, C) :- declared_fn_type(C, ATs, OT, _), length(ATs, K),
                        \+ fun(C),
                        nonvar(OT), \+ wildcard_type_t(OT), type_compat_soft(OT, M).

%An earlier branch consumed EVERY (Ctor V1 ... Vk) value: its pattern is
%headed by Ctor at that arity and its arguments are distinct variables, so the
%match cannot fail. A pattern like (CPU foo $a $r) constrains a field and
%consumes nothing.
prior_consumed_ctor(Prior, Ctor, K) :-
    member(P0, Prior),
    nonvar(P0),
    is_list(P0),
    P0 = [H|As],
    H == Ctor,
    length(As, K),
    maplist(var, As),
    sort(As, Distinct),
    length(Distinct, K),
    !.

%Check the clause body's inferred output type against the declared output type:
clause_output_goals(_, none, _, _, _, []) :- !.
clause_output_goals(F, out(OT, ATs), Args, ExpOut, BodyExpr, Gs) :-
        ( var(OT) -> ( term_variables(ATs, Vs), \+ memberchk_eq(OT, Vs)
                       -> parametric_output_check(F, ExpOut) ; true ),
                     Gs = []
        ; wildcard_type_t(OT) -> Gs = []
        ; nonvar(BodyExpr), BodyExpr = [Q, QV], Q == quote, \+ atomic(QV)
          -> with_quoted_declared_params(
                 Args, ATs,
                 quoted_compound_output_status(QV, OT, QuoteStatus)),
             ( QuoteStatus == ok
               -> Gs = []
             ; QuoteStatus == unknown
               -> type_guard(F, ExpOut, OT, Gs)
             ; quoted_structural_type(QV, Structural),
               throw(error(type_conflict(existing(Structural), required(OT)),
                           typecheck)) )
        ; var(ExpOut) ->
            ( known_candidates(ExpOut, Cs) ->
                ( member(C, Cs), output_candidate_conflict(C, OT, Bad)
                  -> throw(error(type_conflict(existing(Bad), required(OT)), typecheck))
                ; member(C, Cs), \+ output_candidate_fits(C, OT)
                  -> type_guard(F, ExpOut, OT, Gs)
                   ; Gs = [] )
            ; type_guard(F, ExpOut, OT, Gs) )
        ; check_value(ExpOut, OT, St),
          ( St == mismatch -> throw(error(literal_type_mismatch(ExpOut, OT), typecheck))
          ; St == unknown -> type_guard(F, ExpOut, OT, Gs)
          ; Gs = [] ) ).

%A quoted compound is unevaluated, but its runtime value still has structural
%shape. Check that shape directly instead of asking value_candidate_types/2,
%which would interpret an atom-headed term such as (+ 1 2) as a CALL and
%inherit Number from +. Expression remains the unconstrained supertype.
quoted_compound_output_status(Value, Required, Status) :-
    quoted_structural_value_status(Value, Required, Status).

quoted_structural_value_status(Value, T, Status) :-
    var(Value), !,
    ( ( known_singleton(Value, Known)
      ; quoted_declared_var_type(Value, Known) )
      -> ( type_compat_soft(Known, T) -> Status = ok
         ; Status = mismatch )
    ; Status = unknown ).
quoted_structural_value_status(_, T, ok) :-
    wildcard_type_t(T), !.
quoted_structural_value_status(Value, T, Status) :-
    is_union(T), !, T = ['|'|Members],
    quoted_union_status(Value, Members, Status).
quoted_structural_value_status(Value, T, Status) :-
    tagged_tuple_type(T, Tag, FieldTs), !,
    ( is_list(Value), Value = [ValueTag|Fields],
      ValueTag == Tag, same_length(Fields, FieldTs)
      -> quoted_fields_status(Fields, FieldTs, Status)
    ; Status = mismatch ).
quoted_structural_value_status(Value, T, Status) :-
    contextual_product_type(T), !,
    ( is_list(Value), same_length(Value, T)
      -> quoted_fields_status(Value, T, Status)
    ; Status = mismatch ).
quoted_structural_value_status(Value, T, Status) :-
    atom(T), declared_newtype(T, Representation), !,
    quoted_structural_value_status(Value, Representation, Status).
quoted_structural_value_status(Value, T, Status) :-
    atom(T), \+ primitive_type(T), \+ wildcard_type_t(T),
    structural_pattern_fields(Value, T, Fields, FieldTs), !,
    quoted_fields_status(Fields, FieldTs, Status).
quoted_structural_value_status(Value, T, Status) :-
    ( var(Value) -> elem_status(Value, T, Status)
    ; atomic(Value) -> check_value(Value, T, Status)
    ; Status = mismatch ).

quoted_union_status(_, [], mismatch).
quoted_union_status(Value, [Member|Members], Status) :-
    quoted_structural_value_status(Value, Member, First),
    ( First == ok -> Status = ok
    ; quoted_union_status(Value, Members, Rest),
      ( Rest == ok -> Status = ok
      ; First == unknown -> Status = unknown
      ; Status = Rest ) ).

quoted_fields_status([], [], ok).
quoted_fields_status([Value|Values], [Type|Types], Status) :-
    quoted_structural_value_status(Value, Type, First),
    ( First == mismatch -> Status = mismatch
    ; quoted_fields_status(Values, Types, Rest),
      ( Rest == mismatch -> Status = mismatch
      ; First == unknown -> Status = unknown
      ; Status = Rest ) ).

%Diagnostic-only structural type. Unknown fields stay visibly unknown rather
%than becoming unification variables that could look like positive evidence.
quoted_structural_type(Value, Type) :-
    var(Value), !,
    ( known_singleton(Value, Known) -> Type = Known
    ; Type = '$unknown' ).
quoted_structural_type(Value, 'Number') :- number(Value), !.
quoted_structural_type(Value, 'String') :- string(Value), !.
quoted_structural_type(true, 'Bool') :- !.
quoted_structural_type(false, 'Bool') :- !.
quoted_structural_type(Value, Type) :-
    atom(Value), !,
    findall(T, declared_value_type(Value, T), Types),
    ( Types = [Only] -> Type = Only ; Type = 'Atom' ).
quoted_structural_type(Value, Type) :-
    is_list(Value), !,
    maplist(quoted_structural_type, Value, Type).
quoted_structural_type(_, 'Expression').

%Atom/Expression parameters intentionally carry no tknown attribute because
%they are checker wildcards. Inside a quote, however, the declaration is
%positive structural information: the runtime slot contains that parameter
%value literally. Publish the declaration mapping only for this output check,
%without changing the variables' ordinary checker attributes.
with_quoted_declared_params(Args, Types, Goal) :-
    quoted_param_pairs(Args, Types, Pairs),
    ( catch(b_getval('$quoted_declared_params', Saved), _, fail) -> true
    ; Saved = [] ),
    setup_call_cleanup(
        b_setval('$quoted_declared_params', Pairs),
        Goal,
        b_setval('$quoted_declared_params', Saved)).

quoted_declared_var_type(Value, Type) :-
    catch(b_getval('$quoted_declared_params', Pairs), _, fail),
    member(param(Param, Type), Pairs),
    Param == Value, !.

quoted_param_pairs([], [], []).
quoted_param_pairs([Arg|Args], [Type|Types], Pairs) :-
    quoted_pattern_param_pairs(Arg, Type, Here),
    quoted_param_pairs(Args, Types, Rest),
    append(Here, Rest, Pairs).

quoted_pattern_param_pairs(Arg, Type, [param(Arg, Type)]) :-
    var(Arg), !.
quoted_pattern_param_pairs(Arg, Type, Pairs) :-
    functional_pattern_application(Arg, _, _), !,
    ( functional_pattern_signature(Arg, Type, PatternArgs, ArgTypes)
      -> quoted_param_pairs(PatternArgs, ArgTypes, Pairs)
    ; Pairs = [] ).
quoted_pattern_param_pairs(Arg, Type, Pairs) :-
    list_type(Type, ET), transformed_cons_pattern(Arg, Head, Tail), !,
    quoted_pattern_param_pairs(Head, ET, A),
    quoted_pattern_param_pairs(Tail, ['List', ET], B),
    append(A, B, Pairs).
quoted_pattern_param_pairs(Arg, Type, Pairs) :-
    structural_pattern_fields(Arg, Type, Fields, FieldTypes), !,
    quoted_param_pairs(Fields, FieldTypes, Pairs).
quoted_pattern_param_pairs(Arg, Type, Pairs) :-
    is_list(Arg), is_list(Type), same_length(Arg, Type),
    \+ is_arrow_type(Type), !,
    quoted_param_pairs(Arg, Type, Pairs).
quoted_pattern_param_pairs(_, _, []).

output_candidate_fits(C, OT) :-
    candidate_evidence(C, literal(V)), !,
    check_value(V, OT, ok).
output_candidate_fits(C, OT) :-
    \+ indefinite_candidate(C),
    ( type_compat_soft(C, OT) ; refinement_pair(C, OT) ).

output_candidate_conflict(C, OT, V) :-
    candidate_evidence(C, literal(V)), !,
    check_value(V, OT, mismatch).
output_candidate_conflict(C, OT, C) :-
    \+ indefinite_candidate(C),
    \+ type_compat_soft(C, OT),
    \+ refinement_pair(C, OT).
