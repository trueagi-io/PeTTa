%%% Declaration storage and lifecycle. All declaration mutation enters through
%%% maybe_cache_type_decl/2, maybe_uncache_type_decl/2 or forget_symbol_types/1.

:- thread_local loading_origin/1.
:- thread_local declaration_provenance/1.
:- dynamic fn_decl/6.
:- dynamic nonfn_decl_origin/2.
:- dynamic declared_value_type/2.   % declared_value_type(Name, Type)
:- dynamic declared_newtype/2.      % declared_newtype(Name, Representation)
:- dynamic declared_type_alias/2.   % declared_type_alias(Name, Representation)
:- dynamic declared_foreign_type/2. % declared_foreign_type(Name, Arity)
:- dynamic declared_space_type/2.   % declared_space_type(Name, RowType)

%%% Function-declaration record:
%
% fn_decl(F, Arity, scheme(ArgTypes, OutType), Effect, Origin, Provenance)
%
% Effect = effect_model(Top, Variables)
%   Top       = det | semidet | nondet | unspecified | variable(Name)
%   Variables = [] | [effect_var(Name, [closure_arg(Index, Arity), ...])]
%
% Provenance keeps the source location and original syntax, so late alias
% addition/removal can renormalize this store in place.

fn_decl_copy(F, N, Scheme, Effect, Origin, Provenance) :-
    fn_decl(F, N, S0, E0, Origin, P0),
    Stored = S0-E0-P0,
    ( ground(Stored)
      -> Scheme-Effect-Provenance = Stored
      ; copy_term(Stored, Scheme-Effect-Provenance) ).

declared_fn_type(F, ATs, OT, Det) :-
    fn_decl_copy(F, _, scheme(ATs, OT), Effect, _, _),
    effect_model_det(Effect, Det).

explicit_det_decl(F, N) :-
    fn_decl(F, N, _, effect_model(det, _), _, _).

explicit_committed_decl(F, N, Det) :-
    fn_decl(F, N, _, effect_model(Det, _), _, _),
    committed_det(Det).

trusted_library_decl(F) :-
    fn_decl(F, _, _, _, library(_), _), !.

effect_model_det(effect_model(variable(Name), _), effect(Name)).
effect_model_det(effect_model(Det, _), Det) :- Det \= variable(_).

canonical_effect_model(Det, ATs, effect_model(Top, Variables)) :-
    ( Det = effect(Name) -> Top = variable(Name) ; Top = Det ),
    findall(Name-closure_arg(Index, Arity),
            ( nth0(Index, ATs, T), nonvar(T), is_arrow_type(T),
              T = [Arrow|Rest], effect_arrow_atom(Arrow, Name),
              length(Rest, Len), Arity is Len - 1 ),
            Pairs),
    findall(Name, member(Name-_, Pairs), Names0),
    sort(Names0, Names),
    ( Names == []
      -> Variables = []
    ; Names = [Name],
      findall(Pos, member(Name-Pos, Pairs), Positions),
      Variables = [effect_var(Name, Positions)] ).

current_fn_decl_provenance(Type, provenance(Location, syntax(Type))) :-
    ( declaration_provenance(Location) -> true
    ; current_metta_file(File), File \== '<string>'
      -> Location = source(File, unknown)
    ; Location = unknown ).

%The only two predicates that write the function-declaration store:
add_fn_decl_record(fn_decl(F, N, Scheme, Effect, Origin, Provenance), Added) :-
    ( fn_decl(F, N, S2, E2, _, _), (S2-E2) =@= (Scheme-Effect)
      -> Added = false
    ; assertz(fn_decl(F, N, Scheme, Effect, Origin, Provenance)),
      Added = true ).

remove_fn_decl_record(F, N, Scheme, Effect, Removed) :-
    clause(fn_decl(F, N, S2, E2, Origin, Provenance), true, Ref),
    (S2-E2) =@= (Scheme-Effect), !,
    erase(Ref),
    Removed = fn_decl(F, N, S2, E2, Origin, Provenance).

replace_fn_decl_record(Old, New) :-
    Old = fn_decl(F, N, Scheme, Effect, _, _),
    remove_fn_decl_record(F, N, Scheme, Effect, _),
    add_fn_decl_record(New, _).

remove_all_fn_decl_records(F) :-
    findall(key(N, Scheme, Effect),
            fn_decl_copy(F, N, Scheme, Effect, _, _),
            Keys),
    forall(member(key(N, Scheme, Effect), Keys),
           remove_fn_decl_record(F, N, Scheme, Effect, _)).

%A curated library load is a dynamic scope, not a sticky global mode. Nested
%plain-file imports inherit the surrounding library origin; a nested curated
%import temporarily records its own library and restores the outer one.
with_library_origin(Name, Goal) :-
    setup_call_cleanup(asserta(loading_origin(library(Name)), Ref),
                       Goal,
                       erase(Ref)).

%%% Store maintenance, called from add_sexp/remove_sexp and forget_symbol.
%%% Caching is idempotent so seeded builtins and imports do not duplicate:
%A function type declared for a parenthesized name - (: (/?\) (-> ...)) - is a
%malformed declaration that would otherwise be ignored silently:
maybe_cache_type_decl(Space, Term) :- Space == '&self', is_list(Term), Term = [C, [Name], Type],
                                      C == (:), atom(Name),
                                      nonvar(Type), fn_type_shape(Type, _, _, _), !,
                                      format(user_error,
                                             "Warning: type declaration name (~w) is an expression; write (: ~w ...) to declare the function~n",
                                             [Name, Name]).
maybe_cache_type_decl(Space, Term) :- Space == '&self', is_list(Term), Term = [C, Name, Type],
                                      C == (:), atom(Name), kind_decl(Type, Keyword, Value), !,
                                      with_decl_transaction(Name,
                                          cache_kind_decl(Name, Type, Keyword, Value)).
maybe_cache_type_decl(Space, Term) :- ( Space == '&self', is_list(Term), Term = [C, Name, Type],
                                        C == (:), atom(Name)
                                        -> ( nonvar(Type), infix_arrow_misuse(Type)
                                             -> throw(error(infix_arrow_syntax(Name, Type), typecheck))
                                           ; nonvar(Type), fn_type_shape(Type, ATs, OT, Det)
                                              -> with_decl_transaction(
                                                     Name,
                                                     ( prepare_decl_origin(Name, Type, Origin),
                                                       current_fn_decl_provenance(Type, Provenance),
                                                       cache_fn_type_decl(Name, Type, ATs, OT, Det,
                                                                          Origin, Provenance) ))
                                              ; with_decl_transaction(
                                                    Name,
                                                    cache_value_decl(Name, Type)) )
                                        ; true ).

%%% Kind declarations (: Name (Keyword ...)), each kind in its own store keyed
%%% by name. Erased nominal newtypes (: KB (Newtype Expression)) declare a
%%% distinct compile-time role over a representation; nothing exists at
%%% runtime. Structural aliases (: Row (Alias (Number String))) name an erased
%%% type expression. Opaque foreign types (: Heap (Foreign)) and
%%% (: Heap (Foreign 1)) enter only a nominal name and arity; native values stay
%%% opaque. Typed spaces (: &jobs (SpaceOf Row)) opt one statically named space
%%% into row checking. Representations are normalized when cached, so aliases
%%% are erased and later lookup is one non-recursive expansion. Kind
%%% declarations are source-ordered, not hoisted by precache_fn_type_decl/2;
%%% declare-before-use stays cheapest, while a fresh late alias repairs prior
%%% declarations.
%Keyword, notification kind, store, conflict-warning noun, "already" phrase:
decl_kind('Newtype', newtype, declared_newtype,      'Newtype',     'a Newtype').
decl_kind('Alias',   alias,   declared_type_alias,   'type alias',  'an Alias').
decl_kind('Foreign', foreign, declared_foreign_type, 'foreign type', 'a Foreign type').
decl_kind('SpaceOf', space,   declared_space_type,   'space type',  'a SpaceOf type').

%Every per-name type store with its notification kind:
type_store(value, declared_value_type).
type_store(Kind, Store) :- decl_kind(_, Kind, Store, _, _).

%Type is a well-formed kind declaration; Value is what its store holds:
kind_decl(Type, Keyword, Value) :-
    is_list(Type), Type = [Keyword|Spec], atom(Keyword),
    decl_kind(Keyword, _, _, _, _),
    ( Keyword == 'Foreign'
      -> ( Spec == [] -> Value = 0 ; Spec = [Value], integer(Value), Value > 0 )
    ; Spec = [R], normalize_type(R, Value) ).

kind_decl_clause(Name, Keyword, Value, Ref) :-
    decl_kind(Keyword, _, Store, _, _),
    Entry =.. [Store, Name, Stored],
    clause(Entry, true, Ref),
    Stored =@= Value.

cache_kind_decl(Name, Type, Keyword, Value) :-
    prepare_decl_origin(Name, Type, _),
    decl_kind(Keyword, Kind, Store, Noun, _),
    Entry =.. [Store, Name, Prior],
    ( call(Entry)
      -> ( Prior =@= Value -> true
         ; ( Keyword == 'Foreign' -> What = 'arity ' ; What = '' ),
           format(user_error,
                  "Warning: conflicting ~w declaration for ~w ignored: ~w~p differs from ~p~n",
                  [Noun, Name, What, Value, Prior]) )
    ; decl_kind(Other, _, OtherStore, _, Already), Other \== Keyword,
      OtherEntry =.. [OtherStore, Name, _], call(OtherEntry)
      -> format(user_error,
                "Warning: ~w declaration for ~w ignored: name is already ~w~n",
                [Noun, Name, Already])
    ; ( Keyword == 'SpaceOf' -> validate_existing_space_rows(Name, Value) ; true ),
      New =.. [Store, Name, Value],
      assertz(New),
      ( Keyword == 'Alias' -> renormalize_late_alias(Name, _) ; true ),
      decl_notify(declaration_changed(Kind, Name, added)) ).

cache_value_decl(Name, Type) :-
    prepare_decl_origin(Name, Type, Origin),
    ground(Origin),
    normalize_type(Type, TN),
    ( declared_value_type(Name, T2), T2 =@= TN -> true
    ; assertz(declared_value_type(Name, TN)),
      decl_notify(declaration_changed(value, Name, added)),
      notify_symbol_constructor_sets(Name) ).

%A SpaceOf declaration is a checked schema for the rows already in the named
%space. As at update time, open fields are accepted and only a definite
%contradiction rejects. The declaration transaction removes any staged origin
%or store state if this validation throws.
validate_existing_space_rows(Name, Schema) :-
    forall(existing_space_row(Name, Row),
           ( value_definitely_mismatch(Row, Schema)
             -> throw(error(space_schema_row_mismatch(Name, Row, Schema),
                            typecheck))
           ; true )).

existing_space_row(Name, Row) :-
    current_predicate(Name/Arity),
    functor(Head, Name, Arity),
    catch(clause(Head, true), _, fail),
    Head =.. [Name|Row].

%A declaration-driven graph revalidation is transactional over the declaration
%state: executable clauses, declarations, origins and inferred types are
%restored together. The raw source atom of a runtime add-atom stays outside it.
with_decl_transaction(Name, Goal) :-
    snapshot_decl_transaction(Name, Snapshot),
    catch(( Goal -> true
          ; restore_decl_transaction(Name, Snapshot),
            fail ),
          Error,
          ( restore_decl_transaction(Name, Snapshot),
            throw(Error) )).

snapshot_decl_transaction(Name,
        decl_transaction(Records, Origins, Inferred, Stores)) :-
    findall(fn_decl(Name, N, Scheme, Effect, Origin, Provenance),
            fn_decl_copy(Name, N, Scheme, Effect, Origin, Provenance),
            Records),
    findall(Origin, nonfn_decl_origin(Name, Origin), Origins),
    findall(inferred(ATs, OT), inferred_fn_type(Name, ATs, OT), Inferred),
    findall(Store-Ts,
            ( type_store(_, Store),
              Entry =.. [Store, Name, T],
              findall(T, Entry, Ts) ),
            Stores).

restore_decl_transaction(Name,
        decl_transaction(Records, Origins, Inferred, Stores)) :-
    findall(N, fn_decl(Name, N, _, _, _, _), CurrentArities0),
    findall(N, member(fn_decl(Name, N, _, _, _, _), Records), PriorArities0),
    append(CurrentArities0, PriorArities0, Arities0),
    sort(Arities0, Arities),
    with_decl_notifications_suppressed(
        ( remove_all_fn_decl_records(Name),
          forall(member(Record, Records), add_fn_decl_record(Record, _)),
          retractall(nonfn_decl_origin(Name, _)),
          forall(member(Origin, Origins),
                 assertz(nonfn_decl_origin(Name, Origin))),
          retractall(inferred_fn_type(Name, _, _)),
          forall(member(inferred(ATs, OT), Inferred),
                 assertz(inferred_fn_type(Name, ATs, OT))),
          forall(member(Store-Ts, Stores),
                 ( Any =.. [Store, Name, _],
                   retractall(Any),
                   forall(member(T, Ts),
                          ( Entry =.. [Store, Name, T], assertz(Entry) )) )) )),
    %Consumers may have reacted to the origin flip before the staged
    %declaration failed: re-run those edges under the restored state, keeping
    %the original validation error.
    catch(decl_notify(declaration_changed(origin, Name, changed)), _, true),
    forall(member(N, Arities),
           catch(decl_notify(declaration_changed(Name/N, changed)), _, true)),
    forall(type_store(Kind, _),
           catch(decl_notify(declaration_changed(Kind, Name, changed)), _, true)).

cache_fn_type_decl(Name, Type, ATs, OT, Det, Origin, Provenance) :-
    validate_effect_variable_decl(Name, Type),
    require_explicit_det_arrows(Name, Type),
    maplist(normalize_type, ATs, ATN),
    normalize_type(OT, OTN),
    remove_unexpanded_fn_precache(Name, ATs, OT, Det, ATN, OTN),
    canonical_effect_model(Det, ATN, Effect),
    length(ATN, N),
    retractall(inferred_fn_type(Name, _, _)),  %declaration supersedes inference
    add_fn_decl_record(
        fn_decl(Name, N, scheme(ATN, OTN), Effect, Origin, Provenance),
        Added),
    ( Added == true
      -> decl_notify(declaration_changed(Name/N, added)),
         notify_symbol_constructor_sets(Name)
    ; true ).

decl_notify(_) :-
    catch(b_getval('$suppress_decl_notifications', true), _, fail), !.
decl_notify(Event) :-
    nb_current('$batched_decl_notifications', Events0), !,
    nb_setval('$batched_decl_notifications', [Event|Events0]).
decl_notify(Event) :-
    notify_mutation(Event).

:- meta_predicate with_decl_notifications_batched(0).

% Function declarations are hoisted as one file-level prepass, so consumers are
% rebuilt once against the final set, not once per prefix. Nested batching
% feeds the outer queue.
with_decl_notifications_batched(Goal) :-
    ( nb_current('$batched_decl_notifications', _)
      -> call(Goal)
    ; setup_call_cleanup(
          nb_setval('$batched_decl_notifications', []),
          call(Goal),
          flush_decl_notification_batch) ).

flush_decl_notification_batch :-
    nb_getval('$batched_decl_notifications', Reversed),
    nb_delete('$batched_decl_notifications'),
    reverse(Reversed, Ordered),
    list_to_set(Ordered, Events),
    notify_mutations(Events).

with_decl_notifications_suppressed(Goal) :-
    catch(b_getval('$suppress_decl_notifications', Saved), _, Saved = false),
    setup_call_cleanup(b_setval('$suppress_decl_notifications', true),
                       Goal,
                       b_setval('$suppress_decl_notifications', Saved)).

notify_symbol_constructor_sets(Name) :-
    current_predicate(fun/1), !,
    recorded_constructor_dependency_types(Candidates),
    include(symbol_enters_constructor_set(Name), Candidates, Ts),
    forall(member(T, Ts),
           decl_notify(constructor_set_changed(T, Name))).
notify_symbol_constructor_sets(_).

%Name is a constructor or constant of T, so declaring or removing it changes
%the ctor_set(T) the dependency graph tracks:
symbol_enters_constructor_set(Name, T) :-
    ( member_ctor(T, _, Name) -> true
    ; declared_value_type(Name, T), \+ fun(Name) ).

%Origin is deliberately symbol-level: one user declaration opts the whole
%callable back into conservative guards, including all of its overloads. A
%library may mark a symbol only when it is introducing it (or continuing a
%library-origin declaration); importing a library after a user declaration
%must not silently turn that user symbol into trusted code.
prepare_decl_origin(Name, Type, Origin) :-
    ( loading_origin(LoadOrigin)
      -> ( symbol_library_origin(Name, Existing)
           -> Origin = Existing
         ; cached_symbol_declaration(Name)
           -> Origin = user
         ; Origin = LoadOrigin,
           note_nonfn_library_origin(Name, Type, Origin) )
    ; symbol_library_origin(Name, library(Library))
      -> warn_user_library_redeclaration(Name, Type, Library),
         mark_symbol_origin_user(Name),
         Origin = user
    ; Origin = user ).

symbol_library_origin(Name, library(Library)) :-
    fn_decl(Name, _, _, _, library(Library), _), !.
symbol_library_origin(Name, library(Library)) :-
    nonfn_decl_origin(Name, library(Library)), !.

note_nonfn_library_origin(_, Type, _) :-
    nonvar(Type), fn_type_shape(Type, _, _, _), !.
note_nonfn_library_origin(Name, _, Origin) :-
    ( nonfn_decl_origin(Name, _) -> true
    ; assertz(nonfn_decl_origin(Name, Origin)) ).

mark_symbol_origin_user(Name) :-
    findall(fn_decl(Name, N, Scheme, Effect, Origin, Provenance),
            fn_decl_copy(Name, N, Scheme, Effect, Origin, Provenance),
            Records),
    forall(member(Old, Records),
           ( Old = fn_decl(Name, N, Scheme, Effect, _, Provenance),
             New = fn_decl(Name, N, Scheme, Effect, user, Provenance),
             replace_fn_decl_record(Old, New) )),
    retractall(nonfn_decl_origin(Name, _)),
    decl_notify(declaration_changed(origin, Name, changed)).

cached_symbol_declaration(Name) :- fn_decl(Name, _, _, _, _, _), !.
cached_symbol_declaration(Name) :- type_store(_, Store),
                                   Entry =.. [Store, Name, _],
                                   call(Entry), !.

warn_user_library_redeclaration(Name, Type, Library) :-
    ( cached_declaration_matches(Name, Type)
      -> true
    ; library_decl_location(Name, Location),
      format(user_error,
             "Warning: user declaration for ~w differs from trusted library ~w declaration~w: ~p~n",
             [Name, Library, Location, Type]) ).

library_decl_location(Name, Text) :-
    ( once(fn_decl(Name, _, _, _, library(_), provenance(Location, _))),
      Location = source(File, Line)
      -> format(atom(Text), " at ~w:~w", [File, Line])
    ; Text = '' ).

cached_declaration_matches(Name, Type) :-
    nonvar(Type), fn_type_shape(Type, ATs, OT, Det), !,
    maplist(normalize_type, ATs, ATN),
    normalize_type(OT, OTN),
    declared_fn_type(Name, A2, O2, D2),
    (A2-O2-D2) =@= (ATN-OTN-Det).
cached_declaration_matches(Name, Type) :- kind_decl(Type, Keyword, Value), !,
    kind_decl_clause(Name, Keyword, Value, _).
cached_declaration_matches(Name, Type) :-
    normalize_type(Type, TN),
    declared_value_type(Name, T2),
    T2 =@= TN.

%Bounded v1 effect-variable syntax. The only legal occurrences are the
%top-level arrow head and direct arrow-typed parameter heads. Names are atoms
%extracted from -[$name]->, so declaration copies never share a logic binding.
validate_effect_variable_decl(Name, Type) :-
    normalize_type(Type, Normalized),
    findall(V, effect_var_occurrence(Normalized, V), Vs0),
    sort(Vs0, Vs),
    ( Vs = []
      -> true
    ; Vs = [V]
      -> validate_effect_variable_positions(Name, Normalized, V)
    ; throw(error(effect_variable_multiple(Name, Vs), determinism)) ).

effect_var_occurrence(T, V) :-
    nonvar(T), is_list(T), T = [H|Rest],
    ( effect_arrow_atom(H, V)
    ; member(E, Rest), effect_var_occurrence(E, V) ).

validate_effect_variable_positions(Name, Type, V) :-
    fn_type_shape(Type, ATs, OT, TopDet),
    ( member(A, ATs), forbidden_effect_parameter_occurrence(A)
      -> throw(error(effect_variable_bad_position(Name, V), determinism))
    ; effect_var_occurrence(OT, _)
      -> throw(error(effect_variable_bad_position(Name, V), determinism))
    ; TopDet = effect(V), \+ direct_effect_parameter(ATs, V)
      -> throw(error(effect_variable_uninstantiable(Name, V), determinism))
    ; true ).

direct_effect_parameter(ATs, V) :-
    member(T, ATs), nonvar(T), is_arrow_type(T),
    T = [H|_], effect_arrow_atom(H, V), !.

forbidden_effect_parameter_occurrence(T) :-
    ( nonvar(T), is_arrow_type(T), T = [H|Rest], effect_arrow_atom(H, _)
      -> member(E, Rest), effect_var_occurrence(E, _)
    ; effect_var_occurrence(T, _) ).

%Under --strict-det every arrow in a function declaration, including nested
%and alias-expanded ones, must name its cardinality. Builtin signatures are
%exempt: builtin determinism comes from the registry's cardinality column.
require_explicit_det_arrows(Name, Type) :-
    ( strict_det(true), \+ nb_current('$builtin_signatures', true),
      normalize_type(Type, Normalized),
      type_contains_plain_arrow(Normalized)
      -> throw(error(strict_det_plain_arrow(Name), determinism))
    ; true ).

type_contains_plain_arrow(T) :-
    nonvar(T), is_list(T),
    ( T = [H|_], H == (->)
    ; member(E, T), type_contains_plain_arrow(E) ), !.

%Arrow declarations are pre-cached before source-ordered aliases exist; when
%the declaration is processed in place and an alias expanded, remove the stale
%syntax-only prepass copy:
remove_unexpanded_fn_precache(Name, ATs, OT, Det, ATN, OTN) :-
    maplist(normalize_type(syntax), ATs, RawATs),
    normalize_type(syntax, OT, RawOT),
    ( (RawATs-RawOT) =@= (ATN-OTN)
      -> true
    ; canonical_effect_model(Det, RawATs, RawEffect),
      length(RawATs, N),
      remove_fn_decl_record(Name, N, scheme(RawATs, RawOT), RawEffect, _)
      -> true
    ; true ).

%%% A fresh alias may arrive after declarations cached its name as an opaque
%%% atom: rebuild, in source order, every store containing that atom. Only
%%% functions whose arrow entries changed are recompiled.
renormalize_late_alias(Name, Fs) :-
    self_type_declarations(All),
    dependent_type_names(All, [Name], Names),
    include(declaration_mentions_any(Names), All, Rebuilt),
    renormalize_alias_fn_decls(Name, Fs),
    renormalize_alias_store(Name, declared_value_type),
    renormalize_alias_store(Name, declared_space_type),
    renormalize_alias_alias_decls(Name),
    renormalize_alias_store(Name, declared_newtype),
    notify_rebuilt_declarations(Rebuilt).

type_term_mentions_alias(T, Name) :- sub_term(S, T), S == Name, !.

renormalize_alias_fn_decls(Name, Fs) :-
    findall(fn_decl(F, N, Scheme, Effect, Origin, Provenance),
            fn_decl_copy(F, N, Scheme, Effect, Origin, Provenance),
            Ds),
    findall(F, ( member(fn_decl(F, _, scheme(ATs, OT), _, _, _), Ds),
                 type_term_mentions_alias(ATs-OT, Name) ), Fs0),
    sort(Fs0, Fs),
    forall(member(D, Ds), reassert_alias_fn_decl(Name, D)).

reassert_alias_fn_decl(Name, Old) :-
    Old = fn_decl(F, _, scheme(ATs, OT), _, Origin,
                  provenance(Location, syntax(Type))),
    ( type_term_mentions_alias(ATs-OT, Name)
      -> fn_type_shape(Type, RawATs, RawOT, Det),
         maplist(normalize_type, RawATs, ATN),
         normalize_type(RawOT, OTN),
         canonical_effect_model(Det, ATN, Effect),
         length(ATN, N),
         New = fn_decl(F, N, scheme(ATN, OTN), Effect, Origin,
                       provenance(Location, syntax(Type))),
         replace_fn_decl_record(Old, New)
    ; true ).

%Renormalize the Name(Key, Type) store entries that mention the alias Name:
renormalize_alias_store(Name, Store) :-
    Entry =.. [Store, K, T],
    findall(K-T, Entry, Ds),
    ( member(_-T0, Ds), type_term_mentions_alias(T0, Name)
      -> Any =.. [Store, _, _],
         retractall(Any),
         forall(member(K1-T1, Ds),
                ( ( type_term_mentions_alias(T1, Name) -> normalize_type(T1, TN) ; TN = T1 ),
                  Existing =.. [Store, K1, T2],
                  ( call(Existing), T2 =@= TN -> true
                  ; New =.. [Store, K1, TN], assertz(New) ) ))
    ; true ).

renormalize_alias_alias_decls(Name) :-
    findall(alias(A, T), declared_type_alias(A, T), Ds),
    ( member(alias(A0, T0), Ds), A0 \== Name, type_term_mentions_alias(T0, Name)
      -> retractall(declared_type_alias(_, _)),
         forall(member(alias(A, T), Ds),
                ( ( A \== Name, type_term_mentions_alias(T, Name)
                    -> normalize_type(T, TN) ; TN = T ),
                  assertz(declared_type_alias(A, TN)) ))
    ; true ).

%Declaration prepass: only function (arrow) declarations are hoisted, so
%definitions may call helpers declared later in the same file. Value
%declarations stay order-sensitive - they are knowledge atoms whose position
%is meaningful (see examples/types_nondet.metta):
precache_fn_type_decl(Space, Term) :- ( is_list(Term), Term = [C, Name, Type],
                                        C == (:), atom(Name), nonvar(Type),
                                        fn_type_shape(Type, _, _, _)
                                        -> maybe_cache_type_decl(Space, Term)
                                         ; true ).

%%% Builtin signatures are the typing column of builtin_spec/6. They seed the
%%% store once after loading, and lib_builtin_types adds them to a space as
%%% (: Name Type) atoms for get-type and match.
builtin_type_declaration([:, F, [Arrow|Types]]) :-
    builtin_signature(F, _, Det, ArgTypes, OutType),
    once(arrow_det(Arrow, Det)),
    append(ArgTypes, [OutType], Types).

with_builtin_signatures(Goal) :-
    setup_call_cleanup(nb_setval('$builtin_signatures', true),
                       forall(builtin_type_declaration(Decl), call(Goal, Decl)),
                       nb_delete('$builtin_signatures')).

seed_builtin_types :- with_builtin_signatures(maybe_cache_type_decl('&self')).

add_builtin_signatures(Space, true) :-
    with_builtin_signatures([Decl]>>'add-atom'(Space, Decl, true)).

maybe_uncache_type_decl(Space, Term) :-
    ( Space == '&self', is_list(Term), Term = [C, Name, Type],
      C == (:), atom(Name)
      -> uncache_type_decl(Name, Type)
    ; true ).

%Removal is the inverse of caching: erase the exact entry, rebuild any stores
%whose normalization depended on a removed alias, recompute explicit-arrow
%metadata from the declarations that remain in &self, and recompile affected
%clauses so cuts and guards match the surviving declarations.
uncache_type_decl(Name, Type) :- kind_decl(Type, Keyword, Value), !,
    ( kind_decl_clause(Name, Keyword, Value, Ref)
      -> ( Keyword == 'Alias'
           -> alias_removal_rebuild(Name, Ref)
         ; decl_kind(Keyword, Kind, _, _, _),
           erase(Ref),
           retractall(nonfn_decl_origin(Name, _)),
           decl_notify(declaration_changed(Kind, Name, removed)) )
    ; true ).
uncache_type_decl(Name, Type) :-
    ( nonvar(Type), fn_type_shape(Type, ATs, OT, Det)
      -> maplist(normalize_type, ATs, ATN),
         normalize_type(OT, OTN),
         canonical_effect_model(Det, ATN, Effect),
         length(ATN, N),
         recorded_constructor_dependency_types(ConstructorCandidates),
         include(symbol_enters_constructor_set(Name), ConstructorCandidates,
                 RemovedCtorTypes),
         ( remove_fn_decl_record(Name, N, scheme(ATN, OTN), Effect, _)
           -> decl_notify(declaration_changed(Name/N, removed)),
              forall(member(T, RemovedCtorTypes),
                     decl_notify(constructor_set_changed(T, Name)))
         ; true )
    ; normalize_type(Type, TN),
      ( clause(declared_value_type(Name, T2), true, Ref), T2 =@= TN
        -> erase(Ref),
           retractall(nonfn_decl_origin(Name, _)),
           decl_notify(declaration_changed(value, Name, removed)),
           notify_removed_value_constructor(Name, TN)
      ; true ) ).

notify_removed_value_constructor(Name, T) :-
    ( atom(T), \+ primitive_type(T), \+ wildcard_type(T)
      -> decl_notify(constructor_set_changed(T, Name))
    ; true ).

%All raw (: Name Type) atoms still present in &self, in assertion order.
self_type_declarations(Terms) :-
    findall([C, Name, Type],
            ( C = (:),
              Goal =.. ['&self', C, Name, Type],
              catch(call(Goal), _, fail) ),
            Terms).


%Removing an alias must reconstruct declarations from their raw &self atoms:
%the cached stores contain only the expanded representation and cannot recover
%the alias spelling on their own. Include aliases/newtypes/spaces that depend
%on the removed name transitively, then rebuild every declaration mentioning
%one of those names in source order.
alias_removal_rebuild(Name, Ref) :-
    self_type_declarations(All),
    dependent_type_names(All, [Name], Names),
    include(declaration_mentions_any(Names), All, Terms),
    findall(fn_decl(F, N, Scheme, Effect, Origin, Provenance),
            fn_decl_copy(F, N, Scheme, Effect, Origin, Provenance),
            SavedFnDecls),
    with_decl_notifications_suppressed(
        ( forall(member(T, Terms), erase_cached_declaration_only(T)),
          erase(Ref),
          retractall(nonfn_decl_origin(Name, _)),
          forall(member(T, Terms),
                 recache_type_decl_preserving_origin(T, SavedFnDecls)) )),
    notify_rebuilt_declarations(Terms),
    decl_notify(declaration_changed(alias, Name, removed)).

notify_rebuilt_declarations(Terms) :-
    forall(( member(Term, Terms),
             rebuilt_declaration_event(Term, Event) ),
           decl_notify(Event)).

rebuilt_declaration_event([_, F, Type], declaration_changed(F/N, changed)) :-
    nonvar(Type), fn_type_shape(Type, ATs, _, _), !,
    length(ATs, N).
rebuilt_declaration_event([_, Name, Type],
                          declaration_changed(Kind, Name, changed)) :-
    kind_decl(Type, Keyword, _), !,
    decl_kind(Keyword, Kind, _, _, _).
rebuilt_declaration_event([_, Name, _],
                          declaration_changed(value, Name, changed)).

%Alias removal temporarily erases and rebuilds dependent cache entries, but
%their source declarations were not removed. Preserve any library provenance
%across that internal rebuild; only the removed alias itself loses origin.
recache_type_decl_preserving_origin([_, Name, Type], SavedFnDecls) :-
    nonvar(Type), fn_type_shape(Type, ATs, OT, Det), !,
    ( member(fn_decl(Name, _, _, _, Origin, Provenance), SavedFnDecls),
      Provenance = provenance(_, syntax(Syntax)),
      Syntax =@= Type
      -> cache_fn_type_decl(Name, Type, ATs, OT, Det, Origin, Provenance)
    ; maybe_cache_type_decl('&self', [(:), Name, Type]) ).
recache_type_decl_preserving_origin(Term, _) :-
    Term = [_, Name, _],
    ( nonfn_decl_origin(Name, library(Library))
      -> with_library_origin(Library, maybe_cache_type_decl('&self', Term))
    ; maybe_cache_type_decl('&self', Term) ).

dependent_type_names(All, Names0, Names) :-
    findall(N,
            ( member([_, N, Type], All),
              type_kind_representation(Type, Rep),
              member(Dep, Names0), type_term_mentions_alias(Rep, Dep) ),
            More),
    append(Names0, More, Ns0), sort(Ns0, Ns),
    ( Ns == Names0 -> Names = Ns
    ; dependent_type_names(All, Ns, Names) ).

type_kind_representation([K, R], R) :-
    ( K == 'Alias' ; K == 'Newtype' ; K == 'SpaceOf' ).

declaration_mentions_any(Names, [_, _, Type]) :-
    member(Name, Names), type_term_mentions_alias(Type, Name), !.

erase_cached_declaration_only([_, Name, Type]) :- kind_decl(Type, Keyword, Value), !,
    ( kind_decl_clause(Name, Keyword, Value, Ref) -> erase(Ref) ; true ).
erase_cached_declaration_only([_, Name, Type]) :-
    ( nonvar(Type), fn_type_shape(Type, ATs, OT, Det)
      -> maplist(normalize_type, ATs, ATN), normalize_type(OT, OTN),
         canonical_effect_model(Det, ATN, Effect),
         length(ATN, N),
         ( remove_fn_decl_record(Name, N, scheme(ATN, OTN), Effect, _)
           -> true ; true )
    ; normalize_type(Type, TN),
      ( clause(declared_value_type(Name, T2), true, Ref), T2 =@= TN -> erase(Ref) ; true ) ).

forget_symbol_types(Name) :- remove_all_fn_decl_records(Name),
                             unified_checker_invalidate_event(
                                 generated_specialization_removed(Name)),
                             retractall(nonfn_decl_origin(Name, _)),
                             forall(type_store(_, Store),
                                    ( Entry =.. [Store, Name, _], retractall(Entry) )),
                             retractall(inferred_fn_type(Name, _, _)),
                             retractall(det_bound_proviso(Name, _, _, _)),
                             analysis_cache_forget_symbol(Name).

%%% Store lookup (each retrieval yields a fresh copy of the declaration):
fn_decl_arity(F, N, ATs, OT) :- declared_fn_type(F, ATs, OT, _), length(ATs, N).
unique_fn_decl(F, N, ATs, OT) :- findall(A-O, fn_decl_arity(F, N, A, O), [ATs-OT]).
fn_decl_partial(F, N, PTs, RTs, OT, Det) :- declared_fn_type(F, ATs, OT, Det),
                                            length(ATs, Total), Total > N,
                                            length(PTs, N), append(PTs, RTs, ATs).
