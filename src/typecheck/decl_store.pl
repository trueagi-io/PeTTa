%%% Canonical declaration storage and declaration lifecycle.
%
% Owns: every function/value/type declaration store, declaration provenance and
% library origin, add/remove/cache/renormalize operations, and fresh-copy lookup
% views.
% Consumes: arrow/type normalization, mutation notification, source-loading
% provenance helpers, and translator recompilation entry points.
% Boundary: all declaration mutation enters through maybe_cache_type_decl/2,
% maybe_uncache_type_decl/2, or forget_symbol_types/1.

:- thread_local loading_origin/1.
:- thread_local declaration_provenance/1.
:- dynamic fn_decl/6.
:- dynamic nonfn_decl_origin/2.
:- dynamic declared_value_type/2.   % declared_value_type(Name, Type)
:- dynamic declared_newtype/2.      % declared_newtype(Name, Representation)
:- dynamic declared_type_alias/2.   % declared_type_alias(Name, Representation)
:- dynamic declared_foreign_type/2. % declared_foreign_type(Name, Arity)
:- dynamic declared_space_type/2.   % declared_space_type(Name, RowType)
:- discontiguous maybe_cache_type_decl/2.

%%% Canonical function-declaration record.
%
% fn_decl(F, Arity, scheme(ArgTypes, OutType), Effect, Origin, Provenance)
%
% Effect = effect_model(Top, Variables)
%   Top       = det | semidet | nondet | unspecified | variable(Name)
%   Variables = [] | [effect_var(Name, [closure_arg(Index, Arity), ...])]
%
% Provenance keeps both the source location and original syntax. The syntax is
% load-bearing lifecycle information: it lets late alias addition/removal
% renormalize this one store without reconstructing declarations from parallel
% effect/origin tables. Effect metadata itself is closed and contains no Prolog
% variables.

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

%Compatibility view: like the old side table, it succeeds only for trusted
%library origin (user was represented by absence).
decl_origin(F, Origin) :-
    fn_decl(F, _, _, _, Origin, _),
    Origin = library(_).
decl_origin(Name, Origin) :-
    nonfn_decl_origin(Name, Origin).

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

%The only two predicates that write the canonical store. Every cache,
%renormalization, origin change, removal and forget path is expressed through
%this pair.
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
%Erased nominal newtypes: (: KB (Newtype Expression)) declares KB as a
%distinct compile-time role over the given representation. Nothing exists at
%runtime; the brand lives purely in the checker.
maybe_cache_type_decl(Space, Term) :- Space == '&self', is_list(Term), Term = [C, Name, [NT, R]],
                                      C == (:), atom(Name), NT == 'Newtype', !,
                                      with_decl_transaction(Name,
                                          cache_newtype_decl(Name, R)).
cache_newtype_decl(Name, R) :-
                                      prepare_decl_origin(Name, [NT, R], _),
                                      NT = 'Newtype',
                                      normalize_type(R, RN),
                                      ( declared_newtype(Name, R2)
                                        -> ( R2 =@= RN -> true
                                           ; format(user_error,
                                                    "Warning: conflicting Newtype declaration for ~w ignored: ~p differs from ~p~n",
                                                    [Name, RN, R2]) )
                                      ; declared_type_alias(Name, _)
                                        -> format(user_error,
                                                  "Warning: Newtype declaration for ~w ignored: name is already an Alias~n",
                                                  [Name])
                                      ; declared_foreign_type(Name, _)
                                        -> format(user_error,
                                                  "Warning: Newtype declaration for ~w ignored: name is already a Foreign type~n",
                                                  [Name])
                                      ; declared_space_type(Name, _)
                                        -> format(user_error,
                                                  "Warning: Newtype declaration for ~w ignored: name is already a SpaceOf type~n",
                                                  [Name])
                                      ; assertz(declared_newtype(Name, RN)),
                                        decl_notify(declaration_changed(newtype, Name,
                                                                         added)) ).
%Structural aliases: (: Row (Alias (Number String))) names an erased type
%expression, not a nominal role. Normalize now so later lookup is one
%non-recursive expansion. Aliases are not hoisted by precache_fn_type_decl/2;
%declare-before-use stays cheapest, while a fresh late alias repairs prior
%declarations below.
maybe_cache_type_decl(Space, Term) :- Space == '&self', is_list(Term), Term = [C, Name, [A, R]],
                                      C == (:), atom(Name), A == 'Alias', !,
                                      with_decl_transaction(Name,
                                          cache_alias_decl(Name, R)).
cache_alias_decl(Name, R) :-
                                      prepare_decl_origin(Name, [A, R], _),
                                      A = 'Alias',
                                      normalize_type(R, RN),
                                      ( declared_type_alias(Name, R2)
                                        -> ( R2 =@= RN -> true
                                           ; format(user_error,
                                                    "Warning: conflicting type alias declaration for ~w ignored: ~p differs from ~p~n",
                                                    [Name, RN, R2]) )
                                      ; declared_newtype(Name, _)
                                        -> format(user_error,
                                                  "Warning: type alias declaration for ~w ignored: name is already a Newtype~n",
                                                  [Name])
                                      ; declared_foreign_type(Name, _)
                                        -> format(user_error,
                                                  "Warning: type alias declaration for ~w ignored: name is already a Foreign type~n",
                                                  [Name])
                                      ; declared_space_type(Name, _)
                                        -> format(user_error,
                                                  "Warning: type alias declaration for ~w ignored: name is already a SpaceOf type~n",
                                                  [Name])
                                      ; assertz(declared_type_alias(Name, RN)),
                                        renormalize_late_alias(Name, _),
                                        decl_notify(declaration_changed(alias, Name,
                                                                         added)) ).
%Opaque foreign types: (: Heap (Foreign)) and (: Heap (Foreign 1)) declare a
%nominal runtime-uncheckable type constructor. Only its name and arity enter
%the checker; native values themselves remain wholly opaque.
maybe_cache_type_decl(Space, Term) :- Space == '&self', is_list(Term), Term = [C, Name, [F|Spec]],
                                      C == (:), atom(Name), F == 'Foreign',
                                      ( Spec == [] -> Arity = 0
                                       ; Spec = [Arity], integer(Arity), Arity > 0 ), !,
                                      with_decl_transaction(Name,
                                          cache_foreign_decl(Name, Spec, Arity)).
cache_foreign_decl(Name, Spec, Arity) :-
                                      F = 'Foreign',
                                      prepare_decl_origin(Name, [F|Spec], _),
                                      ( declared_foreign_type(Name, A2)
                                        -> ( A2 == Arity -> true
                                           ; format(user_error,
                                                    "Warning: conflicting foreign type declaration for ~w ignored: arity ~w differs from ~w~n",
                                                    [Name, Arity, A2]) )
                                      ; declared_newtype(Name, _)
                                        -> format(user_error,
                                                  "Warning: foreign type declaration for ~w ignored: name is already a Newtype~n",
                                                  [Name])
                                      ; declared_type_alias(Name, _)
                                        -> format(user_error,
                                                  "Warning: foreign type declaration for ~w ignored: name is already an Alias~n",
                                                  [Name])
                                      ; declared_space_type(Name, _)
                                        -> format(user_error,
                                                  "Warning: foreign type declaration for ~w ignored: name is already a SpaceOf type~n",
                                                  [Name])
                                      ; assertz(declared_foreign_type(Name, Arity)),
                                        decl_notify(declaration_changed(foreign, Name,
                                                                         added)) ).
%Typed spaces: (: &jobs (SpaceOf Row)) opts one statically named space into
%row checking. Normalize now so aliases in schemas are erased before use;
%like Newtype/Alias/Foreign, this declaration is source-ordered, not hoisted.
maybe_cache_type_decl(Space, Term) :- Space == '&self', is_list(Term), Term = [C, Name, [SO, R]],
                                      C == (:), atom(Name), SO == 'SpaceOf', !,
                                      with_decl_transaction(Name,
                                          cache_space_decl(Name, R)).
cache_space_decl(Name, R) :-
                                      SO = 'SpaceOf',
                                      prepare_decl_origin(Name, [SO, R], _),
                                      normalize_type(R, RN),
                                      ( declared_space_type(Name, R2)
                                        -> ( R2 =@= RN -> true
                                           ; format(user_error,
                                                    "Warning: conflicting space type declaration for ~w ignored: ~p differs from ~p~n",
                                                    [Name, RN, R2]) )
                                      ; declared_newtype(Name, _)
                                        -> format(user_error,
                                                  "Warning: space type declaration for ~w ignored: name is already a Newtype~n",
                                                  [Name])
                                      ; declared_type_alias(Name, _)
                                        -> format(user_error,
                                                  "Warning: space type declaration for ~w ignored: name is already an Alias~n",
                                                  [Name])
                                      ; declared_foreign_type(Name, _)
                                        -> format(user_error,
                                                  "Warning: space type declaration for ~w ignored: name is already a Foreign type~n",
                                                  [Name])
                                      ; validate_existing_space_rows(Name, RN),
                                        assertz(declared_space_type(Name, RN)),
                                        decl_notify(declaration_changed(space, Name,
                                                                         added)) ).
maybe_cache_type_decl(Space, Term) :- ( Space == '&self', is_list(Term), Term = [C, Name, Type],
                                        C == (:), atom(Name)
                                        -> ( nonvar(Type), infix_arrow_misuse(Type)
                                             -> throw(error(infix_arrow_syntax(Name, Type), typecheck))
                                           ; nonvar(Type), fn_type_shape(Type, ATs, OT, Det)
                                              -> with_fn_decl_transaction(
                                                     Name,
                                                     ( prepare_decl_origin(Name, Type, Origin),
                                                       current_fn_decl_provenance(Type, Provenance),
                                                       cache_fn_type_decl(Name, Type, ATs, OT, Det,
                                                                          Origin, Provenance) ))
                                              ; with_decl_transaction(
                                                    Name,
                                                    cache_value_decl(Name, Type)) )
                                        ; true ).

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

%A declaration-driven graph revalidation is transactional with respect to the
%canonical declaration state.  The raw source atom inserted by runtime
%add-atom remains outside this bounded transaction (MeTTa has no catchable
%mutation exception boundary yet), but executable clauses, declarations,
%origins and inferred types are restored together.
with_fn_decl_transaction(Name, Goal) :-
    with_decl_transaction(Name, Goal).

with_decl_transaction(Name, Goal) :-
    snapshot_decl_transaction(Name, Snapshot),
    catch(( Goal -> true
          ; restore_decl_transaction(Name, Snapshot),
            fail ),
          Error,
          ( restore_decl_transaction(Name, Snapshot),
            throw(Error) )).

snapshot_decl_transaction(Name,
        decl_transaction(Records, Origins, Inferred, Values, Newtypes,
                         Aliases, Foreigns, Spaces)) :-
    findall(fn_decl(Name, N, Scheme, Effect, Origin, Provenance),
            fn_decl_copy(Name, N, Scheme, Effect, Origin, Provenance),
            Records),
    findall(Origin, nonfn_decl_origin(Name, Origin), Origins),
    findall(inferred(ATs, OT), inferred_fn_type(Name, ATs, OT), Inferred),
    findall(T, declared_value_type(Name, T), Values),
    findall(T, declared_newtype(Name, T), Newtypes),
    findall(T, declared_type_alias(Name, T), Aliases),
    findall(A, declared_foreign_type(Name, A), Foreigns),
    findall(T, declared_space_type(Name, T), Spaces).

restore_decl_transaction(Name,
        decl_transaction(Records, Origins, Inferred, Values, Newtypes,
                         Aliases, Foreigns, Spaces)) :-
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
          retractall(declared_value_type(Name, _)),
          forall(member(T, Values), assertz(declared_value_type(Name, T))),
          retractall(declared_newtype(Name, _)),
          forall(member(T, Newtypes), assertz(declared_newtype(Name, T))),
          retractall(declared_type_alias(Name, _)),
          forall(member(T, Aliases), assertz(declared_type_alias(Name, T))),
          retractall(declared_foreign_type(Name, _)),
          forall(member(A, Foreigns), assertz(declared_foreign_type(Name, A))),
          retractall(declared_space_type(Name, _)),
          forall(member(T, Spaces), assertz(declared_space_type(Name, T))) )),
    %Consumers may already have reacted to an origin flip before the staged
    %declaration failed. Re-run those exact graph edges under the restored
    %state; retain the original validation error if rollback revalidation
    %itself reports anything.
    catch(decl_notify(declaration_changed(origin, Name, changed)), _, true),
    forall(member(N, Arities),
           catch(decl_notify(declaration_changed(Name/N, changed)), _, true)),
    forall(member(Kind, [value, newtype, alias, foreign, space]),
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
    catch(nb_getval('$batched_decl_notifications', Events0), _, fail),
    is_list(Events0), !,
    nb_setval('$batched_decl_notifications', [Event|Events0]).
decl_notify(Event) :-
    notify_mutation(Event).

:- meta_predicate with_decl_notifications_batched(0).

% Function declarations are hoisted as one file-level prepass.  Consumers
% should therefore be rebuilt against that final declaration set, not once for
% every intermediate prefix of it.  Nested batching contributes to the outer
% queue; the outermost scope restores notification mode before flushing.
with_decl_notifications_batched(Goal) :-
    ( catch(nb_getval('$batched_decl_notifications', Existing), _, fail),
      is_list(Existing)
      -> call(Goal)
    ; setup_call_cleanup(
          push_decl_notification_batch(Saved),
          call(Goal),
          pop_and_flush_decl_notification_batch(Saved)) ).

push_decl_notification_batch(saved(yes, Previous)) :-
    catch(nb_getval('$batched_decl_notifications', Previous), _, fail), !,
    nb_setval('$batched_decl_notifications', []).
push_decl_notification_batch(saved(no, none)) :-
    nb_setval('$batched_decl_notifications', []).

pop_and_flush_decl_notification_batch(Saved) :-
    ( catch(nb_getval('$batched_decl_notifications', Reversed), _, fail)
      -> true
    ; Reversed = [] ),
    restore_decl_notification_batch(Saved),
    reverse(Reversed, Ordered),
    list_to_set(Ordered, Events),
    notify_mutations(Events).

restore_decl_notification_batch(saved(yes, Previous)) :- !,
    nb_setval('$batched_decl_notifications', Previous).
restore_decl_notification_batch(_) :-
    catch(nb_delete('$batched_decl_notifications'), _, true).

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

symbol_enters_constructor_set(Name, T) :-
    once(new_ctor_key(Name, T, _)).

%Bridge declaration mutation to the dependency graph's ctor_set(Type) events.
new_ctor_key(Name, T, K) :- member_ctor(T, K, Name).
new_ctor_key(Name, T, 0) :-
    declared_value_type(Name, T2),
    T = T2,
    \+ fun(Name).

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
cached_symbol_declaration(Name) :- declared_value_type(Name, _), !.
cached_symbol_declaration(Name) :- declared_newtype(Name, _), !.
cached_symbol_declaration(Name) :- declared_type_alias(Name, _), !.
cached_symbol_declaration(Name) :- declared_foreign_type(Name, _), !.
cached_symbol_declaration(Name) :- declared_space_type(Name, _).

warn_user_library_redeclaration(Name, Type, Library) :-
    ( cached_declaration_matches(Name, Type)
      -> true
    ; library_decl_location(Name, Location),
      format(user_error,
             "Warning: user declaration for ~w differs from trusted library ~w declaration~w: ~p~n",
             [Name, Library, Location, Type]) ).

library_decl_location(Name, Text) :-
    fn_decl(Name, _, _, _, library(_), provenance(Location, _)), !,
    provenance_location_text(Location, Text).
library_decl_location(_, '').

provenance_location_text(source(File, Line), Text) :-
    format(atom(Text), " at ~w:~w", [File, Line]).
provenance_location_text(_, '').

cached_declaration_matches(Name, Type) :-
    nonvar(Type), fn_type_shape(Type, ATs, OT, Det), !,
    maplist(normalize_type, ATs, ATN),
    normalize_type(OT, OTN),
    declared_fn_type(Name, A2, O2, D2),
    (A2-O2-D2) =@= (ATN-OTN-Det).
cached_declaration_matches(Name, [NT, R]) :- NT == 'Newtype', !,
    normalize_type(R, RN), declared_newtype(Name, R2), R2 =@= RN.
cached_declaration_matches(Name, [A, R]) :- A == 'Alias', !,
    normalize_type(R, RN), declared_type_alias(Name, R2), R2 =@= RN.
cached_declaration_matches(Name, [F|Spec]) :- F == 'Foreign', !,
    ( Spec == [] -> Arity = 0 ; Spec = [Arity] ),
    declared_foreign_type(Name, Arity).
cached_declaration_matches(Name, [SO, R]) :- SO == 'SpaceOf', !,
    normalize_type(R, RN), declared_space_type(Name, R2), R2 =@= RN.
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

%Strict determinism is an explicit-effect mode: every arrow in a function
%declaration, including arrows nested in parameter/output positions (and
%aliases expanded into those positions), must name its cardinality. The
%standard builtin signature file is checker-internal type metadata; builtin
%determinism comes authoritatively from det_builtins.pl, so that one load path
%is exempt instead of duplicating hundreds of table annotations.
require_explicit_det_arrows(Name, Type) :-
    ( strict_det(true), \+ builtin_signature_load,
      normalize_type(Type, Normalized),
      type_contains_plain_arrow(Normalized)
      -> throw(error(strict_det_plain_arrow(Name), determinism))
    ; true ).

type_contains_plain_arrow(T) :-
    nonvar(T), is_list(T),
    ( T = [H|_], H == (->)
    ; member(E, T), type_contains_plain_arrow(E) ), !.

builtin_signature_load :- nb_current('$seeding_builtin_types', true), !.
builtin_signature_load :-
    current_metta_file(File),
    standard_library_path(Base),
    atomic_list_concat([Base, '/lib_builtin_types.metta'], BuiltinFile),
    catch(same_file(File, BuiltinFile), _, fail).

%Arrow declarations are pre-cached before source-ordered Alias declarations
%exist. When the declaration is processed in place, remove its syntax-only
%prepass copy if normalize_type/2 expanded an alias; otherwise that stale
%opaque overload would leak the alias name into the checker.
remove_unexpanded_fn_precache(Name, ATs, OT, Det, ATN, OTN) :-
    maplist(normalize_type_syntax, ATs, RawATs),
    normalize_type_syntax(OT, RawOT),
    ( (RawATs-RawOT) =@= (ATN-OTN)
      -> true
    ; canonical_effect_model(Det, RawATs, RawEffect),
      length(RawATs, N),
      remove_fn_decl_record(Name, N, scheme(RawATs, RawOT), RawEffect, _)
      -> true
    ; true ).

%Canonicalize only arrow syntax, deliberately leaving atoms untouched. This
%reconstructs the exact type cached before a source-local alias was visible.
normalize_type_syntax(T, T) :- var(T), !.
normalize_type_syntax(T, T) :- atomic(T), !.
normalize_type_syntax(T, TN) :- is_list(T), fn_type_shape(T, ATs, OT, _), !,
                                T = [Arrow|_],
                                canonical_arrow(Arrow, H),
                                maplist(normalize_type_syntax, ATs, ATN),
                                normalize_type_syntax(OT, OTN),
                                append(ATN, [OTN], Xs),
                                TN = [H|Xs].
normalize_type_syntax(T, TN) :- is_list(T), !, maplist(normalize_type_syntax, T, TN).
normalize_type_syntax(T, T).

%%% A fresh alias may arrive after declarations already cached its name as an
%%% opaque atom. Rebuild every declaration store that contains that exact atom
%%% in source order, now that normalize_type/2 can erase it. Only functions
%%% whose arrow entries changed need recompilation; determinism metadata is
%%% keyed by function name and arity, neither of which changes here.
renormalize_late_alias(Name, Fs) :-
    self_type_declarations(All),
    dependent_type_names(All, [Name], Names),
    include(declaration_mentions_any(Names), All, Rebuilt),
    renormalize_alias_fn_decls(Name, Fs),
    renormalize_alias_value_decls(Name),
    renormalize_alias_space_decls(Name),
    renormalize_alias_alias_decls(Name),
    renormalize_alias_newtype_decls(Name),
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

renormalize_alias_value_decls(Name) :-
    findall(value(V, T), declared_value_type(V, T), Ds),
    ( member(value(_, T0), Ds), type_term_mentions_alias(T0, Name)
      -> retractall(declared_value_type(_, _)),
         forall(member(value(V, T), Ds),
                ( ( type_term_mentions_alias(T, Name) -> normalize_type(T, TN) ; TN = T ),
                  ( declared_value_type(V, T2), T2 =@= TN -> true
                  ; assertz(declared_value_type(V, TN)) ) ))
    ; true ).

renormalize_alias_space_decls(Name) :-
    findall(space(S, T), declared_space_type(S, T), Ds),
    ( member(space(_, T0), Ds), type_term_mentions_alias(T0, Name)
      -> retractall(declared_space_type(_, _)),
         forall(member(space(S, T), Ds),
                ( ( type_term_mentions_alias(T, Name) -> normalize_type(T, TN) ; TN = T ),
                  ( declared_space_type(S, T2), T2 =@= TN -> true
                  ; assertz(declared_space_type(S, TN)) ) ))
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

renormalize_alias_newtype_decls(Name) :-
    findall(newtype(N, T), declared_newtype(N, T), Ds),
    ( member(newtype(_, T0), Ds), type_term_mentions_alias(T0, Name)
      -> retractall(declared_newtype(_, _)),
         forall(member(newtype(N, T), Ds),
                ( ( type_term_mentions_alias(T, Name) -> normalize_type(T, TN) ; TN = T ),
                  ( declared_newtype(N, T2), T2 =@= TN -> true
                  ; assertz(declared_newtype(N, TN)) ) ))
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

%Seed the store with the builtin operator types (called once after loading):
seed_builtin_types :- standard_library_path(Base),
                      atomic_list_concat([Base, '/lib_builtin_types.metta'], Path),
                      read_file_to_string(Path, S, []),
                      metta_string_forms(S, Forms),
                      setup_call_cleanup(
                          nb_setval('$seeding_builtin_types', true),
                          in_metta_file(
                              Path,
                              forall(member(form(FormStr, Line), Forms),
                                     with_form_location(
                                         Line, FormStr,
                                         ( sread(FormStr, Term),
                                           maybe_cache_type_decl('&self', Term) )))),
                          nb_delete('$seeding_builtin_types')).

maybe_uncache_type_decl(Space, Term) :-
    ( Space == '&self', is_list(Term), Term = [C, Name, Type],
      C == (:), atom(Name)
      -> uncache_type_decl(Name, Type)
    ; true ).

%Removal is the inverse of caching: erase the exact entry, rebuild any stores
%whose normalization depended on a removed alias, recompute explicit-arrow
%metadata from the declarations that remain in &self, and recompile affected
%clauses so cuts and guards match the surviving declarations.
uncache_type_decl(Name, [NT, R]) :- NT == 'Newtype', !,
    normalize_type(R, RN),
    ( clause(declared_newtype(Name, R2), true, Ref), R2 =@= RN
      -> erase(Ref),
         retractall(nonfn_decl_origin(Name, _)),
         decl_notify(declaration_changed(newtype, Name, removed))
    ; true ).
uncache_type_decl(Name, [A, R]) :- A == 'Alias', !,
    normalize_type(R, RN),
    ( clause(declared_type_alias(Name, R2), true, Ref), R2 =@= RN
      -> alias_removal_rebuild(Name, Ref)
    ; true ).
uncache_type_decl(Name, [F|Spec]) :-
    F == 'Foreign',
    ( Spec == [] -> Arity = 0
    ; Spec = [Arity], integer(Arity), Arity > 0 ), !,
    ( clause(declared_foreign_type(Name, A2), true, Ref), A2 == Arity
      -> erase(Ref),
         retractall(nonfn_decl_origin(Name, _)),
         decl_notify(declaration_changed(foreign, Name, removed))
    ; true ).
uncache_type_decl(Name, [SO, R]) :- SO == 'SpaceOf', !,
    normalize_type(R, RN),
    ( clause(declared_space_type(Name, R2), true, Ref), R2 =@= RN
      -> erase(Ref),
         retractall(nonfn_decl_origin(Name, _)),
         decl_notify(declaration_changed(space, Name, removed))
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
rebuilt_declaration_event([_, Name, [Kind, _]],
                          declaration_changed(StoreKind, Name, changed)) :-
    declaration_kind_name(Kind, StoreKind), !.
rebuilt_declaration_event([_, Name, [Foreign|_]],
                          declaration_changed(foreign, Name, changed)) :-
    Foreign == 'Foreign', !.
rebuilt_declaration_event([_, Name, _],
                          declaration_changed(value, Name, changed)).

declaration_kind_name('Alias', alias).
declaration_kind_name('Newtype', newtype).
declaration_kind_name('SpaceOf', space).

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

declaration_function_name([_, F, Type], F) :-
    nonvar(Type), fn_type_shape(Type, _, _, _).

erase_cached_declaration_only([_, Name, [NT, R]]) :- NT == 'Newtype', !,
    normalize_type(R, RN),
    ( clause(declared_newtype(Name, R2), true, Ref), R2 =@= RN -> erase(Ref) ; true ).
erase_cached_declaration_only([_, Name, [A, R]]) :- A == 'Alias', !,
    normalize_type(R, RN),
    ( clause(declared_type_alias(Name, R2), true, Ref), R2 =@= RN -> erase(Ref) ; true ).
erase_cached_declaration_only([_, Name, [F|Spec]]) :-
    F == 'Foreign',
    ( Spec == [] -> Arity = 0
    ; Spec = [Arity], integer(Arity), Arity > 0 ), !,
    ( clause(declared_foreign_type(Name, A2), true, Ref), A2 == Arity -> erase(Ref) ; true ).
erase_cached_declaration_only([_, Name, [SO, R]]) :- SO == 'SpaceOf', !,
    normalize_type(R, RN),
    ( clause(declared_space_type(Name, R2), true, Ref), R2 =@= RN -> erase(Ref) ; true ).
erase_cached_declaration_only([_, Name, Type]) :-
    ( nonvar(Type), fn_type_shape(Type, ATs, OT, Det)
      -> maplist(normalize_type, ATs, ATN), normalize_type(OT, OTN),
         canonical_effect_model(Det, ATN, Effect),
         length(ATN, N),
         ( remove_fn_decl_record(Name, N, scheme(ATN, OTN), Effect, _)
           -> true ; true )
    ; normalize_type(Type, TN),
      ( clause(declared_value_type(Name, T2), true, Ref), T2 =@= TN -> erase(Ref) ; true ) ).

affected_decl_functions(Names, Fs) :-
    findall(F,
            ( declared_fn_type(F, ATs, OT, _),
              member(Name, Names), type_term_mentions_alias(ATs-OT, Name) ),
            ByType),
    findall(F,
            ( catch(translated_from(_, Term), _, fail), nonvar(Term),
              Term = [=, Head, _], nonvar(Head), Head = [F|_],
              member(Name, Names), type_term_mentions_alias(Term, Name) ),
            BySource),
    append(ByType, BySource, Fs0), sort(Fs0, Fs).

forget_symbol_types(Name) :- remove_all_fn_decl_records(Name),
                             unified_checker_invalidate_event(
                                 generated_specialization_removed(Name)),
                             retractall(nonfn_decl_origin(Name, _)),
                             retractall(declared_value_type(Name, _)),
                             retractall(declared_newtype(Name, _)),
                             retractall(declared_type_alias(Name, _)),
                             retractall(declared_foreign_type(Name, _)),
                             retractall(declared_space_type(Name, _)),
                             retractall(inferred_fn_type(Name, _, _)),
                             retractall(det_bound_proviso(Name, _, _, _)),
                             analysis_cache_invalidate(effect(Name)),
                             reset_output_certs(Name).  %withdraw the output certificates

%%% Store lookup (each retrieval yields a fresh copy of the declaration):
fn_decl_arity(F, N, ATs, OT) :- declared_fn_type(F, ATs, OT, _), length(ATs, N).
fn_decl_partial(F, N, PTs, RTs, OT) :- fn_decl_partial(F, N, PTs, RTs, OT, _).
fn_decl_partial(F, N, PTs, RTs, OT, Det) :- declared_fn_type(F, ATs, OT, Det),
                                            length(ATs, Total), Total > N,
                                            length(PTs, N), append(PTs, RTs, ATs).
