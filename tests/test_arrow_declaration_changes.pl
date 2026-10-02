:- begin_tests(arrow_declaration_changes).

:- ensure_loaded('../src/metta.pl').

quiet :- retractall(silent(_)), assertz(silent(true)).
lambda_count(N) :- catch(nb_getval(lambda_counter, N), _, N = 0).

metadata(Fun, Meta) :- findall(Entry, function_metadata(Fun, Entry), Meta).

compiler_snapshot(state(Metadata, Sources, Arities, Specializations, Functions)) :-
    findall(Fun-Meta, function_metadata(Fun, Meta), Metadata),
    findall(Ref-Stored, translated_from(Ref, Stored), Sources),
    findall(Fun-Arity, arity(Fun, Arity), Arities),
    findall(Fun-Spec, ho_specialization(Fun, Spec), Specializations),
    findall(Fun, fun(Fun), Functions).

test(declaration_below_its_caller_reaches_that_caller, [setup(quiet)]) :-
    process_metta_string(
        "(= (ltc_order_g $x) (ltc_order_f $x))\n\c
         (: ltc_order_f (-> Number Number))\n\c
         (= (ltc_order_f $x) $x)", _),
    once(ltc_order_g(7, 7)),
    \+ ltc_order_g("bad", _).

test(added_and_removed_declaration_reaches_callers_of_callers, [setup(quiet)]) :-
    process_metta_string(
        "(= (ltc_dynamic_f $x) $x)\n\c
         (= (ltc_dynamic_g $x) (ltc_dynamic_f $x))\n\c
         (= (ltc_dynamic_h $x) (ltc_dynamic_g $x))", _),
    once(ltc_dynamic_h("before", "before")),
    Type = [':', ltc_dynamic_f, ['->', 'Number', 'Number']],
    'add-atom'('&self', Type, true),
    \+ ltc_dynamic_h("after-add", _),
    'remove-atom'('&self', Type, true),
    once(ltc_dynamic_h("after-remove", "after-remove")).

test(atom_argument_is_passed_unevaluated_by_an_earlier_caller, [setup(quiet)]) :-
    process_metta_string(
        "(= (ltc_role_f $x) $x)\n\c
         (= (ltc_role_g) (ltc_role_f (+ 2 3)))", _),
    once(ltc_role_g(5)),
    Type = [':', ltc_role_f, ['->', 'Atom', 'Atom']],
    'add-atom'('&self', Type, true),
    once(ltc_role_g([+, 2, 3])),
    'remove-atom'('&self', Type, true),
    once(ltc_role_g(5)).

test(removal_by_pattern_reaches_every_function_it_matches, [setup(quiet)]) :-
    process_metta_string(
        "(: ltc_pat_v LtcPat)\n\c
         (: ltc_pat_f (-> LtcPat LtcPat))\n\c
         (= (ltc_pat_f $x) $x)\n\c
         (: ltc_pat_g (-> LtcPat LtcPat))\n\c
         (= (ltc_pat_g $x) $x)\n\c
         (= (ltc_pat_user $x) (pair (ltc_pat_f $x) (ltc_pat_g $x)))", _),
    once(ltc_pat_user(ltc_pat_v, [pair, ltc_pat_v, ltc_pat_v])),
    \+ ltc_pat_user(other, _),
    'remove-atom'('&self', [':', _, ['->', 'LtcPat', 'LtcPat']], true),
    arrow_declarations(ltc_pat_f, []),
    arrow_declarations(ltc_pat_g, []),
    once(ltc_pat_user(other, [pair, other, other])).

% ltc_ref_a_first is translated again before ltc_ref_z_last turns out not to translate.
test(refused_declaration_changes_nothing, [setup(quiet)]) :-
    process_metta_string(
        "(= (ltc_ref_f $x $y) (pair $x $y))\n\c
         (= (ltc_ref_a_first $x) (one (ltc_ref_f $x)))\n\c
         (= (ltc_ref_z_last $x) (ltc_ref_f $x 1))", _),
    Functions = [ltc_ref_f, ltc_ref_a_first, ltc_ref_z_last],
    maplist(metadata, Functions, MetaBefore),
    clause(ltc_ref_a_first(_, _), _, FirstBefore),
    clause(ltc_ref_z_last(_, _), _, LastBefore),
    \+ 'add-atom'('&self', [':', ltc_ref_f, ['->', 'Number', 'Number']], true),
    arrow_declarations(ltc_ref_f, []),
    clause(ltc_ref_a_first(_, _), _, FirstAfter),
    clause(ltc_ref_z_last(_, _), _, LastAfter),
    FirstBefore == FirstAfter,
    LastBefore == LastAfter,
    maplist(metadata, Functions, MetaAfter),
    MetaBefore =@= MetaAfter,
    once(ltc_ref_z_last(5, [pair, 5, 1])).

test(refused_declaration_leaves_no_metadata_of_anonymous_functions_it_made, [setup(quiet)]) :-
    process_metta_string(
        "(= (ltc_anon_f $x $y) (pair $x $y))\n\c
         (= (ltc_anon_a $x) (|-> ($z) (one (ltc_anon_f $x))))\n\c
         (= (ltc_anon_z $x) (ltc_anon_f $x 1))", _),
    lambda_count(Before),
    \+ 'add-atom'('&self', [':', ltc_anon_f, ['->', 'Number', 'Number']], true),
    lambda_count(After),
    After > Before,
    First is Before + 1,
    forall(between(First, After, N),
           ( format(atom(Lambda), 'lambda_~d', [N]), \+ function_metadata(Lambda, _) )).

test(failed_transaction_undoes_a_declaration_and_what_followed, [setup(quiet)]) :-
    process_metta_string(
        "(= (ltc_tx_f $x) $x)\n\c
         (= (ltc_tx_g $x) (ltc_tx_f $x))", _),
    \+ transaction(( 'add-atom'('&self', [':', ltc_tx_f, ['->', 'Number', 'Number']], true), fail )),
    arrow_declarations(ltc_tx_f, []),
    once(ltc_tx_g("text", "text")).


% The earlier caller's constrained head captures data differently under an Atom domain.
test(refused_batch_restores_earlier_constrained_head_metadata, [setup(quiet)]) :-
    process_metta_string(
        "(= (ltc_meta_f $x $y) (pair $x $y))\n\c
         (= (ltc_meta_a_first (ltc_meta_f (+ 1 2))) datum)\n\c
         (= (ltc_meta_z_last $x) (ltc_meta_f $x 1))", _),
    compiler_snapshot(Before),
    \+ 'add-atom'('&self', [':', ltc_meta_f, ['->', 'Atom', 'Number']], true),
    compiler_snapshot(After),
    Before =@= After,
    arrow_declarations(ltc_meta_f, []),
    once(ltc_meta_z_last(5, [pair, 5, 1])).

test(outer_transaction_restores_new_lambda_metadata, [setup(quiet)]) :-
    process_metta_string(
        "(= (ltc_outer_f $x) $x)\n\c
         (= (ltc_outer_a $x) (|-> ($z) (pair $z (ltc_outer_f $x))))", _),
    compiler_snapshot(Before),
    lambda_count(BeforeCount),
    \+ transaction((
        'add-atom'('&self', [':', ltc_outer_f, ['->', 'Number', 'Number']], true),
        fail )),
    lambda_count(AfterCount),
    AfterCount > BeforeCount,
    compiler_snapshot(After),
    Before =@= After,
    First is BeforeCount + 1,
    forall(between(First, AfterCount, N),
           ( format(atom(Lambda), 'lambda_~d', [N]),
             \+ function_metadata(Lambda, _) )),
    once(ltc_outer_f("text", "text")).

test(repeated_and_unrelated_declarations_translate_nothing_again, [setup(quiet)]) :-
    process_metta_string(
        "(= (ltc_noop_f $x) $x)\n\c
         (= (ltc_noop_g $x) (ltc_noop_f $x))", _),
    Type = [':', ltc_noop_f, ['->', 'Number', 'Number']],
    'add-atom'('&self', Type, true),
    clause(ltc_noop_g(_, _), _, Before),
    'add-atom'('&self', Type, true),
    'add-atom'('&self', [':', ltc_unrelated, ['->', 'Number', 'Number']], true),
    clause(ltc_noop_g(_, _), _, After),
    Before == After.

test(clause_order_and_metadata_survive, [setup(quiet)]) :-
    process_metta_string(
        "(= (ltc_ord_f $x) $x)\n\c
         (= (ltc_ord_g 1) (one (ltc_ord_f 1)))\n\c
         (= (ltc_ord_g $x) (two (ltc_ord_f $x)))", _),
    findall(R, ltc_ord_g(1, R), Before),
    metadata(ltc_ord_g, MetaBefore), length(MetaBefore, 2),
    'add-atom'('&self', [':', ltc_ord_f, ['->', 'Number', 'Number']], true),
    findall(R, ltc_ord_g(1, R), After),
    Before == After,
    Before = [[one, 1], [two, 1]],
    metadata(ltc_ord_g, MetaAfter),
    MetaBefore =@= MetaAfter.

test(arity_of_an_earlier_translation_is_forgotten, [setup(quiet)]) :-
    process_metta_string(
        "(= (ltc_eta_g $x $y) (pair $x $y))\n\c
         (= (ltc_eta_f $x) (ltc_eta_g $x))", _),
    findall(A, arity(ltc_eta_f, A), Before), sort(Before, [2, 3]),
    Type = [':', ltc_eta_f, ['->', '%Undefined%', 'Atom']],
    'add-atom'('&self', Type, true),
    findall(A, arity(ltc_eta_f, A), Held), sort(Held, [2]),
    once(ltc_eta_f(1, [ltc_eta_g, 1])),
    'remove-atom'('&self', Type, true),
    findall(A, arity(ltc_eta_f, A), After), sort(After, [2, 3]).

test(specialization_of_a_higher_order_function_follows, [setup(quiet)]) :-
    process_metta_string(
        "(= (ltc_apply $op $x) ($op $x))\n\c
         (= (ltc_target $x) $x)\n\c
         (= (ltc_wrapper $x) (ltc_apply ltc_target $x))", _),
    ho_specialization(ltc_apply, _),
    once(ltc_wrapper("text", "text")),
    'add-atom'('&self', [':', ltc_target, ['->', 'Number', 'Number']], true),
    once(ltc_wrapper(1, 1)),
    \+ ltc_wrapper("text", _).


test(specialization_of_a_captured_partial_follows_add_and_remove, [setup(quiet)]) :-
    process_metta_string(
        "(= (ltc_partial_target $x $y) $y)\n\c
         (= (ltc_partial_apply $f $y) ($f $y))\n\c
         (= (ltc_partial_use $y) (ltc_partial_apply (ltc_partial_target 1) $y))", _),
    once(ho_specialization(ltc_partial_apply, _)),
    once(ltc_partial_use("text", "text")),
    findall(R, ltc_partial_use(2, R), Before),
    Before == [2],
    Type = [':', ltc_partial_target, ['->', 'Number', 'Number', '%Undefined%']],
    'add-atom'('&self', Type, true),
    \+ ltc_partial_use("text", _),
    findall(R, ltc_partial_use(2, R), After),
    After == Before,
    'remove-atom'('&self', Type, true),
    once(ltc_partial_use("text", "text")),
    findall(R, ltc_partial_use(2, R), Removed),
    Removed == Before.

test(declaration_of_another_function_keeps_specializations, [setup(quiet)]) :-
    process_metta_string(
        "(= (ltc_keep_apply $f $x) ($f $x))\n\c
         (= (ltc_keep_inc $x) (+ $x 1))\n\c
         (= (ltc_keep_use) (ltc_keep_apply ltc_keep_inc 3))\n\c
         (= (ltc_keep_other $x) $x)", _),
    findall(Spec, ho_specialization(ltc_keep_apply, Spec), Before),
    Before = [_|_],
    'add-atom'('&self', [':', ltc_keep_other, ['->', 'Number', 'Number']], true),
    findall(Spec, ho_specialization(ltc_keep_apply, Spec), After),
    Before == After,
    once(ltc_keep_use(4)).

test(anonymous_function_made_earlier_follows, [setup(quiet)]) :-
    process_metta_string(
        "(= (ltc_lam_f $x) $x)\n\c
         (= (ltc_lam_make) (|-> ($y) (ltc_lam_f $y)))", _),
    once(ltc_lam_make(Lambda)),
    once(reduce([Lambda, "text"], "text")),
    'add-atom'('&self', [':', ltc_lam_f, ['->', 'Number', 'Number']], true),
    once(reduce([Lambda, 1], 1)),
    \+ reduce([Lambda, "text"], _).


test(recompiled_body_invalidates_only_its_memoized_answers, [setup(quiet)]) :-
    library('lib_memo.pl', Library), ensure_loaded(Library),
    process_metta_string(
        "(= (ltc_memo_f $x) (+ $x 1))\n\c
         (= (ltc_memo_unrelated $x) (+ $x 2))", _),
    setup_call_cleanup(
        ( enable_memoization(ltc_memo_f), enable_memoization(ltc_memo_unrelated) ),
        ( once(eval([ltc_memo_f, 6], 7)),
          once(eval([ltc_memo_unrelated, 6], 8)),
          once(metta_memo_entry(ltc_memo_unrelated, 2, Generation, [6], Retained)),
          findall(R, 'add-atom'('&self',
                              [':', ltc_memo_f, ['->', 'Number', 'Atom']], R), Added),
          Added == [true],
          findall(R, eval([ltc_memo_f, 6], R), Results),
          Results == [[+, 6, 1]],
          once(metta_memo_entry(ltc_memo_unrelated, 2, Generation, [6], After)),
          Retained == After ),
        ( disable_memoization(ltc_memo_f), cache_invalidate(ltc_memo_f),
          disable_memoization(ltc_memo_unrelated), cache_invalidate(ltc_memo_unrelated) ) ).

test(translator_rule_follows_its_declaration, [setup(quiet)]) :-
    process_metta_string(
        "(= (ltc_rule $x) (if (is-expr $x) held evaluated))\n\c
         !(add-translator-rule! ltc_rule)\n\c
         (= (ltc_rule_user) (ltc_rule (+ 1 2)))", _),
    once(ltc_rule_user(evaluated)),
    Type = [':', ltc_rule, ['->', 'Atom', '%Undefined%']],
    'add-atom'('&self', Type, true),
    once(ltc_rule_user(held)),
    'remove-atom'('&self', Type, true),
    once(ltc_rule_user(evaluated)).

% The equation that adds the declaration finishes as it was translated, like one that adds an equation.
test(running_equation_finishes_as_translated_and_its_next_run_follows, [setup(quiet)]) :-
    process_metta_string(
        "(= (ltc_run_k $x) (got $x))\n\c
         (= (ltc_run_c) (let $u (add-atom &self (: ltc_run_k (-> Atom %Undefined%))) (ltc_run_k (+ 1 2))))", _),
    once(ltc_run_c(First)),
    First == [got, 3],
    once(ltc_run_c(Second)),
    Second == [got, [+, 1, 2]].

% A thread that evaluates in parallel must not make a specialization the translating thread also makes.
test(compiler_metadata_stays_with_the_thread_that_translated, [setup(quiet)]) :-
    process_metta_string("(= (ltc_thread_f $x) $x)", _),
    function_metadata(ltc_thread_f, _),
    thread_create(\+ function_metadata(ltc_thread_f, _), Thread),
    thread_join(Thread, Status),
    Status == true.

:- end_tests(arrow_declaration_changes).
