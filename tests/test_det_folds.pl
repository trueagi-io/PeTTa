:- ensure_loaded('../src/metta').

:- begin_tests(det_folds).

load_program :-
        ( silent(true) -> true ; retractall(silent(_)), assertz(silent(true)) ),
        process_metta_string("
!(import! &self (library lib_roman))
(: add-step (-[det]-> Number Number Number))
(= (add-step $acc $item) (+ $acc $item))
(: inc (-[det]-> Number Number))
(= (inc $x) (+ $x 1))
(: sum-list (-[det]-> (List Number) Number))
(= (sum-list $xs) (fold-flat add-step 0 $xs))
(: incs (-[det]-> (List Number) (List Number)))
(= (incs $xs) (map-flat inc $xs))
(: either-step (-[nondet]-> Number Number Number))
(= (either-step $acc $item) (superpose ((+ $acc $item) (* $acc $item))))
(= (either-sums $xs) (fold-flat either-step 1 $xs))
(: count-step (-[det]-> Number Number Number))
(= (count-step $acc $item) (+ $acc 1))
(= (pass $xs) $xs)
(= (count-passed $xs) (fold-flat count-step 0 (pass $xs)))
", _).

deterministic(Goal) :- call_cleanup(Goal, Det = true), Det == true.

:- load_program.

% A fold or map over a det step, called where the checker proves it det,
% leaves no choice point on the clause it ends on.
test(det_fold_leaves_no_choice_point) :-
        deterministic('sum-list'([1, 2, 3], S)),
        S == 6.

test(det_map_leaves_no_choice_point) :-
        deterministic(incs([1, 2, 3], L)),
        L == [2, 3, 4].

% A nondeterministic step keeps every answer.
test(nondet_step_keeps_answers, Sums == [6, 9, 5, 6]) :-
        findall(S, 'either-sums'([2, 3], S), Sums).

% Over a list the checker cannot see, the call commits only once the list
% tests proper at runtime: a proper list leaves no choice point, an unbound
% one still enumerates lists.
test(runtime_list_test_commits) :-
        deterministic('count-passed'([a, b, c], N)),
        N == 3.

test(unbound_list_still_enumerates, Counts == [0, 1, 2]) :-
        findall(N, limit(3, 'count-passed'(_, N)), Counts).

:- end_tests(det_folds).
