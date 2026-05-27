:- use_module(library(clpfd)).
:- use_module(library(error)).

% -------- Assignment (cost matrix) --------

csp_assignment_opt(CostMatrix, Out) :-
    validate_cost_matrix(CostMatrix, N),
    assignment_model(CostMatrix, N, TaskVars1, TotalCost),
    (   once(labeling([min(TotalCost), ffc, bisect], TaskVars1))
    ->  assignment_solution_term(TaskVars1, TotalCost, Out)
    ;   Out = 'Empty'
    ).

csp_assignment_topk(K, CostMatrix, Out) :-
    must_be(integer, K),
    (   K > 0
    ->  true
    ;   domain_error(not_less_than_one, K)
    ),
    validate_cost_matrix(CostMatrix, N),
    once(findnsols(
        K,
        Sol,
        ( assignment_model(CostMatrix, N, TaskVars1, TotalCost),
          labeling([min(TotalCost), ffc, bisect], TaskVars1),
          assignment_solution_term(TaskVars1, TotalCost, Sol)
        ),
        Solutions
    )),
    (   Solutions == []
    ->  Out = 'Empty'
    ;   Out = Solutions
    ).

assignment_model(CostMatrix, N, TaskVars1, TotalCost) :-
    length(TaskVars1, N),
    TaskVars1 ins 1..N,
    all_distinct(TaskVars1),
    maplist(row_cost, CostMatrix, TaskVars1, Costs),
    sum(Costs, #=, TotalCost).

row_cost(Row, Task1, Cost) :-
    element(Task1, Row, Cost).

assignment_solution_term(TaskVars1, TotalCost, ['assignment-solution', TotalCost, Assignments]) :-
    findall([assign, WorkerIdx, TaskIdx0],
            ( nth0(WorkerIdx, TaskVars1, TaskIdx1),
              TaskIdx0 is TaskIdx1 - 1
            ),
            Assignments).

validate_cost_matrix(CostMatrix, N) :-
    must_be(list, CostMatrix),
    length(CostMatrix, N),
    (   N > 0
    ->  true
    ;   domain_error(non_empty_matrix, CostMatrix)
    ),
    maplist(valid_cost_row(N), CostMatrix).

valid_cost_row(N, Row) :-
    must_be(list, Row),
    length(Row, N),
    maplist(must_be(integer), Row).

% -------- Timetabling --------

csp_timetable_solve(ClassesRaw, RoomsRaw, SlotsRaw, BlockedRaw, Out) :-
    validate_timetable_inputs(ClassesRaw, RoomsRaw, SlotsRaw, BlockedRaw, Classes, Rooms, Slots, Blocked),
    (   timetable_model(Classes, Rooms, Slots, Blocked, SlotVars, RoomVars)
    ->  append(SlotVars, RoomVars, SearchVars),
        (   once(labeling([ffc, bisect], SearchVars))
        ->  timetable_solution_term(Classes, Rooms, SlotVars, RoomVars, Out)
        ;   Out = 'Empty'
        )
    ;   Out = 'Empty'
    ).

timetable_model(Classes, Rooms, Slots, Blocked, SlotVars, RoomVars) :-
    length(Classes, NClasses),
    length(Rooms, NRooms),
    NRooms > 0,
    length(SlotVars, NClasses),
    length(RoomVars, NClasses),
    RMax is NRooms - 1,
    RoomVars ins 0..RMax,
    list_to_fdset(Slots, SlotSet),
    maplist(slot_in_set(SlotSet), SlotVars),
    room_capacities(Rooms, Capacities),
    maplist(capacity_constraint(Capacities), Classes, RoomVars),
    apply_blocked_constraints(Classes, Blocked, SlotVars),
    pairwise_constraints(Classes, SlotVars, RoomVars).

room_capacities([], []).
room_capacities([room(_, Cap)|Rs], [Cap|Caps]) :-
    room_capacities(Rs, Caps).

slot_in_set(SlotSet, SlotVar) :-
    in_set(SlotVar, SlotSet).

capacity_constraint(Capacities, class(_, _, _, Size), RoomVar) :-
    Room1 #= RoomVar + 1,
    element(Room1, Capacities, Cap),
    Size #=< Cap.

apply_blocked_constraints([], _, []).
apply_blocked_constraints([Class|Classes], Blocked, [Slot|Slots]) :-
    apply_blocked_to_class(Class, Blocked, Slot),
    apply_blocked_constraints(Classes, Blocked, Slots).

apply_blocked_to_class(_, [], _).
apply_blocked_to_class(class(_, Teacher, Group, _), [B|Bs], SlotVar) :-
    (   B = blocked_teacher(T, Slot),
        Teacher == T
    ->  SlotVar #\= Slot
    ;   B = blocked_group(G, Slot),
        Group == G
    ->  SlotVar #\= Slot
    ;   true
    ),
    apply_blocked_to_class(class(_, Teacher, Group, _), Bs, SlotVar).

pairwise_constraints([], [], []).
pairwise_constraints([C|Cs], [S|Ss], [R|Rs]) :-
    pair_with_rest(C, S, R, Cs, Ss, Rs),
    pairwise_constraints(Cs, Ss, Rs).

pair_with_rest(_, _, _, [], [], []).
pair_with_rest(class(_, T1, G1, _), S1, R1, [class(_, T2, G2, _)|Cs], [S2|Ss], [R2|Rs]) :-
    (T1 == T2 -> S1 #\= S2 ; true),
    (G1 == G2 -> S1 #\= S2 ; true),
    S1 #= S2 #==> R1 #\= R2,
    pair_with_rest(class(_, T1, G1, _), S1, R1, Cs, Ss, Rs).

timetable_solution_term(Classes, Rooms, SlotVars, RoomVars, ['timetable-solution', Assignments]) :-
    room_ids(Rooms, RoomIds),
    classes_to_assignments(Classes, SlotVars, RoomVars, RoomIds, Assignments).

room_ids([], []).
room_ids([room(Id, _)|Rs], [Id|Ids]) :-
    room_ids(Rs, Ids).

classes_to_assignments([], [], [], _, []).
classes_to_assignments([class(ClassId, _, _, _)|Cs], [Slot|Slots], [RoomIdx|RoomIdxs], RoomIds, [[at, ClassId, Slot, RoomId]|As]) :-
    nth0(RoomIdx, RoomIds, RoomId),
    classes_to_assignments(Cs, Slots, RoomIdxs, RoomIds, As).

validate_timetable_inputs(ClassesRaw, RoomsRaw, SlotsRaw, BlockedRaw, Classes, Rooms, Slots, Blocked) :-
    validate_classes(ClassesRaw, Classes),
    validate_rooms(RoomsRaw, Rooms),
    validate_slots(SlotsRaw, Slots),
    validate_blocked(BlockedRaw, Slots, Blocked).

validate_classes(ClassesRaw, Classes) :-
    must_be(list, ClassesRaw),
    ClassesRaw \= [],
    maplist(parse_class, ClassesRaw, Classes).

parse_class([class, ClassId, Teacher, Group, Size], class(ClassId, Teacher, Group, Size)) :-
    must_be(atom, ClassId),
    must_be(atom, Teacher),
    must_be(atom, Group),
    must_be(integer, Size),
    (Size >= 0 -> true ; domain_error(non_negative_size, Size)),
    !.
parse_class(Term, _) :-
    domain_error(class_tuple, Term).

validate_rooms(RoomsRaw, Rooms) :-
    must_be(list, RoomsRaw),
    RoomsRaw \= [],
    maplist(parse_room, RoomsRaw, Rooms).

parse_room([room, RoomId, Capacity], room(RoomId, Capacity)) :-
    must_be(atom, RoomId),
    must_be(integer, Capacity),
    (Capacity >= 0 -> true ; domain_error(non_negative_capacity, Capacity)),
    !.
parse_room(Term, _) :-
    domain_error(room_tuple, Term).

validate_slots(SlotsRaw, Slots) :-
    must_be(list, SlotsRaw),
    SlotsRaw \= [],
    maplist(must_be(integer), SlotsRaw),
    sort(SlotsRaw, Slots).

validate_blocked(BlockedRaw, Slots, Blocked) :-
    must_be(list, BlockedRaw),
    maplist(parse_blocked(Slots), BlockedRaw, Blocked).

parse_blocked(Slots, [teacher, Teacher, Slot], blocked_teacher(Teacher, Slot)) :-
    must_be(atom, Teacher),
    must_be(integer, Slot),
    (memberchk(Slot, Slots) -> true ; domain_error(valid_slot, Slot)),
    !.
parse_blocked(Slots, [group, Group, Slot], blocked_group(Group, Slot)) :-
    must_be(atom, Group),
    must_be(integer, Slot),
    (memberchk(Slot, Slots) -> true ; domain_error(valid_slot, Slot)),
    !.
parse_blocked(_, Term, _) :-
    domain_error(blocked_tuple, Term).
