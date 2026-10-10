:- module(builtin_registry,
          [ builtin_spec/6,
            builtin_flat_cardinality/3,
            builtin_argument_rule/3,
            builtin_conditional_rule/3,
            builtin_contextual_typing/3,
            builtin_signature/5,
            builtin_codegen_hook/3,
            builtin_codegen_symbol/2,
            special_builtin_form/3,
            builtin_registration_exemption/2,
            builtin_implementation_exemption/2,
            validate_builtin_registry/0,
            validate_builtin_registry_schema/0,
            validate_builtin_registry_unique/0,
            validate_builtin_registry_hooks/0,
            validate_builtin_registration_coverage/0,
            validate_builtin_implementation_coverage/0
          ]).

%%% Declarative registry for language-visible builtins and compiler forms: one
%%% record per builtin carries its signature, determinism, contextual output
%%% rule and lowering. Procedural implementations stay in their subject files;
%%% the load-time validator below proves every named hook exists.
%
% builtin_spec(
%     Name/MeTTaArity,
%     implementation(metta|spaces|parser|external|compiler|foreign_promise),
%     typing(signature(Det, ArgTypes, OutType)|contextual(Hook)|untyped),
%     evaluation(eager|special),
%     cardinality(fixed(Level)|
%                 argument_sensitive(RuleId, WorstCase)|
%                 conditional(RuleId)|
%                 compiler_derived|
%                 unspecified),
%     lowering(generic|special(Hook))).
%
% `variadic` marks compiler forms whose source arity is open. Type variables in
% signature records are fresh per lookup.
%
% Cardinality is the only determinism knowledge the checker has about a
% builtin, and an unlisted one is `unspecified`. An entry describes the
% predicate (N MeTTa arguments plus the result) under the weakest calling
% convention: any argument may be unbound or ill-typed (the residual guard
% accepts an unbound variable), so a (List T) position may hold an open list,
% and the result may already be bound. An exception is not a solution: det is
% exactly one solution for every instantiation, semidet at most one. Relational
% modes are real (append/3 inverts, length/2 enumerates shapes, bool/1
% enumerates), so such builtins are not det.

% Arithmetic and numeric comparisons.
builtin_spec('+'/2, implementation(metta), typing(signature(unspecified, ['Number','Number'], 'Number')), evaluation(eager), cardinality(fixed(det)), lowering(special(arithmetic_native))).
builtin_spec('-'/2, implementation(metta), typing(signature(unspecified, ['Number','Number'], 'Number')), evaluation(eager), cardinality(fixed(det)), lowering(special(arithmetic_native))).
builtin_spec('*'/2, implementation(metta), typing(signature(unspecified, ['Number','Number'], 'Number')), evaluation(eager), cardinality(fixed(det)), lowering(special(arithmetic_native))).
builtin_spec('/'/2, implementation(metta), typing(signature(unspecified, ['Number','Number'], 'Number')), evaluation(eager), cardinality(fixed(det)), lowering(special(arithmetic_native))).
builtin_spec('%'/2, implementation(metta), typing(signature(unspecified, ['Number','Number'], 'Number')), evaluation(eager), cardinality(fixed(det)), lowering(special(arithmetic_native))).
builtin_spec(min/2, implementation(metta), typing(signature(unspecified, ['Number','Number'], 'Number')), evaluation(eager), cardinality(fixed(det)), lowering(special(arithmetic_native))).
builtin_spec(max/2, implementation(metta), typing(signature(unspecified, ['Number','Number'], 'Number')), evaluation(eager), cardinality(fixed(det)), lowering(special(arithmetic_native))).
builtin_spec(exp/1, implementation(metta), typing(signature(unspecified, ['Number'], 'Number')), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('pow-math'/2, implementation(metta), typing(signature(unspecified, ['Number','Number'], 'Number')), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('sqrt-math'/1, implementation(metta), typing(signature(unspecified, ['Number'], 'Number')), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('abs-math'/1, implementation(metta), typing(signature(unspecified, ['Number'], 'Number')), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('log-math'/2, implementation(metta), typing(signature(unspecified, ['Number','Number'], 'Number')), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('trunc-math'/1, implementation(metta), typing(signature(unspecified, ['Number'], 'Number')), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('ceil-math'/1, implementation(metta), typing(signature(unspecified, ['Number'], 'Number')), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('floor-math'/1, implementation(metta), typing(signature(unspecified, ['Number'], 'Number')), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('round-math'/1, implementation(metta), typing(signature(unspecified, ['Number'], 'Number')), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('sin-math'/1, implementation(metta), typing(signature(unspecified, ['Number'], 'Number')), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('cos-math'/1, implementation(metta), typing(signature(unspecified, ['Number'], 'Number')), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('tan-math'/1, implementation(metta), typing(signature(unspecified, ['Number'], 'Number')), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('asin-math'/1, implementation(metta), typing(signature(unspecified, ['Number'], 'Number')), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('acos-math'/1, implementation(metta), typing(signature(unspecified, ['Number'], 'Number')), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('atan-math'/1, implementation(metta), typing(signature(unspecified, ['Number'], 'Number')), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('isnan-math'/1, implementation(metta), typing(signature(unspecified, ['Number'], 'Bool')), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('isinf-math'/1, implementation(metta), typing(signature(unspecified, ['Number'], 'Bool')), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('min-atom'/1, implementation(metta), typing(signature(unspecified, [_], 'Number')), evaluation(eager), cardinality(argument_sensitive(nonempty_list_arg0, semidet)), lowering(generic)).
builtin_spec('max-atom'/1, implementation(metta), typing(signature(unspecified, [_], 'Number')), evaluation(eager), cardinality(argument_sensitive(nonempty_list_arg0, semidet)), lowering(generic)).
builtin_spec(sqrt/1, implementation(external), typing(untyped), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec(log/1, implementation(external), typing(untyped), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec(sin/1, implementation(external), typing(untyped), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec(cos/1, implementation(external), typing(untyped), evaluation(eager), cardinality(fixed(det)), lowering(generic)).

builtin_spec('<'/2, implementation(metta), typing(signature(unspecified, ['Number','Number'], 'Bool')), evaluation(eager), cardinality(fixed(det)), lowering(special(reified_comparison))).
builtin_spec('<='/2, implementation(metta), typing(signature(unspecified, ['Number','Number'], 'Bool')), evaluation(eager), cardinality(fixed(det)), lowering(special(reified_comparison))).
builtin_spec('>'/2, implementation(metta), typing(signature(unspecified, ['Number','Number'], 'Bool')), evaluation(eager), cardinality(fixed(det)), lowering(special(reified_comparison))).
builtin_spec('>='/2, implementation(metta), typing(signature(unspecified, ['Number','Number'], 'Bool')), evaluation(eager), cardinality(fixed(det)), lowering(special(reified_comparison))).
builtin_spec('='/2, implementation(metta), typing(signature(unspecified, [_A,_B], 'Bool')), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('=='/2, implementation(metta), typing(signature(unspecified, [_A,_B], 'Bool')), evaluation(eager), cardinality(fixed(det)), lowering(special(reified_comparison))).
builtin_spec('!='/2, implementation(metta), typing(signature(unspecified, [_A,_B], 'Bool')), evaluation(eager), cardinality(fixed(det)), lowering(special(reified_comparison))).
builtin_spec('=?'/2, implementation(metta), typing(signature(unspecified, [_A,_B], 'Bool')), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('=alpha'/2, implementation(metta), typing(signature(unspecified, [_A,_B], 'Bool')), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('=@='/2, implementation(metta), typing(signature(unspecified, [_A,_B], 'Bool')), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('@<'/2, implementation(metta), typing(signature(unspecified, [_A,_B], 'Bool')), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('@>'/2, implementation(metta), typing(signature(unspecified, [_A,_B], 'Bool')), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('@<='/2, implementation(metta), typing(signature(unspecified, [_A,_B], 'Bool')), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('@>='/2, implementation(metta), typing(signature(unspecified, [_A,_B], 'Bool')), evaluation(eager), cardinality(fixed(det)), lowering(generic)).

% CLP(FD) arithmetic and comparisons.
builtin_spec('#+'/2, implementation(metta), typing(untyped), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('#-'/2, implementation(metta), typing(untyped), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('#*'/2, implementation(metta), typing(untyped), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('#div'/2, implementation(metta), typing(untyped), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('#//'/2, implementation(metta), typing(untyped), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('#mod'/2, implementation(metta), typing(untyped), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('#min'/2, implementation(metta), typing(untyped), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('#max'/2, implementation(metta), typing(untyped), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('#<'/2, implementation(metta), typing(untyped), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('#>'/2, implementation(metta), typing(untyped), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('#='/2, implementation(metta), typing(untyped), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('#\\='/2, implementation(metta), typing(untyped), evaluation(eager), cardinality(fixed(det)), lowering(generic)).

% Boolean generators. Their worst case enumerates unbound Bool operands; the
% named argument rule upgrades calls whose operands are manifest booleans.
builtin_spec(and/2, implementation(metta), typing(signature(unspecified, ['Bool','Bool'], 'Bool')), evaluation(eager), cardinality(argument_sensitive(manifest_booleans, nondet)), lowering(generic)).
builtin_spec(or/2, implementation(metta), typing(signature(unspecified, ['Bool','Bool'], 'Bool')), evaluation(eager), cardinality(argument_sensitive(manifest_booleans, nondet)), lowering(generic)).
builtin_spec(not/1, implementation(metta), typing(signature(unspecified, ['Bool'], 'Bool')), evaluation(eager), cardinality(argument_sensitive(manifest_booleans, nondet)), lowering(generic)).
builtin_spec(xor/2, implementation(metta), typing(signature(unspecified, ['Bool','Bool'], 'Bool')), evaluation(eager), cardinality(argument_sensitive(manifest_booleans, nondet)), lowering(generic)).
builtin_spec(implies/2, implementation(metta), typing(signature(unspecified, ['Bool','Bool'], 'Bool')), evaluation(eager), cardinality(argument_sensitive(manifest_booleans, nondet)), lowering(generic)).

% Nondeterminism, spaces and reflection.
builtin_spec(superpose/1, implementation(metta), typing(signature(unspecified, [['List',T]], T)), evaluation(special), cardinality(fixed(nondet)), lowering(special(superpose_literal))).
builtin_spec(empty/0, implementation(metta), typing(signature(unspecified, [], _)), evaluation(eager), cardinality(fixed(semidet)), lowering(generic)).
builtin_spec('get-atoms'/1, implementation(spaces), typing(untyped), evaluation(eager), cardinality(fixed(nondet)), lowering(generic)).
builtin_spec(match/3, implementation(spaces), typing(untyped), evaluation(special), cardinality(fixed(nondet)), lowering(special(typed_space_match))).
builtin_spec('add-atom'/2, implementation(spaces), typing(untyped), evaluation(special), cardinality(argument_sensitive(space_update, semidet)), lowering(special(typed_space_update))).
builtin_spec('remove-atom'/2, implementation(spaces), typing(untyped), evaluation(special), cardinality(argument_sensitive(space_update, semidet)), lowering(special(typed_space_update))).
builtin_spec('get-type'/1, implementation(metta), typing(untyped), evaluation(eager), cardinality(fixed(nondet)), lowering(generic)).
builtin_spec('get-metatype'/1, implementation(metta), typing(untyped), evaluation(eager), cardinality(fixed(semidet)), lowering(generic)).
builtin_spec('is-var'/1, implementation(metta), typing(signature(unspecified, [_], 'Bool')), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('is-ground'/1, implementation(metta), typing(signature(unspecified, [_], 'Bool')), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('is-expr'/1, implementation(metta), typing(signature(unspecified, [_], 'Bool')), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('is-space'/1, implementation(metta), typing(signature(unspecified, [_], 'Bool')), evaluation(eager), cardinality(fixed(det)), lowering(generic)).

% Lists and tuples. `contextual` means the output type is supplied by the
% named procedural rule in clause_checks.pl instead of a global declaration.
% A signature would be consulted before that rule and replace what the
% argument knows: (cdr-atom $xs) on a (List Choice) would come back as
% (List %Undefined%), and a global (List $item) for append would reject legal
% heterogeneous lists. The rules keep element types and reject nothing.
builtin_spec(cons/2, implementation(metta), typing(contextual(cons_list)), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('cons-atom'/2, implementation(metta), typing(contextual(cons_list)), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('decons-atom'/1, implementation(metta), typing(untyped), evaluation(eager), cardinality(fixed(semidet)), lowering(generic)).
builtin_spec(decons/1, implementation(metta), typing(untyped), evaluation(eager), cardinality(fixed(semidet)), lowering(generic)).
builtin_spec('first-from-pair'/1, implementation(metta), typing(untyped), evaluation(eager), cardinality(fixed(semidet)), lowering(generic)).
builtin_spec('second-from-pair'/1, implementation(metta), typing(untyped), evaluation(eager), cardinality(fixed(semidet)), lowering(generic)).
builtin_spec(first/1, implementation(metta), typing(contextual(list_element)), evaluation(eager), cardinality(fixed(semidet)), lowering(generic)).
builtin_spec(last/1, implementation(external), typing(contextual(list_element)), evaluation(eager), cardinality(argument_sensitive(nonempty_list_arg0, nondet)), lowering(generic)).
builtin_spec('car-atom'/1, implementation(metta), typing(contextual(list_element)), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('cdr-atom'/1, implementation(metta), typing(contextual(list_tail)), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('index-atom'/2, implementation(metta), typing(untyped), evaluation(eager), cardinality(argument_sensitive(manifest_indexed_list, nondet)), lowering(generic)).
builtin_spec(member/2, implementation(metta), typing(untyped), evaluation(eager), cardinality(fixed(nondet)), lowering(generic)).
builtin_spec('is-member'/2, implementation(metta), typing(signature(unspecified, [_A,_B], 'Bool')), evaluation(eager), cardinality(argument_sensitive(bound_membership_probe, nondet)), lowering(generic)).
builtin_spec('is-alpha-member'/2, implementation(metta), typing(signature(unspecified, [_A,_B], 'Bool')), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('exclude-item'/2, implementation(metta), typing(untyped), evaluation(eager), cardinality(argument_sensitive(proper_list_arg1, nondet)), lowering(generic)).
builtin_spec('union-atom'/2, implementation(metta), typing(contextual(union_list)), evaluation(eager), cardinality(argument_sensitive(proper_list_arg0, nondet)), lowering(generic)).
builtin_spec(append/2, implementation(external), typing(contextual(union_list)), evaluation(eager), cardinality(argument_sensitive(proper_list_arg0, nondet)), lowering(generic)).
builtin_spec('subtraction-atom'/2, implementation(metta), typing(contextual(first_list)), evaluation(eager), cardinality(argument_sensitive(proper_list_arg0, nondet)), lowering(generic)).
builtin_spec('intersection-atom'/2, implementation(metta), typing(untyped), evaluation(eager), cardinality(argument_sensitive(proper_list_arg0, nondet)), lowering(generic)).
builtin_spec(list_to_set/1, implementation(external), typing(contextual(first_list)), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('size-atom'/1, implementation(metta), typing(signature(unspecified, [_], 'Number')), evaluation(eager), cardinality(argument_sensitive(proper_list_arg0, nondet)), lowering(generic)).
builtin_spec(length/1, implementation(external), typing(signature(unspecified, [_], 'Number')), evaluation(eager), cardinality(argument_sensitive(proper_list_arg0, nondet)), lowering(generic)).
builtin_spec(reverse/1, implementation(external), typing(untyped), evaluation(eager), cardinality(argument_sensitive(proper_list_arg0, nondet)), lowering(generic)).
builtin_spec('alpha-unique-atom'/1, implementation(metta), typing(untyped), evaluation(eager), cardinality(argument_sensitive(proper_list_arg0, nondet)), lowering(generic)).
builtin_spec('unique-atom'/1, implementation(metta), typing(untyped), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('sort-atom'/1, implementation(metta), typing(untyped), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec(sort/1, implementation(external), typing(untyped), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec(msort/1, implementation(external), typing(untyped), evaluation(eager), cardinality(fixed(det)), lowering(generic)).

% Representation, state, diagnostics and foreign calls.
builtin_spec(id/1, implementation(metta), typing(untyped), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec(repr/1, implementation(metta), typing(signature(unspecified, [_], 'String')), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec(repra/1, implementation(metta), typing(signature(unspecified, [_], 'String')), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec(parse/1, implementation(metta), typing(untyped), evaluation(eager), cardinality(unspecified), lowering(generic)).
builtin_spec(atom_chars/1, implementation(external), typing(untyped), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec(atom_concat/2, implementation(external), typing(untyped), evaluation(eager), cardinality(unspecified), lowering(generic)).
builtin_spec(copy_term/1, implementation(external), typing(signature(unspecified, [A], A)), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec(term_hash/1, implementation(external), typing(untyped), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('change-state!'/2, implementation(metta), typing(signature(unspecified, [_,_], 'Bool')), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('get-state'/1, implementation(metta), typing(untyped), evaluation(eager), cardinality(fixed(semidet)), lowering(generic)).
builtin_spec('bind!'/2, implementation(metta), typing(untyped), evaluation(eager), cardinality(fixed(semidet)), lowering(generic)).
builtin_spec('println!'/1, implementation(metta), typing(signature(unspecified, [_], 'Bool')), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('readln!'/0, implementation(metta), typing(untyped), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec(test/2, implementation(metta), typing(untyped), evaluation(special), cardinality(fixed(det)), lowering(special(test_collect))).
builtin_spec(assert/1, implementation(metta), typing(signature(unspecified, ['%Undefined%'], 'Bool')), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('current-time'/0, implementation(metta), typing(untyped), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('format-time'/1, implementation(metta), typing(untyped), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('random-int'/2, implementation(metta), typing(signature(unspecified, ['Number','Number'], 'Number')), evaluation(eager), cardinality(fixed(semidet)), lowering(generic)).
builtin_spec('random-int'/3, implementation(metta), typing(untyped), evaluation(eager), cardinality(unspecified), lowering(generic)).
builtin_spec('random-float'/2, implementation(metta), typing(signature(unspecified, ['Number','Number'], 'Number')), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('random-float'/3, implementation(metta), typing(untyped), evaluation(eager), cardinality(unspecified), lowering(generic)).
builtin_spec('py-call'/1, implementation(metta), typing(untyped), evaluation(eager), cardinality(unspecified), lowering(generic)).
builtin_spec('py-call'/2, implementation(metta), typing(untyped), evaluation(eager), cardinality(unspecified), lowering(generic)).
builtin_spec(argv/1, implementation(metta), typing(untyped), evaluation(eager), cardinality(unspecified), lowering(generic)).
builtin_spec(import_prolog_function/1, implementation(metta), typing(untyped), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('Predicate'/1, implementation(metta), typing(untyped), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec(callPredicate/1, implementation(metta), typing(untyped), evaluation(eager), cardinality(argument_sensitive(manifest_foreign_goal, nondet)), lowering(generic)).
builtin_spec(assertaPredicate/1, implementation(metta), typing(untyped), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec(assertzPredicate/1, implementation(metta), typing(untyped), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec(retractPredicate/1, implementation(metta), typing(untyped), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
% Trusted foreign promises: determinism arrows for Prolog predicates called
% through (callPredicate (Predicate (g ...))). No MeTTa clauses exist for these
% symbols, so the arrows are believed, not validated.
builtin_spec(assertz/2, implementation(foreign_promise), typing(signature(det, [_A,_B], 'Bool')), evaluation(eager), cardinality(unspecified), lowering(generic)).
builtin_spec(erase/1, implementation(foreign_promise), typing(signature(det, [_], 'Bool')), evaluation(eager), cardinality(unspecified), lowering(generic)).
builtin_spec(heap_size/1, implementation(external), typing(signature(unspecified, [_], 'Number')), evaluation(eager), cardinality(unspecified), lowering(generic)).

% Higher-order runtime forms. Their cardinality is derived procedurally from
% list shape and closure/body effect, so there is deliberately no flat level.
builtin_spec('foldl-atom'/3, implementation(metta), typing(untyped), evaluation(eager), cardinality(conditional(higher_order_list)), lowering(generic)).
builtin_spec('foldl-atom'/5, implementation(compiler), typing(untyped), evaluation(special), cardinality(conditional(higher_order_list)), lowering(special(foldl_pseudo_lambda))).
builtin_spec('map-atom'/2, implementation(metta), typing(untyped), evaluation(eager), cardinality(conditional(higher_order_list)), lowering(generic)).
builtin_spec('map-atom'/3, implementation(compiler), typing(untyped), evaluation(special), cardinality(conditional(higher_order_list)), lowering(special(map_pseudo_lambda))).
builtin_spec('filter-atom'/2, implementation(metta), typing(untyped), evaluation(eager), cardinality(conditional(higher_order_list)), lowering(generic)).
builtin_spec('filter-atom'/3, implementation(compiler), typing(untyped), evaluation(special), cardinality(conditional(higher_order_list)), lowering(special(filter_pseudo_lambda))).
builtin_spec(foldl/3, implementation(external), typing(untyped), evaluation(eager), cardinality(unspecified), lowering(generic)).
builtin_spec(maplist/1, implementation(external), typing(untyped), evaluation(eager), cardinality(unspecified), lowering(generic)).
builtin_spec(maplist/2, implementation(external), typing(untyped), evaluation(eager), cardinality(unspecified), lowering(generic)).
builtin_spec(maplist/3, implementation(external), typing(untyped), evaluation(eager), cardinality(unspecified), lowering(generic)).
builtin_spec(maplist/4, implementation(external), typing(untyped), evaluation(eager), cardinality(unspecified), lowering(generic)).

% Imports and dynamic evaluation.
builtin_spec(eval/1, implementation(metta), typing(untyped), evaluation(special), cardinality(unspecified), lowering(special(eval_source))).
builtin_spec(reduce/1, implementation(external), typing(untyped), evaluation(special), cardinality(unspecified), lowering(special(dynamic_reduce))).
builtin_spec('import!'/2, implementation(metta), typing(untyped), evaluation(eager), cardinality(unspecified), lowering(generic)).
builtin_spec('library-import!'/2, implementation(metta), typing(untyped), evaluation(eager), cardinality(unspecified), lowering(generic)).
builtin_spec('cached-import!'/2, implementation(metta), typing(untyped), evaluation(eager), cardinality(unspecified), lowering(generic)).
builtin_spec(library/1, implementation(metta), typing(untyped), evaluation(eager), cardinality(unspecified), lowering(generic)).
builtin_spec(library/2, implementation(metta), typing(untyped), evaluation(eager), cardinality(unspecified), lowering(generic)).
builtin_spec(sread/1, implementation(parser), typing(untyped), evaluation(eager), cardinality(unspecified), lowering(generic)).
builtin_spec('add-translator-rule!'/1, implementation(metta), typing(untyped), evaluation(eager), cardinality(fixed(det)), lowering(generic)).
builtin_spec('remove-translator-rule!'/1, implementation(metta), typing(untyped), evaluation(eager), cardinality(fixed(det)), lowering(generic)).

% Compiler-only forms. These have no ordinary eager predicate call.
builtin_spec(data/variadic, implementation(compiler), typing(untyped), evaluation(special), cardinality(compiler_derived), lowering(special(explicit_data))).
builtin_spec('make-list'/variadic, implementation(compiler), typing(untyped), evaluation(special), cardinality(compiler_derived), lowering(special(explicit_list))).
builtin_spec(collapse/1, implementation(compiler), typing(signature(unspecified, [_], ['List','%Undefined%'])), evaluation(special), cardinality(compiler_derived), lowering(special(collapse_all))).
builtin_spec(cut/0, implementation(compiler), typing(untyped), evaluation(special), cardinality(compiler_derived), lowering(special(cut))).
builtin_spec(once/1, implementation(compiler), typing(untyped), evaluation(special), cardinality(compiler_derived), lowering(special(once))).
builtin_spec(hyperpose/1, implementation(compiler), typing(untyped), evaluation(special), cardinality(compiler_derived), lowering(special(hyperpose))).
builtin_spec(with_mutex/2, implementation(compiler), typing(untyped), evaluation(special), cardinality(compiler_derived), lowering(special(with_mutex))).
builtin_spec(transaction/1, implementation(compiler), typing(untyped), evaluation(special), cardinality(compiler_derived), lowering(special(transaction))).
builtin_spec(progn/variadic, implementation(compiler), typing(untyped), evaluation(special), cardinality(compiler_derived), lowering(special(progn))).
builtin_spec(prog1/variadic, implementation(compiler), typing(untyped), evaluation(special), cardinality(compiler_derived), lowering(special(prog1))).
builtin_spec(if/2, implementation(compiler), typing(untyped), evaluation(special), cardinality(compiler_derived), lowering(special(if_then))).
builtin_spec(if/3, implementation(compiler), typing(untyped), evaluation(special), cardinality(compiler_derived), lowering(special(if_then_else))).
builtin_spec(case/2, implementation(compiler), typing(untyped), evaluation(special), cardinality(compiler_derived), lowering(special(case))).
builtin_spec('and-then'/2, implementation(compiler), typing(untyped), evaluation(special), cardinality(compiler_derived), lowering(special(and_then))).
builtin_spec('or-else'/2, implementation(compiler), typing(untyped), evaluation(special), cardinality(compiler_derived), lowering(special(or_else))).
builtin_spec(let/3, implementation(compiler), typing(untyped), evaluation(special), cardinality(compiler_derived), lowering(special(let_bind))).
builtin_spec(chain/3, implementation(compiler), typing(untyped), evaluation(special), cardinality(compiler_derived), lowering(special(let_bind))).
builtin_spec('let*'/2, implementation(compiler), typing(untyped), evaluation(special), cardinality(compiler_derived), lowering(special(let_star))).
builtin_spec(sealed/2, implementation(compiler), typing(untyped), evaluation(special), cardinality(compiler_derived), lowering(special(sealed))).
builtin_spec(forall/2, implementation(compiler), typing(untyped), evaluation(special), cardinality(compiler_derived), lowering(special(forall))).
builtin_spec(foldall/3, implementation(compiler), typing(untyped), evaluation(special), cardinality(compiler_derived), lowering(special(foldall))).
builtin_spec('|->'/2, implementation(compiler), typing(untyped), evaluation(special), cardinality(compiler_derived), lowering(special(lambda))).
builtin_spec(translatePredicate/1, implementation(compiler), typing(untyped), evaluation(special), cardinality(compiler_derived), lowering(special(translate_predicate))).
builtin_spec(call/1, implementation(compiler), typing(untyped), evaluation(special), cardinality(compiler_derived), lowering(special(manual_call))).
builtin_spec(brand/2, implementation(compiler), typing(untyped), evaluation(special), cardinality(compiler_derived), lowering(special(brand))).
builtin_spec(the/2, implementation(compiler), typing(untyped), evaluation(special), cardinality(compiler_derived), lowering(special(type_ascription))).
builtin_spec(quote/1, implementation(compiler), typing(untyped), evaluation(special), cardinality(compiler_derived), lowering(special(quote))).
builtin_spec(catch/1, implementation(compiler), typing(untyped), evaluation(special), cardinality(compiler_derived), lowering(special(catch))).

% Exemptions from registration completeness: names in register_fun's list that
% do not implement the N-arguments-plus-result convention.
builtin_registration_exemption(concat, legacy_name_without_predicate).
builtin_registration_exemption('get-mettatype', legacy_name_without_predicate).
builtin_registration_exemption('mm2-exec', optional_mork_runtime).
builtin_registration_exemption(set_hook, host_hook).
builtin_registration_exemption(exists_file, host_predicate_without_result_argument).

% Helpers implemented in metta.pl that are intentionally not language
% builtins. The exact Prolog indicators make the completeness audit fail when a
% new implementation is added without either a registry record or a reason.
builtin_implementation_exemption('$autoload'/3, generated_autoload_wrapper).
builtin_implementation_exemption(alpha_list_to_set/2, alpha_unique_helper).
builtin_implementation_exemption(alpha_list_to_set_assoc/3, alpha_unique_helper).
builtin_implementation_exemption(attribute_free_variant/2, identity_helper).
builtin_implementation_exemption(bool/1, boolean_domain_helper).
builtin_implementation_exemption(call_goals/1, evaluator_helper).
builtin_implementation_exemption(claim_import/3, import_helper).
builtin_implementation_exemption(clear_import_state/2, import_helper).
builtin_implementation_exemption(current_working_dir/1, import_helper).
builtin_implementation_exemption(ensure_metta_ext/2, import_helper).
builtin_implementation_exemption(fun/1, registry_runtime_state).
builtin_implementation_exemption(arity/2, registry_runtime_state).
builtin_implementation_exemption(translator_rule/1, translator_runtime_state).
builtin_implementation_exemption(get_function_type/2, legacy_type_helper).
builtin_implementation_exemption(get_type_candidate/2, legacy_type_helper).
builtin_implementation_exemption(import_error_propagates/1, import_helper).
builtin_implementation_exemption(import_file_string/2, import_helper).
builtin_implementation_exemption(import_once/3, import_helper).
builtin_implementation_exemption(importer_helper/2, import_helper).
builtin_implementation_exemption(library_path/1, import_runtime_state).
builtin_implementation_exemption(library_path_candidate/2, import_helper).
builtin_implementation_exemption(library_path_candidate/3, import_helper).
builtin_implementation_exemption(library_source_exists/1, import_helper).
builtin_implementation_exemption(mark_import_loaded/2, import_helper).
builtin_implementation_exemption(metta_import_state/3, import_runtime_state).
builtin_implementation_exemption(member_alpha/2, alpha_membership_helper).
builtin_implementation_exemption(non_list/1, list_guard_helper).
builtin_implementation_exemption(py_bool_norm/2, python_interop_helper).
builtin_implementation_exemption(python_import_file/1, import_helper).
builtin_implementation_exemption(register_fun/1, registration_helper).
builtin_implementation_exemption(resolve_library_path/2, import_helper).
builtin_implementation_exemption(resolve_existing_import_path/3, import_helper).
builtin_implementation_exemption(resolve_metta_import_path/2, import_helper).
builtin_implementation_exemption(resolve_python_import_path/2, import_helper).
builtin_implementation_exemption(run_new_import/3, import_helper).
builtin_implementation_exemption(standard_library_path/1, import_runtime_state).
builtin_implementation_exemption(throw_missing_import/1, import_helper).

%%% Consumer views.

builtin_flat_cardinality(F, N, Det) :-
    builtin_spec(F/N, _, _, _, cardinality(fixed(Det)), _).
builtin_flat_cardinality(F, N, Det) :-
    builtin_spec(F/N, _, _, _, cardinality(argument_sensitive(_, Det)), _).

builtin_argument_rule(F, N, Rule) :-
    builtin_spec(F/N, _, _, _, cardinality(argument_sensitive(Rule, _)), _).

builtin_conditional_rule(F, N, Rule) :-
    builtin_spec(F/N, _, _, _, cardinality(conditional(Rule)), _).

builtin_contextual_typing(F, N, Hook) :-
    builtin_spec(F/N, _, typing(contextual(Hook)), _, _, _).

builtin_signature(F, N, Det, ArgTypes, OutType) :-
    builtin_spec(F/N, _, typing(signature(Det, ArgTypes, OutType)), _, _, _).

builtin_codegen_hook(F, N, Hook) :-
    builtin_spec(F/N, _, _, _, _, lowering(special(Hook))).

builtin_codegen_symbol(F, Hook) :-
    atom(F),
    builtin_spec(F/_, _, _, _, _, lowering(special(Hook))).

special_builtin_form(F, Args, Hook) :-
    atom(F),
    length(Args, N),
    ( builtin_spec(F/N, _, _, evaluation(special), _, lowering(special(Hook)))
    ; builtin_spec(F/variadic, _, _, evaluation(special), _, lowering(special(Hook))) ).

%%% Registry integrity. Called after metta.pl has registered its builtin list.

validate_builtin_registry :-
    validate_builtin_registry_schema,
    validate_builtin_registry_unique,
    validate_builtin_registry_hooks,
    validate_builtin_registration_coverage,
    validate_builtin_implementation_coverage.

validate_builtin_registry_schema :-
    forall(builtin_spec(Key, Implementation, Typing, Evaluation, Cardinality, Lowering),
           valid_builtin_spec(Key, Implementation, Typing, Evaluation, Cardinality, Lowering)).

valid_builtin_spec(F/N, implementation(I), typing(T), evaluation(E), cardinality(C), lowering(L)) :-
    atom(F), ( integer(N), N >= 0 ; N == variadic ),
    memberchk(I, [metta,spaces,parser,external,compiler,foreign_promise]),
    valid_builtin_typing(T),
    memberchk(E, [eager,special]),
    valid_builtin_cardinality(C),
    valid_builtin_lowering(L), !.
valid_builtin_spec(Key, Implementation, Typing, Evaluation, Cardinality, Lowering) :-
    throw(error(invalid_builtin_spec(Key, Implementation, Typing, Evaluation, Cardinality, Lowering),
                builtin_registry)).

valid_builtin_typing(untyped).
valid_builtin_typing(contextual(Hook)) :- atom(Hook).
valid_builtin_typing(signature(Det, Args, _)) :-
    memberchk(Det, [unspecified,det,semidet,nondet]),
    is_list(Args).

valid_builtin_cardinality(fixed(Level)) :- memberchk(Level, [det,semidet,nondet]).
valid_builtin_cardinality(argument_sensitive(Rule, Worst)) :-
    atom(Rule), memberchk(Worst, [det,semidet,nondet]).
valid_builtin_cardinality(conditional(Rule)) :- atom(Rule).
valid_builtin_cardinality(compiler_derived).
valid_builtin_cardinality(unspecified).

valid_builtin_lowering(generic).
valid_builtin_lowering(special(Hook)) :- atom(Hook).

validate_builtin_registry_unique :-
    findall(Key, builtin_spec(Key, _, _, _, _, _), Keys),
    sort(Keys, Unique),
    length(Keys, N), length(Unique, N), !.
validate_builtin_registry_unique :-
    throw(error(duplicate_builtin_registry_key, builtin_registry)).

%Every named hook must have a dispatch clause of its own: a clause keyed by
%the rule name, a deterministic_expr_core/2 clause for a conditional builtin's
%exact form, or a lowering test for the hook in the translator.
validate_builtin_registry_hooks :-
    forall(builtin_argument_rule(_, _, Rule),
           ( clause(user:builtin_argument_rule_verdict(Key, _, _, _), _), Key == Rule
             -> true
             ; throw(error(undefined_builtin_argument_rule(Rule), builtin_registry)) )),
    forall(builtin_conditional_rule(F, N, Rule),
           ( clause(user:deterministic_expr_core([Key|Args], _), _), Key == F,
             is_list(Args), length(Args, N)
             -> true
             ; throw(error(undefined_builtin_conditional_rule(Rule), builtin_registry)) )),
    forall(builtin_contextual_typing(_, _, Hook),
           ( clause(user:builtin_contextual_output_rule(Key, _, _), _), Key == Hook
             -> true
             ; throw(error(undefined_builtin_contextual_typing_rule(Hook), builtin_registry)) )),
    source_file(user:translate_expr(_, _, _), Translator),
    findall(Hook,
            ( source_file(user:Head, Translator),
              clause(user:Head, Body),
              sub_term(Test, Body), nonvar(Test),
              lowering_test(Test, Hook), atom(Hook) ),
            Lowered),
    forall(builtin_codegen_hook(_, _, Hook),
           ( memberchk(Hook, Lowered)
             -> true
             ; throw(error(undefined_builtin_codegen_rule(Hook), builtin_registry)) )).

lowering_test(special_builtin_form(_, _, Hook), Hook).
lowering_test(builtin_codegen_hook(_, _, Hook), Hook).
lowering_test(builtin_codegen_symbol(_, Hook), Hook).

validate_builtin_registration_coverage :-
    forall(user:fun(F),
           ( builtin_spec(F/_, _, _, _, _, _)
             -> true
           ; builtin_registration_exemption(F, _)
             -> true
           ; throw(error(unregistered_builtin_spec(F), builtin_registry)) )).

validate_builtin_implementation_coverage :-
    source_file(user:register_fun(_), File),
    forall(metta_source_predicate(File, F, A),
           ( builtin_implementation_exemption(F/A, _)
             -> true
           ; N is A - 1, N >= 0,
             builtin_spec(F/N, implementation(metta), _, _, _, _)
             -> true
           ; throw(error(unregistered_metta_builtin_implementation(F/A), builtin_registry)) )).

metta_source_predicate(File, F, A) :-
    source_file(user:H0, File),
    functor(H0, F, A),
    functor(H, F, A),
    catch(( clause(user:H, _, Ref), clause_property(Ref, file(File)) ), _, fail).
