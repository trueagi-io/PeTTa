:- ensure_loaded('../src/metta').
:- retractall(silent(_)), assertz(silent(true)).

:- begin_tests(datatypes).

test(malformed_declarations, [forall(member(Source, [
        "(datatype)",
        "(datatype Empty)",
        "(datatype $type ctor)",
        "(datatype BadCtor ())",
        "(datatype BadName (123 Number))",
        "(datatype Duplicate same (same Number))",
        "(datatype Self Self)",
        "(datatype (BadParam Number) ctor)",
        "(datatype (Repeated $a $a) ctor)"
    ])), throws(error(syntax_error(_), _))]) :-
    process_metta_string(Source, _).

test(invalid_form_has_no_partial_declarations) :-
    catch(process_metta_string("(datatype NoPartial valid (123 Number))", _),
          error(syntax_error(_), _), true),
    \+ declared_value_type('NoPartial', _),
    \+ declared_value_type(valid, _).

test(expands_in_the_requested_space) :-
    process_metta_string("(datatype Spaced spaced-none (spaced-some Number))",
                         [], '&datatype-test'),
    '&datatype-test'(:, 'Spaced', 'Type'),
    '&datatype-test'(:, 'spaced-none', 'Spaced'),
    '&datatype-test'(:, 'spaced-some', ['-[det]->', 'Number', 'Spaced']),
    \+ declared_value_type('Spaced', _).

test(nested_data_is_not_expanded) :-
    process_metta_string("(holder (datatype Nested nested-none))", [], '&datatype-data'),
    '&datatype-data'(holder, [datatype, 'Nested', 'nested-none']),
    \+ declared_value_type('Nested', _).

test(runtime_data_is_not_expanded) :-
    'add-atom'('&datatype-runtime', [datatype, 'RuntimeData', 'runtime-none'], true),
    '&datatype-runtime'(datatype, 'RuntimeData', 'runtime-none'),
    \+ declared_value_type('RuntimeData', _).

test(value_declarations_keep_source_order, Results == ['Order']) :-
    process_metta_string("!(match &self (: order-none Order) Order)
                         (datatype Order order-none)
                         !(match &self (: order-none Order) Order)", Results).

:- end_tests(datatypes).
