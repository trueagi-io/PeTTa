import uuid
from unittest.mock import Mock

import pytest


@pytest.mark.parametrize(
    ("verbose", "expected_binding"),
    [(False, "false"), (True, "true")],
)
def test_run_helper_binds_verbose_atom(
    monkeypatch, petta_module, verbose, expected_binding
):
    query_once = Mock(return_value={"Results": []})
    monkeypatch.setattr(petta_module, "CONSULTED", True)
    monkeypatch.setattr(petta_module, "janus", Mock(query_once=query_once))

    petta_module.PeTTa(verbose=verbose).process_metta_string("!(+ 1 1)")

    query_once.assert_called_once_with(
        "run_metta_helper(Verbose, HelperName, Argument, Results)",
        {
            "Verbose": expected_binding,
            "HelperName": "process_metta_string",
            "Argument": "!(+ 1 1)",
        },
    )


def test_load_metta_file_returns_list(petta_instance, dummy_metta_path):
    results = petta_instance.load_metta_file(str(dummy_metta_path))
    assert isinstance(results, list)
    assert results[1] == '(2 3 4)'

def test_process_metta_string_matches_verbose(petta_instance, petta_verbose):
    func_name = f"pytest_identity_{uuid.uuid4().hex}"
    definition = f"(= ({func_name} $x) $x)"
    invocation = f"!({func_name} 42)"
    petta_instance.process_metta_string(definition)
    results_default = petta_instance.process_metta_string(invocation)
    results_verbose = petta_verbose.process_metta_string(invocation)
    assert results_default == results_verbose
    assert results_default[0] == '42'

def test_var_out(petta_instance):
    metta = "(= (fun ($a x)) ($b x)) !(fun (a x))"
    res = petta_instance.process_metta_string(metta)
    assert res, "Expected at least one result from MeTTa execution."
    result = res[0]
    tokens = result.strip('()').split()
    assert len(tokens) == 2, f"Unexpected result format: {result}"
    var, arg = tokens
    assert arg == 'x', f"Expected argument 'x', got '{arg}'"
    assert (
        var == '$b' or (var.startswith('$_') and var[2:].isdigit())
    ), f"Unexpected variable name '{var}' in result '{result}'"
