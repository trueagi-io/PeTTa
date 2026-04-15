import uuid

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

def test_duplicate_import_is_ignored(petta_instance, tmp_path):
    imported_file = tmp_path / "imported_lib.metta"
    root_file = tmp_path / "root.metta"

    imported_file.write_text("")
    root_file.write_text("!(import! &self imported_lib)\n!(import! &self imported_lib)\n")

    results = petta_instance.load_metta_file(str(root_file))

    assert isinstance(results, list)
