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

def test_nested_relative_imports_use_importer_directory(petta_instance, tmp_path):
    main_dir = tmp_path / "main"
    libs_dir = tmp_path / "libs"
    main_dir.mkdir()
    libs_dir.mkdir()

    root_file = main_dir / "root.metta"
    parent_file = libs_dir / "parent.metta"
    sibling_file = libs_dir / "sibling.metta"

    root_file.write_text("!(import! &self ../libs/parent)\n!(from-sibling)\n")
    parent_file.write_text("!(import! &self sibling)\n")
    sibling_file.write_text("(= (from-sibling) 42)\n")

    results = petta_instance.load_metta_file(str(root_file))
    assert any(result == "42" for result in results), f"Expected sibling import to resolve; got {results}"
