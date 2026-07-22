import uuid

import pytest


def write_increment_dependency(tmp_path):
    """Write dependency.metta defining a fresh increment function; return its name and the root path."""
    fn = f"late_inc_{uuid.uuid4().hex}"
    dependency_file = tmp_path / "dependency.metta"
    dependency_file.write_text(f"(= ({fn} $x) (+ $x 1))\n")
    return fn, tmp_path / "root.metta"


def test_failed_import_can_be_retried(petta_instance, tmp_path):
    module_name = f"petta_retry_{uuid.uuid4().hex}"
    root_file = tmp_path / "root.metta"
    dependency_file = tmp_path / "dependency.metta"
    python_file = tmp_path / f"{module_name}.py"

    root_file.write_text("!(import! &self dependency)\n!(retry-result)\n")
    dependency_file.write_text(
        f'!(import! &self "{module_name}.py")\n(= (retry-result) retry-ok)\n'
    )
    python_file.write_text('raise RuntimeError("first import fails")\n')

    with pytest.raises(Exception, match="first import fails"):
        petta_instance.load_metta_file(str(root_file))

    python_file.write_text("RETRY_SUCCEEDED = True\n")
    results = petta_instance.load_metta_file(str(root_file))

    assert "retry-ok" in results


def test_definition_before_import_resolves(petta_instance, tmp_path, capfd):
    fn, root_file = write_increment_dependency(tmp_path)
    root_file.write_text(
        f"(= (uses-{fn} $x) ({fn} $x))\n"
        "!(import! &self dependency)\n"
        f"!(uses-{fn} 41)\n"
    )

    results = petta_instance.load_metta_file(str(root_file))
    stderr = capfd.readouterr().err

    # The definition compiled before the import is recompiled when the import
    # registers the function, so the cross-file call reduces regardless of order.
    assert "42" in results
    assert "Move the import or definition above the first use" not in stderr


def test_definition_before_dynamic_import_resolves(petta_instance, tmp_path):
    # The import target is computed at runtime, so no scan could know it upfront;
    # the definition still heals when the loaded file registers the function.
    fn, root_file = write_increment_dependency(tmp_path)
    root_file.write_text(
        f"(= (uses-{fn} $x) ({fn} $x))\n"
        "(= (dynamic-import-path) dependency)\n"
        "!(import! &self (dynamic-import-path))\n"
        f"!(uses-{fn} 41)\n"
    )

    results = petta_instance.load_metta_file(str(root_file))

    assert "42" in results


def test_execution_before_import_warns(petta_instance, tmp_path, capfd):
    fn, root_file = write_increment_dependency(tmp_path)
    root_file.write_text(
        f"!({fn} 41)\n"
        "!(import! &self dependency)\n"
        f"!({fn} 41)\n"
    )

    results = petta_instance.load_metta_file(str(root_file))
    stderr = capfd.readouterr().err

    # The call that ran before the import treated the name as a plain symbol...
    assert f"({fn} 41)" in results
    # ...the same call after the import reduces...
    assert "42" in results
    # ...and the unrepairable early execution is called out.
    assert fn in stderr
    assert "Move the import or definition above the first use" in stderr
