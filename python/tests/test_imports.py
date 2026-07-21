import uuid

import pytest


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


def test_import_after_use_warns(petta_instance, tmp_path, capfd):
    fn = f"late_inc_{uuid.uuid4().hex}"
    root_file = tmp_path / "root.metta"
    dependency_file = tmp_path / "dependency.metta"

    dependency_file.write_text(f"(= ({fn} $x) (+ $x 1))\n")
    root_file.write_text(
        f"(= (uses-{fn} $x) ({fn} $x))\n"
        "!(import! &self dependency)\n"
        f"!(uses-{fn} 41)\n"
    )

    results = petta_instance.load_metta_file(str(root_file))
    stderr = capfd.readouterr().err

    # The definition compiled before the import treats the name as a plain symbol...
    assert f"({fn} 41)" in results
    # ...and the late registration is called out.
    assert fn in stderr
    assert "Move the import or definition above the first use" in stderr
