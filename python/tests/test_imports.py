import builtins
import concurrent.futures
import time
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


def test_python_import_uses_canonical_path(petta_instance, tmp_path):
    module_name = f"same_name_{uuid.uuid4().hex}"
    event_name = f"PETTA_IMPORT_EVENTS_{uuid.uuid4().hex}"
    left = tmp_path / "left"
    right = tmp_path / "right"
    left.mkdir()
    right.mkdir()
    setattr(builtins, event_name, [])

    try:
        for directory, value in ((left, "left"), (right, "right")):
            (directory / f"{module_name}.py").write_text(
                "import builtins\n"
                f"builtins.{event_name}.append({value!r})\n"
            )
            (directory / "root.metta").write_text(
                f'!(import! &self "{module_name}.py")\n'
            )

        petta_instance.load_metta_file(str(left / "root.metta"))
        petta_instance.load_metta_file(str(right / "root.metta"))

        assert getattr(builtins, event_name) == ["left", "right"]
    finally:
        delattr(builtins, event_name)


def test_all_overloads_are_registered_before_repair(petta_instance, tmp_path):
    function_name = f"overloaded_{uuid.uuid4().hex}"
    caller_name = f"caller_{uuid.uuid4().hex}"
    (tmp_path / "dependency.metta").write_text(
        f"(= ({function_name} $x) one)\n"
        f"(= ({function_name} $x $y $z) three)\n"
    )
    root = tmp_path / "root.metta"
    root.write_text(
        f"(= ({caller_name}) ({function_name} 1 2))\n"
        "!(import! &self dependency)\n"
        f"!({caller_name} 3)\n"
    )

    results = petta_instance.load_metta_file(str(root))

    assert "three" in results


def test_missing_relative_import_does_not_fall_back_to_cwd(
    petta_instance, tmp_path, monkeypatch
):
    fallback_name = f"cwd_fallback_{uuid.uuid4().hex}"
    (tmp_path / "dependency.metta").write_text(
        f"(= ({fallback_name}) wrong-cwd)\n"
    )
    child = tmp_path / "sub"
    child.mkdir()
    root = child / "root.metta"
    root.write_text("!(import! &self dependency)\n")
    monkeypatch.chdir(tmp_path)

    with pytest.raises(Exception, match="source_sink"):
        petta_instance.load_metta_file(str(root))


def test_concurrent_import_waits_for_the_loading_thread(petta_instance, tmp_path):
    module_name = f"slow_import_{uuid.uuid4().hex}"
    event_name = f"PETTA_IMPORT_EVENTS_{uuid.uuid4().hex}"
    setattr(builtins, event_name, [])
    (tmp_path / f"{module_name}.py").write_text(
        "import builtins\n"
        "import time\n"
        "time.sleep(0.3)\n"
        f"builtins.{event_name}.append('loaded')\n"
    )
    root = tmp_path / "root.metta"
    root.write_text(f'!(import! &self "{module_name}.py")\n')

    try:
        with concurrent.futures.ThreadPoolExecutor(max_workers=1) as executor:
            first = executor.submit(petta_instance.load_metta_file, str(root))
            time.sleep(0.05)
            started = time.monotonic()
            petta_instance.load_metta_file(str(root))
            waited = time.monotonic() - started
            first.result()

        assert waited >= 0.2
        assert getattr(builtins, event_name) == ["loaded"]
    finally:
        delattr(builtins, event_name)


def test_concurrent_import_waiters_receive_the_loader_exception(
    petta_instance, tmp_path
):
    module_name = f"failing_import_{uuid.uuid4().hex}"
    message = f"concurrent failure {uuid.uuid4().hex}"
    (tmp_path / f"{module_name}.py").write_text(
        "import time\n"
        "time.sleep(0.3)\n"
        f"raise RuntimeError({message!r})\n"
    )
    root = tmp_path / "root.metta"
    root.write_text(f'!(import! &self "{module_name}.py")\n')

    with concurrent.futures.ThreadPoolExecutor(max_workers=1) as executor:
        first = executor.submit(petta_instance.load_metta_file, str(root))
        time.sleep(0.05)
        with pytest.raises(Exception, match=message):
            petta_instance.load_metta_file(str(root))
        with pytest.raises(Exception, match=message):
            first.result()


def test_concurrent_loads_keep_relative_directories_thread_local(
    petta_instance, tmp_path
):
    function_a = f"thread_a_{uuid.uuid4().hex}"
    function_b = f"thread_b_{uuid.uuid4().hex}"
    directory_a = tmp_path / "a"
    directory_b = tmp_path / "b"
    directory_a.mkdir()
    directory_b.mkdir()

    (directory_a / "pause_a.py").write_text(
        "import time\n"
        "time.sleep(0.2)\n"
    )
    (directory_b / "pause_b.py").write_text(
        "import time\n"
        "time.sleep(0.5)\n"
    )
    (directory_a / "dependency.metta").write_text(
        f"(= ({function_a}) loaded-a)\n"
    )
    (directory_b / "dependency.metta").write_text(
        f"(= ({function_b}) loaded-b)\n"
    )
    root_a = directory_a / "root.metta"
    root_b = directory_b / "root.metta"
    root_a.write_text(
        '!(import! &self "pause_a.py")\n'
        "!(import! &self dependency)\n"
        f"!({function_a})\n"
    )
    root_b.write_text(
        '!(import! &self "pause_b.py")\n'
        "!(import! &self dependency)\n"
        f"!({function_b})\n"
    )

    with concurrent.futures.ThreadPoolExecutor(max_workers=2) as executor:
        future_a = executor.submit(petta_instance.load_metta_file, str(root_a))
        time.sleep(0.05)
        future_b = executor.submit(petta_instance.load_metta_file, str(root_b))
        results_a = future_a.result()
        results_b = future_b.result()

    assert "loaded-a" in results_a
    assert "loaded-b" in results_b
