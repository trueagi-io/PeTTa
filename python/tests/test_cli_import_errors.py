import subprocess


def run_cli(repo_root, metta_file):
    return subprocess.run(
        ["sh", str(repo_root / "run.sh"), str(metta_file)],
        cwd=repo_root,
        capture_output=True,
        text=True,
    )


def test_imported_parse_errors_are_not_silent(repo_root, tmp_path):
    broken = tmp_path / "broken.metta"
    broken.write_text("(= (borken) a\n", encoding="utf-8")

    wrapper = tmp_path / "wrapper.metta"
    wrapper.write_text("!(import! &self broken)\n", encoding="utf-8")

    direct_result = run_cli(repo_root, broken)
    imported_result = run_cli(repo_root, wrapper)

    assert direct_result.returncode != 0
    assert imported_result.returncode != 0
    assert "missing ')'" in direct_result.stderr
    assert "missing ')'" in imported_result.stderr
    assert str(broken) in direct_result.stderr
    assert str(broken) in imported_result.stderr


def test_imported_functions_are_preregistered_before_local_compilation(repo_root, tmp_path):
    dep = tmp_path / "dep.metta"
    dep.write_text("(= (foo $x) (+ $x 1))\n", encoding="utf-8")

    main = tmp_path / "main.metta"
    main.write_text(
        "(= (bar $x) (foo $x))\n"
        "!(import! &self dep)\n"
        "!(bar 41)\n",
        encoding="utf-8",
    )

    result = run_cli(repo_root, main)

    assert result.returncode == 0
    assert "(foo 41)" not in result.stdout
    assert "\n42\n" in result.stdout
