import subprocess
from pathlib import Path


def run_petta(repo_root, program, stack_limit="8g"):
    main_file = Path(repo_root) / "src" / "main.pl"
    return subprocess.run(
        ["swipl", f"--stack_limit={stack_limit}", "-q", "-s", str(main_file), "--", str(program)],
        stdin=subprocess.DEVNULL,
        capture_output=True,
        text=True,
        timeout=120,
    )


def test_stack_overflow_while_loading_keeps_its_message(repo_root, tmp_path):
    program = tmp_path / "overflow.metta"
    program.write_text("(= (deep $n) (+ 1 (deep (+ $n 1))))\n!(deep 0)\n")

    result = run_petta(repo_root, program, stack_limit="16m")

    assert result.returncode != 0
    assert "Stack limit (16.0Mb) exceeded" in result.stderr
    assert "dict' expected" not in result.stderr


def test_error_without_context_names_the_file(repo_root, tmp_path):
    program = tmp_path / "empty_car.metta"
    program.write_text("!(car-atom ())\n")

    result = run_petta(repo_root, program)

    assert result.returncode != 0
    assert "car-atom expects a non-empty expression" in result.stderr
    assert f"'{program}'" in result.stderr
    assert "while loading MeTTa file" in result.stderr
