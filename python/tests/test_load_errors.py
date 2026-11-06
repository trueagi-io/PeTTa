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


def test_declared_argument_type_is_checked_through_inferred_wrapper(repo_root, tmp_path):
    program = tmp_path / "inferred_wrapper_type_error.metta"
    program.write_text(
        '(: need-number (-> Number Number))\n'
        '(= (need-number $x) $x)\n'
        '(= (wrap $x) (need-number $x))\n'
        '!(wrap (car-atom ("bad" 1)))\n'
    )

    result = run_petta(repo_root, program)

    assert result.returncode != 0
    assert "Type mismatch" in result.stderr


def test_declared_argument_type_is_checked_after_caught_ascription(repo_root, tmp_path):
    program = tmp_path / "caught_ascription_type_error.metta"
    program.write_text(
        '(: need-number (-> Number Number))\n'
        '(= (need-number $x) $x)\n'
        '(= (w $x) (progn (catch (the Number $x)) (need-number $x)))\n'
        '!(w (car-atom ("bad" 1)))\n'
    )

    result = run_petta(repo_root, program)

    assert result.returncode != 0
    assert "Type mismatch" in result.stderr


def test_declared_list_element_type_is_checked_through_list_tail(repo_root, tmp_path):
    program = tmp_path / "open_list_tail_type_error.metta"
    program.write_text(
        '(: first-num (-> (List Number) Number))\n'
        '(= (first-num $xs) (car-atom $xs))\n'
        '(: tail-first (-> Expression Number))\n'
        '(= (tail-first $e) (first-num (cdr-atom $e)))\n'
        '!(tail-first (0 "bad" 1))\n'
    )

    result = run_petta(repo_root, program)

    assert result.returncode != 0
    assert "Type mismatch" in result.stderr


def test_declared_argument_type_is_checked_through_inferred_return(repo_root, tmp_path):
    program = tmp_path / "inferred_return_type_error.metta"
    program.write_text(
        '(: need-number (-> Number Number))\n'
        '(= (need-number $x) $x)\n'
        '(= (safe-div $a $b) (if (> $b 0) (/ $a $b) $a))\n'
        '!(need-number (safe-div (car-atom ("bad" 1)) 0))\n'
    )

    result = run_petta(repo_root, program)

    assert result.returncode != 0
    assert "Type mismatch" in result.stderr


def test_declared_argument_type_is_checked_inside_callback(repo_root, tmp_path):
    program = tmp_path / "callback_type_error.metta"
    program.write_text(
        '(: need-number (-> Number Number))\n'
        '(= (need-number $x) $x)\n'
        '(= (apply-one $x $f) ($f $x))\n'
        '!(apply-one "bad" (|-> ($x) (need-number $x)))\n'
    )

    result = run_petta(repo_root, program)

    # The callback must retain the declared Number check after specialization.
    assert "typecheck_or_error(A, 'Number')" in result.stdout


def test_declared_list_element_type_is_checked_after_open_ascription(repo_root, tmp_path):
    program = tmp_path / "open_list_ascription_type_error.metta"
    program.write_text(
        '(: first-num (-> (List Number) Number))\n'
        '(= (first-num $xs) (car-atom $xs))\n'
        '(= (w $xs) (first-num (the (List $t) $xs)))\n'
        '!(w ("bad" 1))\n'
    )

    result = run_petta(repo_root, program)

    assert result.returncode != 0
    assert "Type mismatch" in result.stderr
