import os
from unittest.mock import Mock

from petta import cli


def test_main_forwards_arguments_and_exit_status(monkeypatch, tmp_path):
    runtime = tmp_path / "runtime with spaces"
    main_file = runtime / "src" / "main.pl"
    main_file.parent.mkdir(parents=True)
    main_file.touch()
    call = Mock(return_value=23)
    monkeypatch.setattr(cli, "_resolve_petta_path", lambda: str(runtime))
    monkeypatch.setattr(cli.subprocess, "call", call)

    status = cli.main(["program with spaces.metta", "--example"])

    assert status == 23
    call.assert_called_once_with(
        [
            "swipl",
            "--stack_limit=8g",
            "-q",
            "-s",
            str(main_file),
            "--",
            "program with spaces.metta",
            "--example",
        ],
        env=None,
    )


def test_main_preserves_optional_mork_behavior(monkeypatch, tmp_path):
    runtime = tmp_path / "runtime"
    mork_library = (
        runtime / "mork_ffi" / "target" / "release" / "libmork_ffi.so"
    )
    mork_library.parent.mkdir(parents=True)
    mork_library.touch()
    call = Mock(return_value=0)
    monkeypatch.setattr(cli, "_resolve_petta_path", lambda: str(runtime))
    monkeypatch.setattr(cli.subprocess, "call", call)
    monkeypatch.setenv("PETTA_CLI_TEST", "inherited")

    assert cli.main(["program.metta"]) == 0

    command = call.call_args.args[0]
    child_env = call.call_args.kwargs["env"]
    assert command[-2:] == ["program.metta", "mork"]
    assert child_env["LD_PRELOAD"] == str(mork_library)
    assert child_env["PETTA_CLI_TEST"] == "inherited"
    assert child_env is not os.environ
