"""Command-line launcher for the bundled PeTTa runtime."""

import os
import subprocess
import sys

from . import _resolve_petta_path


def main(argv=None):
    """Run PeTTa's SWI-Prolog entry point and return its exit status."""
    if argv is None:
        argv = sys.argv[1:]

    runtime_root = _resolve_petta_path()
    main_file = os.path.join(runtime_root, "src", "main.pl")
    command = [
        "swipl",
        "--stack_limit=8g",
        "-q",
        "-s",
        main_file,
        "--",
        *argv,
    ]

    mork_library = os.path.join(
        runtime_root, "mork_ffi", "target", "release", "libmork_ffi.so"
    )
    env = None
    if os.path.isfile(mork_library):
        env = os.environ.copy()
        env["LD_PRELOAD"] = mork_library
        command.append("mork")

    return subprocess.call(command, env=env)
