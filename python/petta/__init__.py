import os
import threading
import importlib

CONSULTED = False
CONSULT_LOCK = threading.Lock()
janus = None


def _resolve_petta_path():
    """Locate the PeTTa runtime tree (src/, lib/, python/helper.pl).

    Prefers PETTA_PATH, then the runtime bundled in the installed package,
    then the source-tree root (editable installs and checkouts).
    """
    env_path = os.environ.get("PETTA_PATH")
    if env_path:
        return os.path.abspath(env_path)

    here = os.path.dirname(os.path.abspath(__file__))
    bundled = os.path.join(here, "_runtime")
    if os.path.exists(os.path.join(bundled, "src", "main.pl")):
        return bundled

    return os.path.abspath(os.path.join(here, os.pardir, os.pardir))


class PeTTa:
    def __init__(self, verbose=False, petta_path=None):
        global CONSULTED, janus
        self.verbose = "true" if verbose else "false"
        if not CONSULTED:
            with CONSULT_LOCK:
                if not CONSULTED:
                    if petta_path is None:
                        petta_path = _resolve_petta_path()
                    morklib_file = os.path.join(petta_path, "mork_ffi", "target", "release", "libmork_ffi.so")
                    if os.path.exists(morklib_file):
                        orig_dir = os.getcwd()
                        os.chdir(petta_path)
                        janus = importlib.import_module("janus_swi")
                        os.chdir(orig_dir)
                        janus.query_once("set_prolog_flag(argv, ['mork'])")
                    else:
                        janus = importlib.import_module("janus_swi")
                    main_file = os.path.join(petta_path, "src", "main.pl")
                    helper_file = os.path.join(petta_path, "python", "helper.pl")
                    if not os.path.exists(main_file):
                        raise FileNotFoundError(
                            f"PeTTa runtime not found under {petta_path!r} "
                            f"(expected {main_file!r}). Set the PETTA_PATH "
                            "environment variable or pass petta_path to point at "
                            "a PeTTa checkout."
                        )
                    janus.consult(main_file)
                    janus.consult(helper_file)
                    CONSULTED = True

    def _run_helper(self, helper_name, argument):
        result = janus.query_once(
            "run_metta_helper(Verbose, HelperName, Argument, Results)",
            {
                "Verbose": "true" if self.verbose else "false",
                "HelperName": helper_name,
                "Argument": argument,
            },
        )
        if result is None:
            return []
        return result.get("Results", [])

    def load_metta_file(self, file_path) -> str:
        """Compile a MeTTa file to Prolog and return the results of the run."""
        return self._run_helper("load_metta_file", file_path)

    def process_metta_string(self, metta_code) -> str:
        """Compile a string of MeTTa code to Prolog and return the results of the run."""
        return self._run_helper("process_metta_string", metta_code)
