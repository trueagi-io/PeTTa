import os
import threading
import importlib

CONSULTED = False
CONSULT_LOCK = threading.Lock()
janus = None
DEFAULT_STACK_LIMIT = 8_000_000_000

class PeTTa:
    def __init__(self, verbose=False, petta_path=None):
        global CONSULTED, janus
        self.verbose = "true" if verbose else "false"
        if not CONSULTED:
            with CONSULT_LOCK:
                if not CONSULTED:
                    if petta_path is None:
                        petta_path = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
                    morklib_file = os.path.join(petta_path, "mork_ffi", "target", "release", "libmork_ffi.so")
                    if os.path.exists(morklib_file):
                        orig_dir = os.getcwd()
                        os.chdir(petta_path)
                        janus = importlib.import_module("janus_swi")
                        janus.query_once(f"set_prolog_flag(stack_limit, {DEFAULT_STACK_LIMIT})")
                        os.chdir(orig_dir)
                        janus.query_once("set_prolog_flag(argv, ['mork'])")
                    else:
                        janus = importlib.import_module("janus_swi")
                        janus.query_once(f"set_prolog_flag(stack_limit, {DEFAULT_STACK_LIMIT})")
                    main_file = os.path.join(petta_path, "src", "main.pl")
                    helper_file = os.path.join(petta_path, "python", "helper.pl")
                    janus.consult(main_file)
                    janus.consult(helper_file)
                    CONSULTED = True

    def _run_helper(self, helper_name, argument):
        query = f"run_metta_helper({self.verbose},{helper_name},'{argument}', Results)"
        result = janus.query_once(query)
        if result is None:
            return []
        return result.get("Results", [])

    def load_metta_file(self, file_path) -> str:
        """Compile a MeTTa file to Prolog and return the results of the run."""
        return self._run_helper("load_metta_file", file_path)

    def process_metta_string(self, metta_code) -> str:
        """Compile a string of MeTTa code to Prolog and return the results of the run."""
        return self._run_helper("process_metta_string", metta_code)
