import os
import subprocess
import sys
import textwrap

# Each load runs in a fresh process, as a snapshot only replays onto the
# state it was taken from.
SCRIPT = textwrap.dedent(
    """
    import sys
    sys.path.insert(0, sys.argv[1])
    from python.petta import PeTTa
    petta = PeTTa(petta_path=sys.argv[1])
    print("results", petta.load_metta_file_cached(sys.argv[2]))
    print("value", petta.process_metta_string("!(double 21)"))
    """
)


def load_in_fresh_process(repo_root, library, cache):
    completed = subprocess.run(
        [sys.executable, "-c", SCRIPT, str(repo_root), str(library)],
        env={**os.environ, "PETTA_LOAD_CACHE": str(cache)},
        capture_output=True,
        text=True,
        check=True,
    )
    return completed.stdout.splitlines()


def write_library(directory, factor):
    (directory / "helper.metta").write_text(f"(= (double $x) (* {factor} $x))\n")
    library = directory / "library.metta"
    library.write_text("!(import! &self helper)\n!(println! loading)\n!(double 1)\n")
    return library


def test_second_load_replays_the_snapshot(repo_root, tmp_path):
    library = write_library(tmp_path, 2)
    cache = tmp_path / "cache"

    cold = load_in_fresh_process(repo_root, library, cache)
    warm = load_in_fresh_process(repo_root, library, cache)

    # Output printed while loading is not replayed; results and state are.
    assert cold == ["loading", "results ['true', 'true', '2']", "value ['42']"]
    assert warm == ["results ['true', 'true', '2']", "value ['42']"]
    assert len(list(cache.iterdir())) == 1


def test_changed_import_reloads(repo_root, tmp_path):
    library = write_library(tmp_path, 2)
    cache = tmp_path / "cache"
    load_in_fresh_process(repo_root, library, cache)

    write_library(tmp_path, 3)
    changed = load_in_fresh_process(repo_root, library, cache)

    assert changed == ["loading", "results ['true', 'true', '3']", "value ['63']"]


def test_new_source_beside_an_import_reloads(repo_root, tmp_path):
    library = write_library(tmp_path, 2)
    cache = tmp_path / "cache"
    load_in_fresh_process(repo_root, library, cache)

    (tmp_path / "unrelated.metta").write_text("(= (unrelated) 1)\n")
    reloaded = load_in_fresh_process(repo_root, library, cache)

    assert reloaded[0] == "loading"
