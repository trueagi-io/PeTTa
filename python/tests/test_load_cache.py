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
    print("value", petta.process_metta_string(sys.argv[3]))
    """
)


def load_in_fresh_process(repo_root, library, cache, query="!(double 21)"):
    completed = subprocess.run(
        [sys.executable, "-c", SCRIPT, str(repo_root), str(library), query],
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


def test_scalar_fold_workers_replay(repo_root, tmp_path):
    library = tmp_path / "scalar.metta"
    library.write_text('''
!(import! &self (library lib_roman))
(: cache-step (-[det]-> Number (Number Number) Number (Number Number)))
(= (cache-step $k ($a $b) $x) ((+ $a $x) (+ $b (* $k $x))))
(: cache-totals (-[det]-> Number (List Number) (Number Number)))
(= (cache-totals $k $xs) (fold-flat (cache-step $k) (data 0 0) $xs))
(: cache-sum (-[det]-> Number (List Number) Number))
(= (cache-sum $k $xs) (let ($a $b) (cache-totals $k $xs) (+ $a $b)))
!(println! scalar-loading)
''')
    cache = tmp_path / "cache"
    cold = load_in_fresh_process(repo_root, library, cache, "!(cache-sum 2 (1 2 3))")
    warm = load_in_fresh_process(repo_root, library, cache, "!(cache-sum 2 (1 2 3))")
    assert cold[0] == "scalar-loading"
    assert cold[1:] == warm
    assert warm[-1] == "value ['18']"
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


def test_predicate_larger_than_a_chunk_replays(repo_root, tmp_path):
    # 2500 clauses span several snapshot chunks and end in a partial one.
    library = tmp_path / "large.metta"
    library.write_text("".join(f"(= (f {i}) {i})\n" for i in range(2500)))
    cache = tmp_path / "cache"
    query = "!(f 0) !(f 999) !(f 1000) !(f 2499)"

    cold = load_in_fresh_process(repo_root, library, cache, query)
    warm = load_in_fresh_process(repo_root, library, cache, query)

    assert cold == warm == ["results []", "value ['0', '999', '1000', '2499']"]
    assert len(list(cache.iterdir())) == 1
