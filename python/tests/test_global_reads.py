import subprocess
import sys
import textwrap

# Reading a global that is not set must fail quietly: an existence error per
# read builds and copies an error term, and the compiler reads optional
# globals (scopes, stacks, stored clauses) many times per clause.
SCRIPT = textwrap.dedent(
    """
    import sys
    sys.path.insert(0, sys.argv[1])
    from python.petta import PeTTa
    petta = PeTTa(petta_path=sys.argv[1])
    from python.petta import janus
    janus.consult("missing_global_hook", '''
    :- dynamic missing_global_read/1.
    :- multifile prolog:prolog_exception_hook/5.
    prolog:prolog_exception_hook(error(existence_error(variable, Key), _), _, _, _, _) :-
        assertz(missing_global_read(Key)), fail.
    ''')
    print(petta.process_metta_string('''
    (: inc (-> Number Number))
    (= (inc $x) (+ $x 1))
    (: twice (-> (-> Number Number) Number Number))
    (= (twice $f $x) ($f ($f $x)))
    (= (sum $xs) (foldl-atom $xs 0 $a $b (+ $a $b)))
    !(twice inc 1)
    !(sum (1 2 3))
    !((|-> ($x) (* 2 $x)) 5)
    '''))
    print(janus.query_once("aggregate_all(count, missing_global_read(_), N)")["N"])
    """
)


def test_compiling_reads_no_missing_global(repo_root):
    completed = subprocess.run(
        [sys.executable, "-c", SCRIPT, str(repo_root)],
        capture_output=True,
        text=True,
        check=True,
    )
    assert completed.stdout.splitlines() == ["['3', '6', '10']", "0"]
