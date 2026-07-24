#!/bin/sh
set -eu

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
fixture=$(mktemp -d)
trap 'rm -rf "$fixture"' EXIT HUP INT TERM

remote="$fixture/fixture.git"
source_repo="$fixture/source"
base="$fixture/imports"
mkdir -p "$source_repo" "$base"
git -C "$source_repo" init -q
git -C "$source_repo" config user.email test@example.invalid
git -C "$source_repo" config user.name "PeTTa test"
printf 'build artifact\n' > "$source_repo/payload.txt"
printf '(= (fixture-core-result) core-git-ok)\n' > "$source_repo/module.metta"
printf '.build-count\n' > "$source_repo/.gitignore"
printf '#!/bin/sh\ncount=0\ntest ! -f .build-count || count=$(cat .build-count)\necho $((count + 1)) > .build-count\n' > "$source_repo/build.sh"
chmod +x "$source_repo/build.sh"
git -C "$source_repo" add .
git -C "$source_repo" commit -qm first
first=$(git -C "$source_repo" rev-parse HEAD)
printf 'tip\n' >> "$source_repo/payload.txt"
git -C "$source_repo" commit -qam second
second=$(git -C "$source_repo" rev-parse HEAD)
git clone -q --bare "$source_repo" "$remote"

run_import() {
    url=$1
    build=$2
    import_base=$3
    sha=$4
    swipl -q -g "consult('$project_dir/src/gitimport.pl'),'git-import!'('$url','$build','$import_base','$sha',true),halt"
}

# Fresh non-tip checkout and build.
run_import "$remote" build.sh "$base" "$first"
target="$base/fixture"
test "$(git -C "$target" rev-parse HEAD)" = "$first"
test "$(git -C "$target" symbolic-ref -q HEAD || true)" = ""
test "$(cat "$target/.build-count")" = 1

# The four-input form dispatches through the public MeTTa import layer.
metta_base="$fixture/metta-imports"
metta_program="$fixture/pinned-import.metta"
printf '!(git-import! "%s" "" "%s" "%s")\n!(import! &self (library fixture module))\n!(test (fixture-core-result) core-git-ok)\n' \
    "$remote" "$metta_base" "$first" > "$metta_program"
metta_output=$(cd "$project_dir" && sh run.sh "$metta_program")
printf '%s\n' "$metta_output"
printf '%s\n' "$metta_output" | grep -q '✅'
test "$(git -C "$metta_base/fixture" rev-parse HEAD)" = "$first"

# Repeated invocation is idempotent and does not rerun the build.
run_import "$remote" build.sh "$base" "$first"
test "$(cat "$target/.build-count")" = 1

# A clean transition fetches/checks out/builds the requested revision.
run_import "$remote" build.sh "$base" "$second"
test "$(git -C "$target" rev-parse HEAD)" = "$second"
test "$(cat "$target/.build-count")" = 2

# A transition refuses dirty state and preserves HEAD.
printf 'dirty\n' >> "$target/payload.txt"
if run_import "$remote" '' "$base" "$first" >"$fixture/dirty.log" 2>&1; then
    echo "dirty transition unexpectedly succeeded" >&2
    exit 1
fi
grep -q dirty_git_checkout "$fixture/dirty.log"
test "$(git -C "$target" rev-parse HEAD)" = "$second"
git -C "$target" checkout -q -- payload.txt

# Invalid and unreachable revisions fail without registering or exposing a target.
if run_import "$remote" '' "$fixture/invalid" deadbeef >"$fixture/invalid.log" 2>&1; then
    echo "abbreviated SHA unexpectedly succeeded" >&2
    exit 1
fi
grep -q "exactly 40 hexadecimal" "$fixture/invalid.log"
unreachable=0000000000000000000000000000000000000000
if run_import "$remote" '' "$fixture/unreachable" "$unreachable" >"$fixture/unreachable.log" 2>&1; then
    echo "unreachable SHA unexpectedly succeeded" >&2
    exit 1
fi
grep -q "fetch requested commit" "$fixture/unreachable.log"
test ! -e "$fixture/unreachable/fixture"

# Existing non-Git and wrong-origin targets are rejected.
mkdir -p "$fixture/non-git/fixture"
if run_import "$remote" '' "$fixture/non-git" "$first" >"$fixture/non-git.log" 2>&1; then
    echo "non-Git target unexpectedly succeeded" >&2
    exit 1
fi
grep -q "not a Git checkout" "$fixture/non-git.log"
git clone -q "$remote" "$fixture/wrong/fixture"
git -C "$fixture/wrong/fixture" remote set-url origin "$source_repo"
if run_import "$remote" '' "$fixture/wrong" "$first" >"$fixture/wrong.log" 2>&1; then
    echo "wrong origin unexpectedly succeeded" >&2
    exit 1
fi
grep -q git_origin "$fixture/wrong.log"

# A failed build leaves no partial target directory or library registration.
if run_import "$remote" missing-build.sh "$fixture/build-fail" "$first" >"$fixture/build.log" 2>&1; then
    echo "failed build unexpectedly succeeded" >&2
    exit 1
fi
grep -q "build imported repository" "$fixture/build.log"
test ! -e "$fixture/build-fail/fixture"

# Separate processes serialize a shared fresh target.
run_import "$remote" '' "$fixture/concurrent" "$first" &
pid1=$!
run_import "$remote" '' "$fixture/concurrent" "$first" &
pid2=$!
wait "$pid1"
wait "$pid2"
test "$(git -C "$fixture/concurrent/fixture" rev-parse HEAD)" = "$first"

# URL-only behavior clones a local deterministic remote without lib_import.
mkdir -p "$fixture/legacy"
(cd "$fixture/legacy" && swipl -q -g "consult('$project_dir/src/gitimport.pl'),'git-import!'('$remote',true),halt")
test -d "$fixture/legacy/repos/fixture/.git"

echo "commit-pinned git-import tests passed"
