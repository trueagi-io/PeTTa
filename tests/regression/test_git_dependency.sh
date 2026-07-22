#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
PATH="$ROOT/../../local/swipl-9.3.36/bin:$PATH"
export PATH
fixture=$(mktemp -d)
trap 'rm -rf "$fixture"' EXIT HUP INT TERM

# Two local repositories: b is a leaf, a declares b in deps.metta at its second commit.
src_a="$fixture/src_a"
src_b="$fixture/src_b"
mkdir -p "$src_a" "$src_b"

git -C "$src_b" init -q
git -C "$src_b" config user.email test@example.invalid
git -C "$src_b" config user.name "PeTTa test"
printf '(= (transfn) trans-ok)\n' > "$src_b/translib.metta"
git -C "$src_b" add .
git -C "$src_b" commit -qm b1
b1=$(git -C "$src_b" rev-parse HEAD)
git clone -q --bare "$src_b" "$fixture/b.git"

git -C "$src_a" init -q
git -C "$src_a" config user.email test@example.invalid
git -C "$src_a" config user.name "PeTTa test"
printf '(= (libfn $x) (libok $x))\n' > "$src_a/mylib.metta"
printf '.build-count\n' > "$src_a/.gitignore"
printf '#!/bin/sh\ncount=0\ntest ! -f .build-count || count=$(cat .build-count)\necho $((count + 1)) > .build-count\n' > "$src_a/build.sh"
chmod +x "$src_a/build.sh"
git -C "$src_a" add .
git -C "$src_a" commit -qm a1
a1=$(git -C "$src_a" rev-parse HEAD)
printf '(= (libfn $x) (libok2 $x))\n' > "$src_a/mylib.metta"
printf '(git-dependency "file://%s" "%s")\n' "$fixture/b.git" "$b1" > "$src_a/deps.metta"
git -C "$src_a" add .
git -C "$src_a" commit -qm a2
a2=$(git -C "$src_a" rev-parse HEAD)
git clone -q --bare "$src_a" "$fixture/a.git"

proj="$fixture/proj"
mkdir -p "$proj"

run_prog() {
    prog="$1"
    log="$2"
    ( cd "$proj" && timeout 120s sh "$ROOT/run.sh" "$prog" --silent ) > "$log" 2>&1
}

# Pinned acquisition, transitive manifest, and definitions placed above their imports.
printf '(git-dependency "file://%s" "%s" "build.sh")\n(= (uses) (libfn 1))\n!(import! &self (library mylib))\n!(uses)\n(= (uses2) (transfn))\n!(import! &self (library translib))\n!(uses2)\n' \
    "$fixture/a.git" "$a2" > "$proj/prog.metta"
run_prog "$proj/prog.metta" "$fixture/run1.log"
grep -q '(libok2 1)' "$fixture/run1.log" || { echo "fresh run: cross-repo call did not reduce"; cat "$fixture/run1.log"; exit 1; }
grep -q 'trans-ok' "$fixture/run1.log" || { echo "fresh run: transitive dependency missing"; cat "$fixture/run1.log"; exit 1; }
test "$(git -C "$proj/repos/a" rev-parse HEAD)" = "$a2"
test "$(git -C "$proj/repos/b" rev-parse HEAD)" = "$b1"
test "$(git -C "$proj/repos/a" symbolic-ref -q HEAD || true)" = ""
test "$(cat "$proj/repos/a/.build-count")" = 1

# A second run in the same directory reuses the checkout and produces identical
# program output; only acquisition progress lines differ between fresh and reuse.
run_prog "$proj/prog.metta" "$fixture/run2.log"
grep -v '^Running build:' "$fixture/run1.log" > "$fixture/run1.stripped"
grep -v '^Running build:' "$fixture/run2.log" > "$fixture/run2.stripped"
diff "$fixture/run1.stripped" "$fixture/run2.stripped" || { echo "dirty rerun diverged from fresh run"; exit 1; }
test "$(cat "$proj/repos/a/.build-count")" = 1

# Pinning a different commit retargets the existing checkout and reruns the build.
printf '(git-dependency "file://%s" "%s" "build.sh")\n!(import! &self (library mylib))\n!(libfn 1)\n' \
    "$fixture/a.git" "$a1" > "$proj/retarget.metta"
run_prog "$proj/retarget.metta" "$fixture/run3.log"
grep -q '(libok 1)' "$fixture/run3.log" || { echo "retarget: old-revision definition not active"; cat "$fixture/run3.log"; exit 1; }
test "$(git -C "$proj/repos/a" rev-parse HEAD)" = "$a1"
test "$(cat "$proj/repos/a/.build-count")" = 2

# A dirty checkout is refused rather than overwritten.
printf 'dirty\n' >> "$proj/repos/a/mylib.metta"
if run_prog "$proj/prog.metta" "$fixture/run4.log"; then
    echo "dirty checkout transition unexpectedly succeeded"; cat "$fixture/run4.log"; exit 1
fi
grep -q dirty_git_checkout "$fixture/run4.log"
git -C "$proj/repos/a" checkout -q -- mylib.metta

# Declarations demand a full 40-character commit.
printf '(git-dependency "file://%s" "deadbeef")\n' "$fixture/a.git" > "$proj/shortsha.metta"
if run_prog "$proj/shortsha.metta" "$fixture/run5.log"; then
    echo "abbreviated commit unexpectedly accepted"; cat "$fixture/run5.log"; exit 1
fi
grep -q '40 hexadecimal' "$fixture/run5.log"

# The same url pinned to two revisions in one run is a conflict.
printf '(git-dependency "file://%s" "%s")\n(git-dependency "file://%s" "%s")\n' \
    "$fixture/a.git" "$a1" "$fixture/a.git" "$a2" > "$proj/conflict.metta"
if run_prog "$proj/conflict.metta" "$fixture/run6.log"; then
    echo "conflicting pins unexpectedly accepted"; cat "$fixture/run6.log"; exit 1
fi
grep -q conflicting_git_dependency "$fixture/run6.log"

printf 'git dependency checks passed\n'
