#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
cd "$ROOT"

# Pinned revisions; overrides must identify exact commits.
MORK_FFI_COMMIT="${MORK_FFI_COMMIT:-f6553aa4559142895c5547f56e490d29e29469f1}"
FAISS_FFI_COMMIT="${FAISS_FFI_COMMIT:-d02f69a21954fc09cf160f2e2d49d9d57b64277b}"
MORK_COMMIT="${MORK_COMMIT:-ed57c6716d8c510296fb5fbb8be6fbfe2df241d7}"
PATHMAP_COMMIT="${PATHMAP_COMMIT:-7c632f9d5c1cd42efb99e860ebdc1be64851035f}"

# Require full 40-character hexadecimal commit IDs, not branch or tag names.
validate_commit_id() {
    name=$1
    value=$2

    case "$value" in
        *[!0123456789abcdefABCDEF]*|'')
            echo "ERROR: $name must be a full 40-character commit SHA." >&2
            exit 1
            ;;
    esac

    if [ "${#value}" -ne 40 ]; then
        echo "ERROR: $name must be a full 40-character commit SHA." >&2
        exit 1
    fi
}

validate_commit_id MORK_FFI_COMMIT "$MORK_FFI_COMMIT"
validate_commit_id FAISS_FFI_COMMIT "$FAISS_FFI_COMMIT"
validate_commit_id MORK_COMMIT "$MORK_COMMIT"
validate_commit_id PATHMAP_COMMIT "$PATHMAP_COMMIT"

DEPS="$ROOT/build/deps"

# Refuse to overwrite existing checkouts.
for dir in mork_ffi faiss_ffi "$DEPS/MORK" "$DEPS/PathMap"; do
    if [ -e "$dir" ]; then
        echo "ERROR: $dir already exists; refusing to overwrite it." >&2
        exit 1
    fi
done

# Track which dependency paths this run starts creating.
created_mork=0
created_pathmap=0
created_mork_ffi=0
created_faiss_ffi=0

cleanup_on_failure() {
    status=$?
    trap - 0

    if [ "$status" -ne 0 ]; then
        echo "Build failed; cleaning up dependency paths created by this run." >&2
        [ "$created_mork" -eq 0 ] || rm -rf "$DEPS/MORK"
        [ "$created_pathmap" -eq 0 ] || rm -rf "$DEPS/PathMap"
        [ "$created_mork_ffi" -eq 0 ] || rm -rf "$ROOT/mork_ffi"
        [ "$created_faiss_ffi" -eq 0 ] || rm -rf "$ROOT/faiss_ffi"
    fi

    exit "$status"
}

trap cleanup_on_failure 0

mkdir -p "$DEPS"

created_mork=1
git clone https://github.com/trueagi-io/MORK.git "$DEPS/MORK"
git -C "$DEPS/MORK" checkout --detach "$MORK_COMMIT"

created_pathmap=1
git clone https://github.com/Adam-Vandervorst/PathMap.git "$DEPS/PathMap"
git -C "$DEPS/PathMap" checkout --detach "$PATHMAP_COMMIT"

created_mork_ffi=1

# Keep mork_ffi at the root, where PeTTa expects to find it.
git clone https://github.com/patham9/mork_ffi.git mork_ffi
git -C mork_ffi checkout --detach "$MORK_FFI_COMMIT"

# Adapt the pinned FFI manifest to the isolated in-repository MORK checkout.
MANIFEST="$ROOT/mork_ffi/Cargo.toml"

# Fail if the pinned manifest layout differs from the paths this script expects.
for dependency in frontend expr kernel; do
    case "$dependency" in
        frontend) original_path='path = "../../MORK/frontend"' ;;
        expr)     original_path='path = "../../MORK/expr"' ;;
        kernel)   original_path='path = "../../MORK/kernel"' ;;
    esac

    count=$(grep -F -c "$original_path" "$MANIFEST" || :)
    if [ "$count" -ne 1 ]; then
        echo "ERROR: expected exactly one MORK $dependency path in $MANIFEST; found $count." >&2
        exit 1
    fi
done

sed -i \
    -e 's#path = "../../MORK/frontend"#path = "../build/deps/MORK/frontend"#' \
    -e 's#path = "../../MORK/expr"#path = "../build/deps/MORK/expr"#' \
    -e 's#path = "../../MORK/kernel"#path = "../build/deps/MORK/kernel"#' \
    "$MANIFEST"

if grep -q '../../MORK/' "$MANIFEST"; then
    echo "ERROR: an original MORK path remains in $MANIFEST" >&2
    exit 1
fi

(
    cd "$ROOT/mork_ffi"
    sh -e build.sh
)

# Build FAISS FFI from its pinned revision.
created_faiss_ffi=1
git clone https://github.com/patham9/faiss_ffi.git faiss_ffi
git -C faiss_ffi checkout --detach "$FAISS_FFI_COMMIT"

(
    cd "$ROOT/faiss_ffi"
    sh -e build.sh
)
