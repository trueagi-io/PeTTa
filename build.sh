#!/bin/sh
set -eu

cd -- "$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"

MORK_FFI_COMMIT=${MORK_FFI_COMMIT:-f6553aa4559142895c5547f56e490d29e29469f1}
FAISS_FFI_COMMIT=${FAISS_FFI_COMMIT:-d02f69a21954fc09cf160f2e2d49d9d57b64277b}
MORK_COMMIT=${MORK_COMMIT:-ed57c6716d8c510296fb5fbb8be6fbfe2df241d7}
PATHMAP_COMMIT=${PATHMAP_COMMIT:-7c632f9d5c1cd42efb99e860ebdc1be64851035f}

for dir in mork_ffi faiss_ffi build/deps/MORK build/deps/PathMap; do
    if [ -e "$dir" ]; then
        echo "ERROR: $dir already exists; use a fresh checkout or move it aside." >&2
        exit 1
    fi
done

clone_at() {
    git clone "$1" "$2"
    git -C "$2" checkout --detach "$3"
}

mkdir -p build/deps
clone_at https://github.com/trueagi-io/MORK.git build/deps/MORK "$MORK_COMMIT"
clone_at https://github.com/Adam-Vandervorst/PathMap.git build/deps/PathMap "$PATHMAP_COMMIT"
clone_at https://github.com/patham9/mork_ffi.git mork_ffi "$MORK_FFI_COMMIT"
clone_at https://github.com/patham9/faiss_ffi.git faiss_ffi "$FAISS_FFI_COMMIT"

# Keep the FFI at PeTTa's root and point its dependencies at our pinned MORK.
sed -i 's#\.\./\.\./MORK/#../build/deps/MORK/#g' mork_ffi/Cargo.toml

(cd mork_ffi && sh -e build.sh)
(cd faiss_ffi && sh -e build.sh)
