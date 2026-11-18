#!/bin/sh
set -eu

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
fixture=$(mktemp -d)
trap 'rm -rf "$fixture"' EXIT HUP INT TERM

cat > "$fixture/import_llm.metta" <<'METTA'
!(import! &self (library lib_llm))
METTA

if ! (unset OPENAI_API_KEY; cd "$project_dir" && sh run.sh "$fixture/import_llm.metta") \
    >"$fixture/output.log" 2>&1; then
    cat "$fixture/output.log"
    echo "LLM library import failed without OpenAI initialization" >&2
    exit 1
fi

echo "LLM library imports without OpenAI credentials"
