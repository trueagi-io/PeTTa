#!/bin/sh

SCRIPT_DIR=$(cd -- "$(dirname -- "$0")" && pwd)

if [ "$#" -eq 0 ]; then
    exec swipl -q -s "$SCRIPT_DIR/src/main.pl" -- --debug-help
fi

# Beginner shortcut:
#   sh debug.sh watch <file.metta>            trace everything, nested by call
#   sh debug.sh watch <file.metta> <function> trace just one function's calls
# Expands to a clean, scoped runtime trace so newcomers need not assemble flags.
if [ "$1" = "watch" ]; then
    shift
    FILE="$1"
    if [ -z "$FILE" ]; then
        echo "Usage: sh debug.sh watch <file.metta> [function]" >&2
        echo "  Traces the program; give a function name to trace only its calls." >&2
        exit 2
    fi
    shift
    GOAL=""
    if [ "$#" -ge 1 ]; then
        case "$1" in
            --*) ;;                       # already an option: don't treat as a function
            *) GOAL="--debug-goal=$1"; shift ;;
        esac
    fi
    exec sh "$SCRIPT_DIR/run.sh" "$FILE" --debug=runtime $GOAL --silent --debug-indent "$@"
fi

exec sh "$SCRIPT_DIR/run.sh" "$@"
