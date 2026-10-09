SCRIPT_DIR=$(cd -- "$(dirname -- "$0")" && pwd)
# PETTA_STACK_LIMIT caps swipl's stacks (default 8g). The test harness sets it
# lower so a pool of parallel test runs has a bounded worst case.
STACK_LIMIT=${PETTA_STACK_LIMIT:-8g}
if [ -f $SCRIPT_DIR/mork_ffi/target/release/libmork_ffi.so ]; then
    LD_PRELOAD=$SCRIPT_DIR/mork_ffi/target/release/libmork_ffi.so \
    swipl --stack_limit=$STACK_LIMIT -q -s $SCRIPT_DIR/src/main.pl -- "$@" mork
else
    swipl --stack_limit=$STACK_LIMIT -q -s $SCRIPT_DIR/src/main.pl -- "$@"
fi
