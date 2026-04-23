SCRIPT_DIR=$(cd -- "$(dirname -- "$0")" && pwd)
if [ -f $SCRIPT_DIR/mork_ffi/target/release/libmork_ffi.so ]; then
    LD_PRELOAD=$SCRIPT_DIR/mork_ffi/target/release/libmork_ffi.so \
    swipl --stack_limit=8g -q -s $SCRIPT_DIR/src/main.pl -- "$0" mork
elif [ $1 = "--compiler" ] ; then
    echo "COMPILER called"
    BASENAME=$(basename "${2}")
    COMPILED_FILE=$(mktemp --suffix ".pl" "${TMPDIR}/${BASENAME}XXXXX")
    echo "swipl --stack_limit=8g -q -s $SCRIPT_DIR/src/main.pl -- --silent=true --mode=COMPILER -o ${COMPILED_FILE} ${2}"
    swipl --stack_limit=8g -q -s $SCRIPT_DIR/src/main.pl -- --silent=true --mode=COMPILER -o ${COMPILED_FILE} "${2}"
    echo "COMPILED FILE: ${COMPILED_FILE}"
    OUTPUT=$(swipl -s ${COMPILED_FILE} -g true -t halt)
    echo "OUTPUT: ${COMPILED_FILE}"
    rm ${COMPILED_FILE}
    echo ${OUTPUT}
else
    swipl --stack_limit=8g -q -s $SCRIPT_DIR/src/main.pl -- --silent=false --mode=INTERPRETER "$1"
fi
