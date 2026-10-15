#!/bin/sh
#
# Launch the PeTTa Debug Adapter Protocol (DAP) server. It speaks DAP JSON over
# stdin/stdout, so an editor (e.g. VS Code) launches this as the debug adapter.
# The program to debug is given by the launch request's "program" argument.

SCRIPT_DIR=$(cd -- "$(dirname -- "$0")" && pwd)

exec swipl --stack_limit=8g -q -g dap_server "$SCRIPT_DIR/src/debugger/dap_server.pl" -- "$@"
