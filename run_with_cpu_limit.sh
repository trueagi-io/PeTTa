#!/bin/sh
# cpu_limit.sh
# usage: ./cpu_limit.sh <cpu_seconds> command [args...]

if [ $# -lt 2 ]; then
  echo "Usage: $0 <cpu_seconds> command [args...]"
  exit 1
fi

CPU_LIMIT="$1"
shift

# set CPU time limit (in seconds)
ulimit -t "$CPU_LIMIT"

# run command (replaces shell with it)
exec "$@"
