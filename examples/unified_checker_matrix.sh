#!/bin/sh
set -eu

swipl -q -g "load_files(['src/typecheck/abstract_domain.pl',
                          'src/typecheck/call_summaries.pl',
                          'src/typecheck/relational_ir.pl',
                          'src/typecheck/ir_analyzer.pl',
                          'src/typecheck/unified_checker_cache.pl',
                          'src/typecheck/unified_checker_bridge.pl'],
                         [if(true)]),
             run_tests([abstract_domain, call_summaries, relational_ir,
                        ir_analyzer, unified_checker_cache,
                        unified_checker_bridge]),
             call_summaries:validate_summary_table,
             halt"

swipl -q -s src/metta.pl \
   -s examples/unified_checker_lifecycle_tests.pl \
   -g "run_tests([unified_checker_lifecycle]), halt" -- \
   --strict --strict-det -s

sh run.sh examples/strictdet_unified_cross_file_cache.metta \
   --strict --strict-det -s >/dev/null
