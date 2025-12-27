#!/bin/bash
# Script to run Prolog unit tests with a timeout

run_suite() {
    SUITE=$1
    echo "Running suite: $SUITE"
    timeout 10s swipl -g "run_tests($SUITE)" -t halt 8.pl
    EXIT_CODE=$?
    if [ $EXIT_CODE -eq 124 ]; then
        echo "TIMEOUT for suite: $SUITE"
    elif [ $EXIT_CODE -ne 0 ]; then
        echo "FAILURE for suite: $SUITE (Exit code: $EXIT_CODE)"
    else
        echo "SUCCESS for suite: $SUITE"
    fi
    echo "---------------------------------------------------"
}

run_suite world_generation
run_suite disjoint_logic
run_suite distinguishing_scenarios
run_suite pruning_logic
run_suite pigeonhole_prune_check
run_suite complex_pruning_scenario
