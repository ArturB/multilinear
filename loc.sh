#!/bin/bash

# Library LOC
LIB=$( cat $( find . | grep src/.*\.hs$ ) | wc -l )
LIB_CODE=$( cat $( find . | grep src/.*\.hs$ ) | grep --invert "\{\-" | grep --invert "\-\-" | wc -l )
let "LIB_COMMENTS = $LIB - $LIB_CODE"

echo
echo Library LOC: $LIB, therein:
echo "  - Code: $LIB_CODE"
echo "  - Comments: $LIB_COMMENTS"

# Tests LOC
TESTS=$( cat $( find . | grep test/.*\.hs$ ) | wc -l )
TESTS_CODE=$( cat $( find . | grep test/.*\.hs$ ) | grep --invert "\{\-" | grep --invert "\-\-" | wc -l )
let "TESTS_COMMENTS = $TESTS - $TESTS_CODE"

echo
echo Tests LOC: $TESTS, therein
echo "  - Code: $TESTS_CODE"
echo "  - Comments: $TESTS_COMMENTS"

# Benchmarks LOC
BENCHS=$( cat $( find . | grep benchmark/.*\.hs$ ) | wc -l )
BENCHS_CODE=$( cat $( find . | grep benchmark/.*\.hs$ ) | grep --invert "\{\-" | grep --invert "\-\-" | wc -l )
let "BENCHS_COMMENTS = $BENCHS - $BENCHS_CODE"

echo
echo Benchmarks LOC: $BENCHS, therein
echo "  - Code: $BENCHS_CODE"
echo "  - Comments: $BENCHS_COMMENTS"

# Total LOC
let "TOTAL = $LIB + $TESTS + $BENCHS"
let "TOTAL_CODE = $LIB_CODE + $TESTS_CODE + $BENCHS_CODE"
let "TOTAL_COMMENTS = $LIB_COMMENTS + $TESTS_COMMENTS + $BENCHS_COMMENTS"
echo
echo -e "\u001b[33mTotal LOC: $TOTAL, therein\u001b[0m"
echo -e "\u001b[33m  - Code: $TOTAL_CODE\u001b[0m"
echo -e "\u001b[33m  - Comments: $TOTAL_COMMENTS\u001b[0m"
