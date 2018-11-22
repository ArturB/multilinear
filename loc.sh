# Calculate total LOC number in the project

# Library LOC
LIB=$( cat $( find . | grep src/.*\.hs$ ) | wc -l )
echo Library LOC: $LIB

# Tests LOC
TESTS=$( cat $( find . | grep test/.*\.hs$ ) | wc -l )
echo Tests LOC: $TESTS

# Benchmarks LOC
BENCHS=$( cat $( find . | grep benchmark/.*\.hs$ ) | wc -l )
echo Benchmarks LOC: $BENCHS

# Total LOC
let "TOTAL = $LIB + $TESTS + $BENCHS"
echo -e "\u001b[33mTotal LOC: $TOTAL\u001b[0m"
