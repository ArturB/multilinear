#!/bin/bash

source id-build.sh

# Build whole project: library, tests, benchmarks, documentation 
if stack build --fast --test --coverage --no-run-tests --bench --no-run-benchmarks --haddock; then
    echo -e "Build successful!"
    if [ "$1" != "--nogit" ] ; then
        ./vcs.sh --compiled
    fi
    echo -e "All done!"
    exit 0
else
    ./vcs.sh
    echo -e "\u001b[31mBuild failure!\u001b[0m"
    exit 1
fi
