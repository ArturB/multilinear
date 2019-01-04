#!/bin/bash

source id-build.sh

# Build whole project: library, tests, benchmarks, documentation 
if stack build --fast --test --coverage --no-run-tests --bench --no-run-benchmarks --haddock; then
    echo -e "Build successful!"
    if [ "$1" != "--nogit" ] ; then
        echo -e "Pushing changes to git..."
        ( git commit -aqm "Untested build $BUILD_ID" && git pull -q && git push -q ) &
    fi
    echo -e "All done!"
    exit 0
else
    echo -e "\u001b[31mBuild failure!\u001b[0m"
    exit 1
fi
