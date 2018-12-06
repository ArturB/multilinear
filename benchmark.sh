#!/bin/bash

# Benchmark results filename
BNAME="benchmark/results.html"

# Build all benchmartks and push changes to git if successful
# If build failed, don't go on, but exit
if stack bench --no-run-benchmarks ; then
    echo -e "Build successful!\nPushing changes to git..."
    ( git add -A > /dev/null && git commit -qm "Benchmark build $BUILD_ID" && git pull -q && git push -q ) &
    echo -e "Running all benchmarks..."
else
    exit 1
fi

# Remove earlier benchmarks result if present
rm -f benchmark/results.html
rm -f benchmark/multicore-results.html
rm -f benchmark/sequential-results.html

# Perform all benchmarks in background and move on
stack bench --ba "--output $BNAME" &
STACK_PID=$!

function ctrl_c {
    echo "Terminating stack..."
    kill $STACK_PID
    echo Done!
    exit 0
}
trap ctrl_c INT

# Here we have three benchmarks, GPU, MultiCore and Sequential. Both are written by stack to benchmark/results.html
# so when results of first benchmark (MultiCore) are available, rename them to multicore-results.html 
# and after second results (sequential) are available, rename them to sequential-results.html

while [ true ] ; do
    sleep 1
    if [ -f $BNAME ] ; then
        mv $BNAME benchmark/gpu-results.html
        break
    fi
done
while [ true ] ; do
    sleep 1
    if [ -f $BNAME ] ; then
        mv $BNAME benchmark/multicore-results.html
        break
    fi
done
while [ true ] ; do
    sleep 1
    if [ -f $BNAME ] ; then
        mv $BNAME benchmark/sequential-results.html
        break
    fi
done
