# Build whole project: library, tests, benchmarks, documentation 
if stack build --fast --test --coverage --no-run-tests --bench --no-run-benchmarks --haddock; then
    echo -e "Build successful!\nPushing changes to git..."
    ( git commit -aqm "Untested build $BUILD_ID" && git pull -q && git push -q ) &
    echo -e "All done!"
    exit 0
else
    echo -e "\u001b[31mBuild failure!\u001b[0m"
fi
