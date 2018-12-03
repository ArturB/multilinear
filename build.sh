# Re-build whole project, with documentation 
if stack build --fast --test --coverage --no-run-tests --bench --no-run-benchmarks; then
    echo -e "Build successful!\nPushing changes to git..."
    ( git add -A > /dev/null && git commit -qm "Untested build $BUILD_ID" && git pull -q && git push -q ) &
    echo -e "All done!"
    exit 0
fi
