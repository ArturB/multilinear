# Re-build whole project, with documentation
if stack test --coverage --no-run-tests ; then
    echo -e "Pushing changes to git..."
    ( git add -A > /dev/null && git commit -qm "Untested build $BUILD_ID" && git pull -q && git push -q ) &
    echo -e "All done!"
    exit 0
fi
