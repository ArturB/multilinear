# Re-build and test project and generate coverage report
# If all tests succeeded, make a git commit&push

# Create build ID
let "TODAY_SEC = $( date +%s ) % 86400"
BUILD_ID="Build $( date +%y%j ).$TODAY_SEC"

if stack test --coverage ; then
    echo -e "\u001b[32mAll tests passed!\u001b[0m"
    echo -e "Pushing changes to git..."
    git add -A > /dev/null && git commit -qm "$BUILD_ID" && got pull -q && git push -q
    echo -e "All done!"
else
    echo -e "\u001b[31mSome tests didn't pass!\u001b[0m"
fi
