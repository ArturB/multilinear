# Re-build and test project and generate coverage report
# If all tests succeeded, make a git commit&push

# Create build ID
let "TODAY_SEC = $( date +%s ) % 86400"
BUILD_ID="Build $( date +%y%j ).$TODAY_SEC"

if stack test --coverage && git add -A > /dev/null && git commit -qm "$BUILD_ID" && git push -q ; then
    echo -e "\u001b[32mAll done!\u001b[0m"
else
    echo -e "\u001b[31mSome tests didn't pass!\u001b[0m"
fi
