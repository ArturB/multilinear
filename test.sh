# Re-build and test project and generate coverage report
# If all tests succeeded, make a git commit&push

# Create build ID
let "TODAY_SEC = $( date +%s ) % 86400"
BUILD_ID="Build $( date +%y%j ).$TODAY_SEC"

stack test --coverage && ( $( git add -A && git commit -m "$BUILD_ID" && git push ) > /dev/null )
