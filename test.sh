# Re-build, prform all checks and tests and generate coverage report
# If all tests succeeded, copy coverage report to test/coverage
# and make a git commit&push

# Get package name
PACKAGE_NAME=$( cat package.yaml | grep '^name:' | grep -o '[a-zA-z0-9]\+$' )
# Generate build ID
let "TODAY_SEC = $( date +%s ) % 86400"
BUILD_ID="$( date +%y%j ).$TODAY_SEC"

echo -e "Checking $PACKAGE_NAME, build $BUILD_ID...\n"

# Perform whole-package checks: package version and up-to-date ChangeLog 
PACKAGE_VERSION=$( cat package.yaml | grep -o '[0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+' )
if [ -z $PACKAGE_VERSION ] ; then
    echo -e "\u001b[31mYou must specify 4-digit package version in package.yaml!\u001b[0m"
    echo -e "\u001b[31me.g. version: 1.2.3.4\u001b[0m"
    exit 1
fi
CHANGELOG_VERSION=$( cat ChangeLog.md | grep -o '[0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+' | head -n 1 )
if [ $PACKAGE_VERSION != $CHANGELOG_VERSION ] ; then
    echo -e "\u001b[31mChangeLog.md does not contain description for current package version!\u001b[0m"
    exit 1
else
    echo -e "Package version and ChangeLog.md up-to-date!"
    echo -e "Performing library tests...\n"
fi


# Perform library tests and push changes to git if all tests passed
if stack test --coverage ; then
    echo -e "\u001b[32mAll tests passed!\u001b[0m"
    echo -e "Pushing changes to git..."
    ( git add -A > /dev/null && git commit -qm "Build $BUILD_ID" && git pull -q && git push -q ) &
    echo -e "All done!"
    exit 0
else
    echo -e "\u001b[31mSome tests didn't pass!\u001b[0m"
    exit 1
fi
