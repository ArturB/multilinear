# Get current package name and build ID

# Get package name
PACKAGE_NAME=$( cat package.yaml | grep '^name:' | grep -o '[a-zA-z0-9]\+$' )
# Generate build ID
let "TODAY_SEC = $( date +%s ) % 86400"
BUILD_ID="$( date +%y%j ).$TODAY_SEC"
