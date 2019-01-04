source id-build.sh

# parse arguments
COMPILED=""
TESTED=""

i=0
while [ $i -le $# ] ; do
    case $1 in
        --compiled) COMPILED="true" ; ;; 
        --tested)   TESTED="true" ;   ;; 
        *) echo Unknown option $1 ;   ;; 
    esac
    let "i=$i+1"
    shift
done

# get current branch name
CUR_BRANCH=$( git rev-parse --abbrev-ref HEAD )
USER=$( whoami | tr '[:upper:]' '[:lower:]' )
DAILY_BRANCH="daily-$USER-$BUILD_ID"

# based on build state, commit to master or to daily branch
if [ $CUR_BRANCH == "master" ] ; then
    if [ "$TESTED" != "" ] ; then
        ( git commit -aqm "Build $BUILD_ID" && git pull -q && git push -q ) &
    else
        git stash 
        git checkout -b "DAILY_BRANCH"
        git stash pop
        if [ "$COMPILED" != "" ] ; then
            ( git commit -aqm "Build $BUILD_ID" && git push -q ) &
        else
            ( git commit -aqm "Temp $BUILD_ID"  && git push -q ) &
        fi
    fi
fi

