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

# ON MASTER BRANCH
if [ $CUR_BRANCH == "master" ] ; then
    if [ "$TESTED" != "" ] ; then
        ( git commit -aqm "Build $BUILD_ID" && git pull -q && git push -q ) &
    else
        git add -A && git stash 
        git checkout -b "$DAILY_BRANCH"
        git stash pop
        if [ "$COMPILED" != "" ] ; then
            ( git commit -aqm "Build $BUILD_ID" && git push -q --set-upstream origin "$DAILY_BRANCH" ) &
        else
            ( git commit -aqm "Temp $BUILD_ID"  && git push -q --set-upstream origin "$DAILY_BRANCH" ) &
        fi
    fi

# ON DAILY BRANCH
else
    if [ "$TESTED" != "" ] ; then
        git add -A && git commit -qm "Build $BUILD_ID" && git pull -q
        git checkout master
        git merge $CUR_BRANCH

    else
        if [ "$COMPILED" != "" ] ; then
            ( git add -A && git commit -qm "Build $BUILD_ID" && git push -q ) &
        else
            ( git add -A && git commit -qm "Temp $BUILD_ID"  && git push -q ) &
        fi
    fi
fi
