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
DAILY_BRANCH=$( date +%d-%m )
USER=$( whoami | tr '[:upper:]' '[:lower:]' )

# based on build state, commit to master or to daily branch
if [ $CUR_BRANCH == "master" ] ; then
    if [ "$TESTED" != "" ] ; then
        ( git commit -aqm "Build $BUILD_ID" && git pull -q && git push -q ) &
    else
        git branch "daily-$USER-$DAILY_BRANCH"
        git stash 
        git checkout -b "daily-$USER-$BUILD_ID"
        git stash pop
        if "$COMPILED" ; then
            ( git commit -qm "Build $BUILD_ID" && git push -q ) &
        else
            ( git commit -qm "Temp $BUILD_ID"  && git push -q ) &
        fi
    fi
fi

