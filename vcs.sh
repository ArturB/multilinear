#!/bin/bash

source id-build.sh

# parse arguments
COMPILED=""
TESTED=""

i=0
if [ $# -gt 0 ] ; then
    while [ $i -le $# ] ; do
        case $1 in
            --compiled) COMPILED="true" ; ;; 
            --tested)   TESTED="true" ;   ;; 
            *) echo Unknown option $1 ;   ;; 
        esac
        let "i=$i+1"
        shift
    done
fi

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
        ( git add -A && git stash -q &&
          git checkout -qb "$DAILY_BRANCH" &&
          git stash pop -q &&
          if [ "$COMPILED" != "" ] ; then
              git commit -aqm "Build $BUILD_ID" && git push -q --set-upstream origin "$DAILY_BRANCH" 1>/dev/null 2>/dev/null
          else
              git commit -aqm "Temp $BUILD_ID"  && git push -q --set-upstream origin "$DAILY_BRANCH" 1>/dev/null 2>/dev/null
          fi
        ) &
    fi

# ON DAILY BRANCH
else
    if [ "$TESTED" != "" ] ; then
        ( git add -A && git commit -qm "Build $BUILD_ID" && git pull -q &&
          git checkout -q master &&
          git merge -q $CUR_BRANCH &&
          git branch -qD $CUR_BRANCH &&
          git push -q && git push -q origin --delete $CUR_BRANCH
        ) &
    else
        if [ "$COMPILED" != "" ] ; then
            ( git add -A && git commit -qm "Build $BUILD_ID" && git push -q ) &
        else
            ( git add -A && git commit -qm "Temp $BUILD_ID"  && git push -q ) &
        fi
    fi
fi
