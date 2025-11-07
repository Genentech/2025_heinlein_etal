#!/bin/bash

# set the proper remote name you’re using; ‘origin’ as the most commonly used default;
REMOTE_NAME=origin
CUR_BRANCH=$(git rev-parse --abbrev-ref HEAD)

for branch in `git branch -a | grep remotes | grep -v HEAD | grep -v $CUR_BRANCH`; do
   git branch --track ${branch#remotes/${REMOTE_NAME}/} $branch
done

