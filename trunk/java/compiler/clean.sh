#!/bin/sh

## Delete files according to the patterns defined in the .svnignore file.
while read PATTERN
do
    rm -rf $PATTERN
done < .svnignore