#!/bin/bash

# Build page.
git checkout master
make
cp -r page page_tmp

COMMIT=`git rev-parse --short master`

# Commit and push changes
git checkout gh-pages
mv -f page_tmp/* .
rmdir page_tmp
git commit -as -m "Deploy new page from $COMMIT"
git push johnelse gh-pages

git checkout master
