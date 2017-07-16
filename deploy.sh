#!/bin/bash

# Build page.
git checkout master
make
cp -r page page_tmp

# Commit and push changes
git checkout gh-pages
mv -f page_tmp/* .
rmdir page_tmp
git commit -as -m "Deploy new page"
git push johnelse gh-pages

git checkout master
