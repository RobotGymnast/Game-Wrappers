#!/bin/bash

dir=/tmp/testbuild/`pwd | sed "s/.*\///"`
echo Creating temporary build environment in $dir

if [ -d $dir/dist ]; then
    rm -rf $dir/.git &&
    cp -r --preserve=timestamps .git .gitignore *.cabal src "test" scripts $dir/
else
    mkdir -p $dir &&
    cp -r --preserve=timestamps * .* $dir/
fi &&

cd $dir &&
rm -f .git/hooks/pre-commit &&
git commit -m "Temp" > /dev/null &&
git reset --hard HEAD > /dev/null &&
git clean -fd > /dev/null &&
scripts/setup.sh &&
scripts/build.sh &&
echo &&
scripts/test.sh &&
echo &&
scripts/linecheck.sh &&
echo
