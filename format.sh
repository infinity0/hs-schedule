#!/bin/bash
shopt -s globstar
shopt -s nullglob

sed -i -e 's/= proc /= \\/' -e 's/\$ proc /$ \\/' src/Control/Arrow/Schedule.hs
sed -i -e 's/\$(/(THSplice /' test/Control/Static/ScheduleTest.hs
brittany "$@" --write-mode=inplace {app,src,test}/**/*.hs
sed -i -e 's/(THSplice /$(/' test/Control/Static/ScheduleTest.hs
sed -i -e 's/= \\/= proc /' -e 's/\$ \\/$ proc /' src/Control/Arrow/Schedule.hs

stylish-haskell -i {app,src,test}/**/*.hs

hlint -h hlint.yml src test
