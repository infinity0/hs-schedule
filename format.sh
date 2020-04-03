#!/bin/bash
shopt -s globstar
shopt -s nullglob

sed -i -e 's/= proc /= \\/' -e 's/\$ proc /$ \\/' src/Control/Arrow/Schedule.hs
brittany "$@" --write-mode=inplace {app,src,test}/**/*.hs
stylish-haskell -i {app,src,test}/**/*.hs
sed -i -e 's/= \\/= proc /' -e 's/\$ \\/$ proc /' src/Control/Arrow/Schedule.hs
hlint -h hlint.yml src test
