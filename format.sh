#!/bin/bash
shopt -s globstar
shopt -s nullglob

sed -i -e 's/= proc /= \\/' -e 's/\$ proc /$ \\/' src/Control/Arrow/Transformer/Schedule.hs
brittany "$@" --write-mode=inplace {app,src,test}/**/*.hs
sed -i -e 's/= \\/= proc /' -e 's/\$ \\/$ proc /' src/Control/Arrow/Transformer/Schedule.hs
#stylish-haskell -i {app,src,test}/**/*.hs
