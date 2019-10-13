#!/bin/bash
shopt -s globstar
shopt -s nullglob

brittany "$@" --write-mode=inplace {app,src,test}/**/*.hs
#stylish-haskell -i {app,src,test}/**/*.hs
