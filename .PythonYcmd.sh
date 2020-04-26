#!/bin/bash

LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:~/Research/ycmd/third_party/clang/lib/ \
               python "$@"
