#!/bin/bash

THIS_DIR=$(realpath $(dirname ${BASH_SOURCE}))
BUILDAPP=${BUILDAPP:-buildapp}


${BUILDAPP} --output test-app --dynamic-space-size 100000 --asdf-path ${THIS_DIR}/ --load-system test-system --entry 'cl-user::main' --dumpfile-copy ./dumpfile.lisp

