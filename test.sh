#!/usr/bin/env sh
sbcl \
    --noinform \
    --end-runtime-options \
    --no-sysinit \
    --no-userinit \
    --disable-debugger \
    --eval "(load \"~/quicklisp/setup.lisp\")" \
    --eval "(push \"$GITHUB_WORKSPACE\" ql:*local-project-directories*)" \
    --eval "(ql:quickload :sbu/cli/tests)" \
    --eval "(ql:quickload :sbu/tests)" \
    --eval "(unless (sbu/cli/tests:run-tests) (sb-ext:exit :code 2 :abort t))" \
    --quit
