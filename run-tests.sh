#!/bin/bash

if which sbcl
then
    sbcl <<EOF
(ql:quickload :modf-test)
(modf-test:run-tests)
(with-open-file (out #p"test-results" :if-exists :supersede :direction :output)
 (format out "sbcl: Success~%") )
EOF
fi

# cmucl
if which lisp
then
    lisp <<EOF
(ql:quickload :modf-test)
(modf-test:run-tests)
(with-open-file (out #p"test-results" :if-exists :append :direction :output)
 (format out "cmucl: Success~%") )
(quit)
EOF
fi

if which ccl
then
    ccl <<EOF
(ql:quickload :modf-test)
(modf-test:run-tests)
(with-open-file (out #p"test-results" :if-exists :append :direction :output)
 (format out "ccl: Success~%") )
EOF
fi

if which ccl64
then
    ccl64 <<EOF
(ql:quickload :modf-test)
(modf-test:run-tests)
(with-open-file (out #p"test-results" :if-exists :append :direction :output)
 (format out "ccl64: Success~%") )
EOF
fi

if which clisp
then
    clisp <<EOF
(ql:quickload :modf-test)
(modf-test:run-tests)
(with-open-file (out #p"test-results" :if-exists :append :direction :output)
 (format out "clisp: Success~%"))
EOF
fi

if which ecl
then
    ecl <<EOF
(ql:quickload :modf-test)
(modf-test:run-tests)
(with-open-file (out #p"test-results" :if-exists :append :direction :output)
 (format out "ecl: Success~%"))
EOF
fi

if which abcl
then
    abcl <<EOF
(ql:quickload :modf-test)
(modf-test:run-tests)
(with-open-file (out #p"test-results" :if-exists :append :direction :output)
 (format out "abcl: Success~%"))
EOF
fi
