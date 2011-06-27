#!/bin/bash

echo Test Results: > test-results

if which sbcl
then
    sbcl <<EOF
(ql:quickload :modf-test)
(stefil:without-debugging (modf-test:run-tests))
(with-open-file (out #p"test-results" :if-exists :append :direction :output)
 (format out "sbcl: ~A~%" stefil:*last-test-result*) )
EOF
fi

# cmucl
if which lisp
then
    lisp <<EOF
(ql:quickload :modf-test)
(stefil:without-debugging (modf-test:run-tests))
(with-open-file (out #p"test-results" :if-exists :append :direction :output)
 (format out "cmucl: ~A~%" stefil:*last-test-result*) )
(quit)
EOF
fi

if which ccl
then
    ccl <<EOF
(ql:quickload :modf-test)
(stefil:without-debugging (modf-test:run-tests))
(with-open-file (out #p"test-results" :if-exists :append :direction :output)
 (format out "ccl: ~A~%" stefil:*last-test-result*) )
EOF
fi

if which ccl64
then
    ccl64 <<EOF
(ql:quickload :modf-test)
(stefil:without-debugging (modf-test:run-tests))
(with-open-file (out #p"test-results" :if-exists :append :direction :output)
 (format out "ccl64: ~A~%" stefil:*last-test-result*) )
EOF
fi

if which clisp
then
    clisp <<EOF
(ql:quickload :modf-test)
(stefil:without-debugging (modf-test:run-tests))
(with-open-file (out #p"test-results" :if-exists :append :direction :output)
 (format out "clisp: ~A~%" stefil:*last-test-result*))
EOF
fi

if which ecl
then
    ecl <<EOF
(ql:quickload :modf-test)
(stefil:without-debugging (modf-test:run-tests))
(with-open-file (out #p"test-results" :if-exists :append :direction :output)
 (format out "ecl: ~A~%" stefil:*last-test-result*))
EOF
fi

if which abcl
then
    abcl <<EOF
(ql:quickload :modf-test)
(stefil:without-debugging (modf-test:run-tests))
(with-open-file (out #p"test-results" :if-exists :append :direction :output)
 (format out "abcl: ~A~%" stefil:*last-test-result*))
EOF
fi
