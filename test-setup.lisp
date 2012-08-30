
(defpackage :modf-test
  (:use :cl :stefil :modf :iter)
  (:export :run-tests))

(in-package :modf-test)

(defclass test-parent () ((a :accessor a-of :initarg :a)))
#+sbcl(closer-mop:finalize-inheritance (find-class 'test-parent))
(modf-def:defclass test-class1 (test-parent) ((b :accessor b-of :initarg :b)))
(defclass test-class2 (test-parent) ((c :accessor c-of :initarg :c)))
