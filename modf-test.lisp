
(in-package :modf-test)

(in-root-suite)

(defsuite* lisp-types)

(deftest test-lists ()
  (let ((list '(1 2 (3 4) (5 (6 (7))) 8)))
    (is (equal list (modf (car list) 1)))
    (is (equal list (modf (cadr list) 2)))
    (is (equal list (modf (cadr (caddr list)) 4)))
    (is (equal list (modf (second (fourth list)) (list 6 (list 7)))))
    (is (equal list (modf (last list) (list 8))))
    ;; Make sure it is actually changing things
    (is (equal '(1 2 t nil "hello") (modf (third list) t
                                          & (fourth &) nil
                                          & (fifth &) "hello" )))))

(deftest test-arrays ()
  (let ((arr #(1 2 3 4 5 6)))
    (is (iter (for el1 in-sequence #(6 5 4 3 2 6))
          (for el2 in-sequence (modf (aref arr 0) 6
                                     & (aref & 1) 5
                                     & (aref & 2) 4
                                     & (aref & 3) 3
                                     & (aref & 4) 2 ))
          (always (eql el1 el2)) ))))

(modf-def:define-modf-for-class-slots (defclass test-class2 (test-parent)
                                        ((c :accessor c-of :initarg :c))) )

(deftest test-classes ()
  (let ((class1 (make-instance 'test-class1 :b 4 :a 7))
        (class2 (make-instance 'test-class2 :c t :a nil)))
    (is (eql 0 (b-of (modf (slot-value class1 'b) 0))))
    (is (eql 3 (a-of (modf (slot-value class1 'a) 3))))
    ;; This fails as it doesn't know how to handle it's parent.
    ;; (is (eql 4 (a-of (modf (a-of class1) 4))))
    (is (eql 2 (b-of (modf (b-of class1) 2))))
    (is (eql 'hello (c-of (modf (c-of class2) 'hello)))) ))

(modf-def:defstruct test-struct1 b)
(defstruct test-struct2 c)

(modf-def:define-modf-for-struct-slots (defstruct test-struct2 c))

(deftest test-structs ()
  (let ((s1 (make-test-struct1 :b 'b-slot))
        (s2 (make-test-struct2 :c 'c-slot)) )
    (is (eql 2 (test-struct1-b (modf (test-struct1-b s1) 2))))
    (is (eql -1 (test-struct2-c (modf (test-struct2-c s2) -1)))) ))

(deftest run-tests ()
  (test-lists)
  (test-arrays)
  (test-structs)
  (test-classes) )
