
(in-package :modf-test)

(in-root-suite)

(deftest run-tests ()
  (modf-eval-test)
  (fsetf-tests)
  (recursive-definitions)
  (late-invert)
  (test-lists)
  (test-arrays)
  (test-structs)
  (test-classes) )

;; We need to test to make sure certain recursive definitions are possible.
;; Because we make certain assumptions about the arguments in the case of a
;; missing inversion method, we need to have a argument order like NTH or
;; GETHASH.

(define-modf-function nth** 2 (new-val nth list)
  (if (= nth 0)
      (cons new-val (cdr list))
      (cons (car list)
            (modf (nth** (- nth 1) (modf-eval (cdr list)))
                  new-val ))))

(deftest recursive-definitions ()
  (remhash (intern (modf::modf-name 'nth*)) modf::*modf-nth-arg*)
  (unintern (intern (modf::modf-name 'nth*)))
  (define-modf-function nth* 2 (new-val nth list)
    (if (= nth 0)
        (cons new-val (cdr list))
        (cons (car list)
              (modf (nth* (- nth 1) (modf-eval (cdr list)))
                    new-val ))))
  (let ((list '(1 2 3 4)))
    (is (equal (modf (nth* 2 list) t) '(1 2 t 4)))
    (is (equal (modf (nth** 2 list) t) '(1 2 t 4))) ))

(deftest modf-eval-test ()
  (is (equal '(1 t 3 4) (modf (second (modf-eval '(1 2 3 4))) t))) )

(deftest fsetf-tests ()
  (let ((ima '((1 2 3) (4 5 6) (7 8 9))))
    (fsetf (second ima) 'second)
    (fsetf (second (first ima)) 'first-second
           (third ima) 'third )
    (is (equal '((1 first-second 3) second third)
               ima )) ))

(defclass late-parent () ((parent-slot :accessor parent-slot-of)))
(defclass late-child (late-parent) ((child-slot :accessor child-slot-of)))

(deftest late-invert ()
  (let ((obj (make-instance 'late-child)))
    (is (eql (child-slot-of (modf (child-slot-of obj) 'value)) 'value))
    (is (eql (parent-slot-of (modf (parent-slot-of obj) 'value)) 'value)) )
  (let ((obj (make-instance 'late-parent)))
    (is (eql (parent-slot-of (modf (parent-slot-of obj) 'value)) 'value)) ))

(defsuite* lisp-types)

(deftest test-lists ()
  (let ((list '(1 2 (3 4) (5 (6 (7))) 8)))
    (is (equal '(t 2 (3 4) (5 (6 (7))) 8) (modf (car list) t)))
    (is (equal '(1 t (3 4) (5 (6 (7))) 8) (modf (cadr list) t)))
    (is (equal '(1 2 (3 t) (5 (6 (7))) 8) (modf (cadr (caddr list)) t)))
    (is (equal '(1 2 (3 4) (5 (a b c)) 8) (modf (second (fourth list)) '(a b c))))
    (is (equal '(1 2 (3 4) (5 (6 (7))) . end) (modf (last list) 'end)))
    ;; Chaining
    (is (equal '(1 2 t nil "hello") (modf (third list) t
                                          & (fourth &) nil
                                          & (fifth &) "hello" )))
    ;; Apply and funcall.  These don't really make sense here, but whatever
    (is (equal '(one 2 (3 4) (5 (6 (7))) 8) (modf (funcall #'car list) 'one)))
    (is (equal '(1 two (3 4) (5 (6 (7))) 8) (modf (funcall #'car
                                                           (funcall #'cdr list) )
                                                  'two )))
    ;; (with-expected-failures
    ;;   ;; These should all fail.  This is because, like with setf, you can only
    ;;   ;; apply on certain functions (AREF and friends and user defined
    ;;   ;; functions).  I am thinking about removing this limitation.
    ;;   (is (equal '(one 2 (3 4) (5 (6 (7))) 8) (modf (apply #'car (modf-eval (list list)))
    ;;                                                 'one )))
    ;;   (is (equal '(1 two (3 4) (5 (6 (7))) 8) (modf (apply #'car
    ;;                                                        (apply #'cdr (modf-eval (list list))) nil )
    ;;                                                 'two ))))
    ))



(deftest test-arrays ()
  ;; One dimensional
  (let ((arr #(1 2 3 4 5 6))
        (arr-of-lists #((1 2 3) (4 5 6))) )
    (is (equalp #(t 2 3 4 5 6) (modf (aref arr 0) t)))
    ;; Funcall
    (is (equalp #(1 2 t 4 5 6) (modf (funcall #'aref arr 2) t)))
    (is (equalp #((1 2 t) (4 5 6)) (modf (funcall #'third
                                                  (funcall #'aref arr-of-lists 0))
                                         t )))
    ;; Apply
    (is (equalp #(1 t 3 4 5 6) (modf (apply #'aref arr '(1)) t)))
    (is (equalp #((1 2 3) (4 t 6)) (modf (funcall #'second
                                                  (apply #'aref arr-of-lists '(1) ))
                                         t )))
    ;; Chaining
    (is (iter (for el1 in-sequence #(6 5 4 3 2 6))
          (for el2 in-sequence (modf (aref arr 0) 6
                                     & (aref & 1) 5
                                     & (aref & 2) 4
                                     & (aref & 3) 3
                                     & (aref & 4) 2 ))
          (always (eql el1 el2)) )))
  ;; Two dimensional just to make sure (should be the same)
  (let ((arr #2A((1 2 3) (4 5 6))))
    (is (equalp #2A((t 2 3) (4 5 6)) (modf (aref arr 0 0) t)))
    ;; Funcall
    (is (equalp #2A((1 2 t) (4 5 6)) (modf (funcall #'aref arr 0 2) t)))
    ;; Apply
    (is (equalp #2A((1 t 3) (4 5 6)) (modf (apply #'aref arr '(0 1)) t))) ))

(modf-def:define-modf-for-class-slots (defclass test-class2 (test-parent)
                                        ((c :accessor c-of :initarg :c))) )

(deftest test-classes ()
  (let ((class1 (make-instance 'test-class1 :b 4 :a 7))
        (class2 (make-instance 'test-class2 :c t :a nil)))
    #+closer-mop
    (progn (is (eql 0 (b-of (modf (slot-value class1 'b) 0))))
           (is (eql 3 (a-of (modf (slot-value class1 'a) 3)))) )
    #-closer-mop
    (with-expected-failures
      (is (eql 0 (let ((class1 (make-instance 'test-class1 :b 4 :a 7)))
                   (b-of (modf (slot-value class1 'b) 0)) )))
      (is (eql 3 (let ((class1 (make-instance 'test-class1 :b 4 :a 7)))
                   (a-of (modf (slot-value class1 'a) 3)) ))))
    (is (eql 4 (let ((class1 (make-instance 'test-class1 :b 4 :a 7)))
                 (a-of (modf (a-of class1) 4)) ))
        "Failed to invert accessor function using heuristics." )
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

