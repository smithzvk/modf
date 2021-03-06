
(in-package :modf-test)

(in-root-suite)

(deftest run-tests ()
  (modf-eval-test)
  (fsetf-tests)
  (recursive-definitions)
  (late-invert-class)
  (late-invert-struct)
  (test-lists)
  (test-arrays)
  (test-structs)
  (test-classes))

#+closer-mop
(defun class-equal (obj1 obj2)
  (let* ((class (class-of obj1))
         (slots (closer-mop:class-slots class)))
    (is (eql class (class-of obj2)))
    (iter (for slot in slots)
      (cond ((slot-boundp obj1 (closer-mop:slot-definition-name slot))
             (is (equal
                  (slot-value obj1 (closer-mop:slot-definition-name slot))
                  (slot-value obj2 (closer-mop:slot-definition-name slot)))))
            (t (is (not (slot-boundp
                         obj2 (closer-mop:slot-definition-name slot)))))))))

;; We need to test to make sure certain recursive definitions are possible.
;; Because we make certain assumptions about the arguments in the case of a
;; missing inversion method, we need to have a argument order like NTH or
;; GETHASH.

(define-modf-function nth** 2 (new-val nth list)
  (if (= nth 0)
      (cons new-val (cdr list))
      (cons (car list)
            (modf (nth** (- nth 1) (modf-eval (cdr list)))
                  new-val))))

(deftest recursive-definitions ()
  (remhash (intern (modf::modf-name 'nth*)) modf::*modf-nth-arg*)
  (unintern (intern (modf::modf-name 'nth*)))
  (define-modf-function nth* 2 (new-val nth list)
    (if (= nth 0)
        (cons new-val (cdr list))
        (cons (car list)
              (modf (nth* (- nth 1) (modf-eval (cdr list)))
                    new-val))))
  (let ((list '(1 2 3 4)))
    (is (equal (modf (nth* 2 list) t) '(1 2 t 4)))
    (is (equal (modf (nth** 2 list) t) '(1 2 t 4)))))

(deftest modf-eval-test ()
  (is (equal '(1 t 3 4) (modf (second (modf-eval '(1 2 3 4))) t))))

(deftest fsetf-tests ()
  (let ((ima '((1 2 3) (4 5 6) (7 8 9))))
    (fsetf (second ima) 'second)
    (fsetf (second (first ima)) 'first-second
           (third ima) 'third)
    (is (equal '((1 first-second 3) second third)
               ima))))

(defclass late-parent ()
  ((parent-slot :accessor parent-slot-of
                :initarg :parent-slot)))
(defclass late-child (late-parent)
  ((child-slot :accessor child-slot-of
               :initarg :child-slot)))

(deftest late-invert-class ()
  (let ((obj (make-instance 'late-child)))
    (class-equal (modf (child-slot-of obj) 'value)
                 (make-instance 'late-child
                                :child-slot 'value))
    (class-equal (modf (parent-slot-of obj) 'value)
                 (make-instance 'late-child
                                :parent-slot 'value)))
  (let ((obj (make-instance 'late-child
                            :parent-slot 'a
                            :child-slot 'b)))
    (class-equal (modf (child-slot-of obj) 'value)
                 (make-instance 'late-child
                                :parent-slot (parent-slot-of obj)
                                :child-slot 'value))
    (class-equal (modf (parent-slot-of obj) 'value)
                 (make-instance 'late-child
                                :parent-slot 'value
                                :child-slot (child-slot-of obj))))
  (let ((obj (make-instance 'late-parent)))
    (class-equal (modf (parent-slot-of obj) 'value)
                 (make-instance 'late-parent
                                :parent-slot 'value))))

(defstruct late-struct a b c)

(defun ls-equal (obj1 obj2)
  (and (is (equal (late-struct-a obj1) (late-struct-a obj2)))
       (is (equal (late-struct-b obj1) (late-struct-b obj2)))
       (is (equal (late-struct-c obj1) (late-struct-c obj2)))))

(defstruct late-included included)
(defstruct (late-including (:include late-included)) a b c)

(defun li-equal (obj1 obj2)
  (and (is (equal (late-including-a obj1) (late-including-a obj2)))
       (is (equal (late-including-b obj1) (late-including-b obj2)))
       (is (equal (late-including-c obj1) (late-including-c obj2)))
       (is (equal (late-including-included obj1)
                  (late-including-included obj2)))))

(deftest late-invert-struct ()
  (let ((modf::*accessor-heuristics* t))
    (let ((str1 (make-late-struct :a 1 :b 2 :c 3))
          (str2 (make-late-struct)))
      (ls-equal (modf (late-struct-a str1) t)
                (make-late-struct :a t :b 2 :c 3))
      (ls-equal (modf (late-struct-a str2) t)
                (make-late-struct :a t))
      (ls-equal (modf (late-struct-c str1) t)
                (make-late-struct :a 1 :b 2 :c t))
      (ls-equal (modf (late-struct-c str2) t)
                (make-late-struct :c t)))
    (with-expected-failures
      (let ((str1 (make-late-including :a 1 :b 2 :c 3 :included 4))
            (str2 (make-late-including)))
        (li-equal (modf (late-including-a str1) t)
                  (make-late-including :a t :b 2 :c 3))
        (li-equal (modf (late-including-a str2) t)
                  (make-late-including :a t))
        (li-equal (modf (late-including-included str1) t)
                  (make-late-including :a 1 :b 2 :c 3 :included t))
        (li-equal (modf (late-including-included str2) t)
                  (make-late-including :included t))
        ;; Test using the accessors of an included object
        (li-equal (modf (late-included-included str1) t)
                  (make-late-including :a 1 :b 2 :c 3 :included t))
        (li-equal (modf (late-included-included str2) t)
                  (make-late-including :included t))))))

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
                                          & (fifth &) "hello")))
    ;; Apply and funcall.  These don't really make sense here, but whatever
    (is (equal '(one 2 (3 4) (5 (6 (7))) 8) (modf (funcall #'car list) 'one)))
    (is (equal '(1 two (3 4) (5 (6 (7))) 8) (modf (funcall #'car
                                                           (funcall #'cdr list))
                                                  'two)))
    ;; (with-expected-failures
    ;;   ;; These should all fail.  This is because, like with setf, you can only
    ;;   ;; apply on certain functions (AREF and friends and user defined
    ;;   ;; functions).  I am thinking about removing this limitation.
    ;;   (is (equal '(one 2 (3 4) (5 (6 (7))) 8) (modf (apply #'car (modf-eval (list list)))
    ;;                                                 'one)))
    ;;   (is (equal '(1 two (3 4) (5 (6 (7))) 8) (modf (apply #'car
    ;;                                                        (apply #'cdr (modf-eval (list list))) nil)
    ;;                                                 'two))))
    ))

(deftest test-plists ()
  (let* ((string-key "string-key")
         (plist `(:a 1 :b 2 :b 5 ,string-key 10)))
    (is (equal (modf (getf plist :a) 0)
               `(:a 0 :b 2 :b 5 ,string-key 10)))
    (is (equal (modf (getf plist :b) 0)
               `(:a 1 :b 0 :b 5 ,string-key 10)))
    (is (equal (modf (getf plist :not-present) 0)
               `(:not-present 0 :a 1 :b 2 :b 5 ,string-key 10)))
    (is (equal (modf (getf plist string-key) 100)
               `(:a 1 :b 2 :b 5 ,string-key 100)))
    (let ((equal-but-not-eq (string-downcase (string-upcase string-key))))
      (is (equal (modf (getf plist equal-but-not-eq) 100)
                 `(,string-key 100 :a 1 :b 2 :b 5 ,string-key 10))))))


(deftest test-arrays ()
  ;; One dimensional
  (let ((arr #(1 2 3 4 5 6))
        (arr-of-lists #((1 2 3) (4 5 6))))
    (is (equalp #(t 2 3 4 5 6) (modf (aref arr 0) t)))
    ;; Funcall
    (is (equalp #(1 2 t 4 5 6) (modf (funcall #'aref arr 2) t)))
    (is (equalp #((1 2 t) (4 5 6)) (modf (funcall #'third
                                                  (funcall #'aref arr-of-lists 0))
                                         t)))
    ;; Apply
    (is (equalp #(1 t 3 4 5 6) (modf (apply #'aref arr '(1)) t)))
    (is (equalp #((1 2 3) (4 t 6)) (modf (funcall #'second
                                                  (apply #'aref arr-of-lists '(1)))
                                         t)))
    ;; Chaining
    (is (iter (for el1 in-sequence #(6 5 4 3 2 6))
          (for el2 in-sequence (modf (aref arr 0) 6
                                     & (aref & 1) 5
                                     & (aref & 2) 4
                                     & (aref & 3) 3
                                     & (aref & 4) 2))
          (always (eql el1 el2)))))
  ;; Two dimensional just to make sure (should be the same)
  (let ((arr #2A((1 2 3) (4 5 6))))
    (is (equalp #2A((t 2 3) (4 5 6)) (modf (aref arr 0 0) t)))
    ;; Funcall
    (is (equalp #2A((1 2 t) (4 5 6)) (modf (funcall #'aref arr 0 2) t)))
    ;; Apply
    (is (equalp #2A((1 t 3) (4 5 6)) (modf (apply #'aref arr '(0 1)) t)))))

(modf-def:define-modf-for-class-slots (defclass test-class2 (test-parent)
                                        ((c :accessor c-of :initarg :c))))

(deftest test-classes ()
  (let ((class1 (make-instance 'test-class1 :b 4 :a 7))
        (class2 (make-instance 'test-class2 :c t :a nil)))
    #+closer-mop
    (progn (is (eql 0 (b-of (modf (slot-value class1 'b) 0))))
           (is (eql 3 (a-of (modf (slot-value class1 'a) 3)))))
    #-closer-mop
    (with-expected-failures
      (is (eql 0 (let ((class1 (make-instance 'test-class1 :b 4 :a 7)))
                   (b-of (modf (slot-value class1 'b) 0)))))
      (is (eql 3 (let ((class1 (make-instance 'test-class1 :b 4 :a 7)))
                   (a-of (modf (slot-value class1 'a) 3))))))
    (is (eql 4 (let ((class1 (make-instance 'test-class1 :b 4 :a 7)))
                 (a-of (modf (a-of class1) 4))))
        "Failed to invert accessor function using heuristics.")
    (is (eql 2 (b-of (modf (b-of class1) 2))))
    (is (eql 'hello (c-of (modf (c-of class2) 'hello))))))

(modf-def:defstruct test-struct1 b)
(defstruct test-struct2 c)

(modf-def:define-modf-for-struct-slots (defstruct test-struct2 c))

(deftest test-structs ()
  (let ((s1 (make-test-struct1 :b 'b-slot))
        (s2 (make-test-struct2 :c 'c-slot)))
    (is (eql 2 (test-struct1-b (modf (test-struct1-b s1) 2))))
    (is (eql -1 (test-struct2-c (modf (test-struct2-c s2) -1))))))

