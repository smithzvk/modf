
(in-package :modf)

(defpackage :modf-def
    (:use :cl :modf :iter)
  (:shadow cl:defstruct cl:defclass)
  (:export #:defstruct #:defclass
           #:define-modf-for-class-slots
           #:define-modf-for-struct-slots ))

(in-package :modf-def)

(defmacro defstruct (name-and-options &rest slot-descriptions)
  "Define a new structure with Modf expansions for slots."
  `(progn (cl:defstruct ,name-and-options ,@slot-descriptions)
          (define-modf-for-struct-slots
              (defstruct ,name-and-options ,@slot-descriptions) )))

(defun modf-for-struct-slots-expander (defstruct name-and-options
                                        &rest slot-descriptions )
  ;; We need the name and "conc-name" (what is prepended to the accessor
  ;; functions) in order to define our Modf expansions
  (declare (ignore defstruct))
  (destructuring-bind (&key name
                       (conc-name (concatenate 'string (symbol-name name) "-")) )
      (if (atom name-and-options) (list :name name-and-options)
          (append
           (list :name (first name-and-options))
           (let ((conc-name-option
                  (find :conc-name (rest name-and-options)
                        :key (lambda (x) (if (atom x) x (first x))) )))
             (if (or (atom conc-name-option) (null (rest conc-name-option)))
                 (list :conc-name "")
                 (list :conc-name (symbol-name (second conc-name-option))) ))))
    (iter (for slot in slot-descriptions)
          (let ((accessor (intern (concatenate
                                   'string conc-name
                                   (symbol-name (if (atom slot)
                                                    slot
                                                    (first slot) ))))))
            (collecting
             `(define-modf-function ,accessor 1 (new-val object)
                (let ((new-struct (copy-structure object)))
                  (setf (,accessor new-struct) new-val)
                  new-struct )))))))

(defmacro define-modf-for-struct-slots (structure-definition-form)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(apply #'modf-for-struct-slots-expander structure-definition-form) ))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
                (let ((rest (nthcdr n source)))
                  (if (consp rest)
                      (rec rest (cons (subseq source 0 n) acc))
                      (nreverse (cons source acc)) ))))
    (if source (rec source nil) nil) ))

(defun group-by (list &rest counts)
  (let ((ret list))
    (dolist (cnt counts ret)
      (setf ret (group ret cnt)) )))

(defmacro defclass (name direct-superclasses direct-slots &rest options)
  "Define Modf expansions for class slot accessor and reader methods."
  ;; We need the names of all methods that access data in the object and what
  ;; slot they are associated with.
  `(progn
     (cl:defclass ,name ,direct-superclasses ,direct-slots ,@options)
          ,@(iter :outer
                  (for slot in direct-slots)
                  (let ((slot-name (if (atom slot) slot (first slot))))
                    (iter (for accessor in (remove-if-not
                                            (lambda (x) (member
                                                    (first x)
                                                    '(:accessor :reader) ))
                                            (group-by (rest slot) 2) ))
                          (in :outer
                              (collecting
                               `(define-modf-method ,(second accessor) 1
                                    (new-val (obj ,name))
                                  (modf (slot-value obj ',slot-name) new-val) ))))))))

(defun modf-for-class-slots-expander (class)
  ;; We need the names of all methods that access data in the object and what
  ;; slot they are associated with.
  (if (consp class)
      ;; This is a definition form
      (destructuring-bind (defclass name direct-superclasses direct-slots &rest options)
          class
        (declare (ignore defclass direct-superclasses options))
        (iter :outer
              (for slot in direct-slots)
              (let ((slot-name (if (atom slot) slot (first slot))))
                (iter (for accessor in (remove-if-not
                                        (lambda (x) (member
                                                (first x)
                                                '(:accessor :reader) ))
                                        (group-by (rest slot) 2) ))
                      (in :outer
                          (collecting
                           `(define-modf-method ,(second accessor) 1
                                (new-val (obj ,name))
                              (modf (slot-value obj ',slot-name) new-val) )))))))
      ;; This must be a class object or name
      (let ((class (find-class class)))
        (unless (closer-mop:class-finalized-p class)
          (error "Class ~S not finalized." class) )
        (iter
          :outer
          (for slot in (closer-mop:class-direct-slots class))
          (iter (for reader in (closer-mop:slot-definition-readers slot))
                (in :outer
                    (collecting
                     `(define-modf-method ,reader 1
                          (new-val (obj ,class))
                        (modf (slot-value obj ',(closer-mop:slot-definition-name
                                                 slot ))
                              new-val) ))))))))

(defmacro define-modf-for-class-slots (class-name-or-definition)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(modf-for-class-slots-expander class-name-or-definition) ))

