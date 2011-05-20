
(in-package :modf)

(define-modf-expander cdr 1 (expr val new-val)
  (declare (ignore expr))
  `(cons (car ,val) ,new-val) )

(define-modf-expander car 1 (expr val new-val)
  (declare (ignore expr))
  `(cons ,new-val (cdr ,val)) )

(define-modf-expander slot-value 1 (expr val new-val)
  (with-gensyms (slot-name class slots new-instance slot)
    `(let* ((,slot-name ,(third expr))
            (,class (class-of ,val))
            (,slots (closer-mop:class-slots class))
            (,new-instance (make-instance class)))
       (loop for ,slot in ,slots do
         (if (eql ,slot-name (closer-mop:slot-definition-name ,slot))
             (setf (slot-value ,new-instance ,slot-name)
                   ,new-val )
             (setf (slot-value ,new-instance (closer-mop:slot-definition-name ,slot))
                   (slot-value ,val (closer-mop:slot-definition-name ,slot)))))
       ,new-instance )))

(defun replace-nth (nth list new-val)
  (if (> nth 0)
      (cons (car list) (replace-nth (- nth 1) (cdr list) new-val))
      (cons new-val (cdr list)) ))

(define-modf-function nth 2 (new-val nth obj)
  (replace-nth nth obj new-val) )

(defun replace-nthcdr (nth list new-val)
  (if (> nth 0)
      (cons (car list) (replace-nthcdr (- nth 1) (cdr list) new-val))
      new-val ))

(define-modf-function nthcdr 2 (new-val nth obj)
  (replace-nthcdr nth obj new-val) )

;; @\section{Array Manipulations}

;; @Arrays are inherently non-functional structures in Common Lisp.  This means
;; that these methods will be fundamentally inefficient.  However, they are
;; extremely convenient and, for small arrays or light usage, any inefficiency
;; will probably go unnoticed.

;; These use the Alexandria library.

(define-modf-function aref 1 (new-val array &rest idx)
  (let ((new-arr (alexandria:copy-array array)))
    (setf (apply #'aref new-arr idx) new-val)
    new-arr ))

(define-modf-function gethash 2 (new-val key hash-table)
  (let ((new-hash (alexandria:copy-hash-table hash-table)))
    (setf (gethash key new-hash) new-val)
    new-hash ))

