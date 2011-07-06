
(in-package :modf)

(define-modf-expander cdr 1 (expr val new-val)
  (declare (ignore expr))
  `(cons (car ,val) ,new-val) )

(define-modf-expander car 1 (expr val new-val)
  (declare (ignore expr))
  `(cons ,new-val (cdr ,val)) )

#+closer-mop
(defun copy-instance (obj)
  (let* ((class (class-of obj))
         (slots (closer-mop:class-slots class))
         (new-instance (make-instance class)))
    (loop for slot in slots do
             (cond ((slot-boundp obj (closer-mop:slot-definition-name slot))
                    (setf (slot-value new-instance (closer-mop:slot-definition-name
                                                    slot ))
                          (slot-value obj (closer-mop:slot-definition-name slot)) ))
                   (t (slot-makunbound new-instance
                                       (closer-mop:slot-definition-name slot) ))))
    new-instance ))
#-closer-mop
(defun copy-instance (obj)
  (error "Sorry, I need Closer-MOP to Modf classes.") )

(define-modf-expander slot-value 1 (expr val new-val)
  `(let ((new (copy-instance ,val)))
     (setf (slot-value new ,(third expr)) ,new-val)
     new ))

(define-modf-method pathname-directory 1 (new-val path)
  (make-pathname :directory new-val :defaults path) )

(define-modf-method pathname-name 1 (new-val path)
  (make-pathname :name new-val :defaults path) )

(define-modf-method pathname-type 1 (new-val path)
  (make-pathname :type new-val :defaults path) )

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

(define-modf-function last 1 (new-val obj)
  (let ((len (length obj)))
    (replace-nthcdr (- len 1) obj new-val)) )

(define-modf-function subseq 1 (new-val seq start &optional (end (length seq)))
  (concatenate (type-of seq)
               (subseq seq 0 start)
               (subseq new-val 0 (min (length new-val) (- end start)))
               (subseq seq (+ start (min (length new-val) (- end start)))) ))

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

;; @\section{Hash Table Manipulations}

;; @Same deal as arrays.  Avoid using this for large hash tables which, quite
;; frankly, is the most common use case for any hash tables.  It is just here
;; for completeness.  If you are using a hash table and need to functionally
;; modify it many times, then use a functional dictionary structure, like an
;; FSet map or a FUNDS dictionary.

(define-modf-function gethash 2 (new-val key hash-table)
  (let ((new-hash (alexandria:copy-hash-table hash-table)))
    (setf (gethash key new-hash) new-val)
    new-hash ))

