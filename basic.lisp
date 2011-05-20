
(in-package :modf)

(define-modf-expander cdr (expr val new-val)
  (declare (ignore expr))
  (values
    `(cons (car ,val) ,new-val)
    #'second ))

(define-modf-expander car (expr val new-val)
  (declare (ignore expr))
  (values
    `(cons ,new-val (cdr ,val))
    #'second ))

(define-modf-expander slot-value (expr val new-val)
  (let ((slot-name (third expr)))
    (values
      `(let* ((class (class-of ,val))
              (slots (closer-mop:class-slots class))
              (new-instance (make-instance class)))
         (loop for slot in slots do
           (if (eql ,slot-name (closer-mop:slot-definition-name slot))
               (setf (slot-value new-instance ,slot-name)
                     ,new-val )
               (setf (slot-value new-instance (closer-mop:slot-definition-name slot))
                     (slot-value ,val (closer-mop:slot-definition-name slot)))))
         new-instance )
      #'second )))

(defun replace-nth (nth list new-val)
  (if (> nth 0)
      (cons (car list) (replace-nth (- nth 1) (cdr list) new-val))
      (cons new-val (cdr list)) ))

(define-modf-expander nth (expr val new-val)
  (values
    `(replace-nth ,(second expr) ,val ,new-val)
    #'third ))

