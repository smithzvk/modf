
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

(define-modf-expander nth 2 (expr val new-val)
  `(replace-nth ,(second expr) ,val ,new-val))


