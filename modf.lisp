
(in-package :modf)

;; @\section{Exapansions}

;; @

;; <<>>=
(defvar *modf-expansions*
  (make-hash-table)
  "Holds expansion functions" )

;; <<>>=
(defvar *modf-nth-arg* (make-hash-table)
  "Holds what argument to try to invert next." )

;; <<>>=
(defmacro define-modf-expander (name nth-arg
                                (expr val new-val)
                                &body body )
  "Define a new expander which inverts forms starting with NAME.  Your function
should return an expansion from EXPR to a form that will build a new object that
has NEW-VAL in the place specified by expr.  NTH-ARG marks which argument is
considered the actual data which will be inverted next."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf
      (gethash ',name *modf-expansions*)
      (lambda (,expr ,val ,new-val)
        ,@body )
      (gethash ',name *modf-nth-arg*)
      ,nth-arg )))

;; @\section{Rewrite Rules}

;; @Rewrite rules are basically macros for the {\em modf} expander.
;; <<*modf-rewrites*>> holds the macro expansion functions.

;; <<>>=
(defvar *modf-rewrites*
  (make-hash-table) )

;; <<>>=
(defmacro define-modf-rewrite (name (expr) &body body)
  "Define a new rewrite rule.  If a form starting with NAME is encountered, call
the defined function to return a form that we can deal with (i.e. one defined
via DEFINE-MODF-EXPANDER, DEFINE-MODF-FUNCTION, and DEFINE-MODF-METHOD)."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name *modf-rewrites*)
           (lambda (,expr)
             ,@body ))))

;; @\section{Defining Modf functions and methods}

;; @Much like you can define (setf accessor) functions and methods, you can do
;; the same with Modf.  This is done with <<define-modf-function>> and
;; <<define-modf-method>>.  Unfortunately we can't use syntactic sugar like
;; (defun (modf car) ...) without defining our own defun macro which is a bit
;; heavy handed.

;; <<>>=
(defun modf-name (symbol)
  "Make a symbol name that depends on symbol name and package, but is very
unlikely to be chosen by anyone.  This is for avoiding collisions for my
benefit, not the users, as these symbols belong to the MODF package."
  (mkstr "BUILDER:"
         (package-name (symbol-package symbol))
         ":" (symbol-name symbol) ))

;; <<>>=
(defmacro define-modf-function (name nth-arg (new-val &rest args) &body body)
  "Define a new modf function.  It inverts NAME forms by modifying the NTH-ARG
term of the arguments of the place form in the MODF macro."
  `(progn
     (defun ,(intern (modf-name name) :modf) (,new-val ,@args)
       ,@body )
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ',name *modf-nth-arg*)
             ,nth-arg ))))

;; <<>>=
(defmacro define-modf-method (name nth-arg (new-val &rest args) &body body)
  "Define a new modf method.  It inverts NAME forms by modifying the NTH-ARG
term of the arguments of the place form in the MODF macro.  This method can
specialize on any of ARGS."
  `(progn
     (defmethod ,(intern (modf-name name) :modf) (,new-val ,@args)
       ,@body )
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ',name *modf-nth-arg*)
             ,nth-arg ))))

;; @\section{The {\em modf} macro}

;; @The <<modf>> macro is the main entry point to the library.  It passes its
;; arguments to the <<modf-expand>> function.

;; The basic syntax of the <<modf>> macro is (modf place new-value) where place
;; is a normal form for accessing a part of some data.  Modf will invert this
;; form into a form that will build a new hunk of data equivalent to the old
;; except that the particular specified place will have the value new-value.  It
;; inverts based on the rules and functionds defined via define-modf-rewrite,
;; define-modf-expander, define-modf-method, and define-modf-function, in that
;; order.  It recognizes one special form, modf-eval, which instructs it to not
;; try to invert the enclosed form and just evaluate it.

(defun container-arg-n (expr)
  (cond ((eql (car expr) 'cl:apply)
         (1+ (gethash (cadadr expr) *modf-nth-arg*)) )
        ((eql (car expr) 'cl:funcall)
         (1+ (gethash (cadadr expr) *modf-nth-arg*)) )
        (t (gethash (car expr) *modf-nth-arg*)) ))

(defun modf-fn-defined? (expr)
  (cond ((eql (car expr) 'cl:apply)
         (fboundp (intern (modf-name (cadadr expr)) :modf)) )
        ((eql (car expr) 'cl:funcall)
         (fboundp (intern (modf-name (cadadr expr)) :modf)) )
        (t (fboundp (intern (modf-name (car expr)) :modf))) ))

(defun expansions-defined? (expr)
  (cond ((eql (car expr) 'cl:apply)
         (gethash (cadadr expr) *modf-expansions*) )
        ((eql (car expr) 'cl:funcall)
         (gethash (cadadr expr) *modf-expansions*) )
        (t (gethash (car expr) *modf-expansions*)) ))

(defun accessor-in (expr)
  (case (car expr)
    (cl:apply (cadadr expr))
    (cl:funcall (cadadr expr))
    (otherwise (car expr)) ))

(defun apply-expression? (expr)
  (eql (car expr) 'cl:apply) )

(defun funcall-expression? (expr)
  (eql (car expr) 'cl:funcall) )

;; <<>>=
(defun modf-expand (new-val expr form)
  (cond ((atom expr)
         new-val )
        ((eql (car expr) 'modf-eval)
         new-val )
        ((gethash (car expr) *modf-rewrites*)
         (modf-expand new-val (funcall (gethash (car expr) *modf-rewrites*) expr)
                      form ))
        ((modf-fn-defined? expr)
         (let ((enclosed-obj-sym (gensym)) )
           (modf-expand
            `(let ((,form
                    ,(let ((enclosed-obj (nth (container-arg-n expr)
                                              expr )))
                       (let ((it (and (consp enclosed-obj)
                                      (container-arg-n enclosed-obj) )))
                         (if it
                             (replace-nth it
                                          enclosed-obj
                                          enclosed-obj-sym )
                             enclosed-obj )))))
               ,(cond ((apply-expression? expr)
                       `(apply (function ,(intern (modf-name (cadadr expr)) :modf))
                               ,new-val
                               ,@(cddr
                                  (replace-nth
                                   (container-arg-n expr)
                                   expr form ))))
                      ((funcall-expression? expr)
                       `(funcall (function ,(intern (modf-name (cadadr expr)) :modf))
                                 ,new-val
                                 ,@(cddr
                                    (replace-nth
                                     (container-arg-n expr)
                                     expr form ))))
                      (t
                       `(,(intern (modf-name (car expr)) :modf)
                         ,new-val
                         ,@(cdr
                            (replace-nth
                             (container-arg-n expr)
                             expr form ))))))
            (nth (container-arg-n expr) expr)
            enclosed-obj-sym )))
        ((expansions-defined? expr)
         (let ((enclosed-obj-sym (gensym)) )
           (multiple-value-bind (builder)
               (funcall (gethash (accessor-in expr) *modf-expansions*)
                        expr form new-val )
             (modf-expand
              `(let ((,form
                      ,(let ((enclosed-obj (nth (container-arg-n expr)
                                                expr )))
                         (let ((it (and (consp enclosed-obj)
                                        (container-arg-n enclosed-obj) )))
                           (if it
                               (replace-nth it
                                            enclosed-obj
                                            enclosed-obj-sym )
                               enclosed-obj )))))
                 ,builder )
              (nth (container-arg-n expr) expr)
              enclosed-obj-sym ))))
        (t (error "Don't know how to handle \"~A\"" expr)) ))

;; <<>>=
(defmacro modf (place value &rest args)
  "Make a new object \(which may use some of the old object) such that PLACE
evaluates to VALUE.

ARGS should have the form...

ARGS : NIL
     | (TEMPORARY-BINDING ANOTHER-MODF-PLACE ANOTHER-VALUE . ARGS)

Use it to specify a temporary binding for the new object created which will be
used in the subsequence MODF-PLACE NEW-VALUE pairs until the end of the MODF
form."
  (if args
      (destructuring-bind (next-symbol next-place next-value &rest next-args) args
        `(let ((,next-symbol ,(modf-expand value place (gensym))))
           (modf ,next-place ,next-value ,@next-args) ))
      (modf-expand value place (gensym)) ))

;; <<>>=
(defmacro modf-eval (&rest args)
  `(progn ,@args) )

