
(in-package :modf)

;; @\section{Exapansions}

;; <<>>=
(defvar *modf-expansions*
  (make-hash-table) )

;; <<>>=
(defvar *modf-nth-arg* (make-hash-table))

;; <<>>=
(defmacro define-modf-expander (name nth-arg
                                (expr val new-val)
                                &body body )
  `(setf
    (gethash ',name *modf-expansions*)
    (lambda (,expr ,val ,new-val)
      ,@body )
    (gethash ',name *modf-nth-arg*)
    ,nth-arg ))

;; @\section{Rewrite Rules}

;; @Rewrite rules are basically macros for the {\em modf} expander.
;; <<*modf-rewrites*>> holds the macro expansion functions.

;; <<>>=
(defvar *modf-rewrites*
  (make-hash-table) )

;; (1) A function that returns the next form that needs expansion

;; (2) A function that given a symbol, FORM, representing the value that a form
;; used to have, and a symbol representing the value that (FUNC FORM) should
;; evaluate to, returns a form that builds the correct data structure.

;; <<>>=
(defmacro define-modf-rewrite (name (expr) &body body)
  `(setf (gethash ',name *modf-rewrites*)
         (/. (,expr)
            ,@body )))

;; <<>>=
(defun modf-name (symbol)
  "Make a symbol name that depends on symbol name and package, but is very
unlikely to be chosen by anyone.  This is for avoiding collisions for my
benefit, not the users."
  (mkstr "BUILDER:"
         (package-name (symbol-package symbol))
         ":" (symbol-name symbol) ))

;; <<>>=
(defmacro define-modf-function (name nth-arg (new-val &rest args) &body body)
  `(progn
     (defun ,(intern (modf-name name) :modf) (,new-val ,@args)
       ,@body )
     (setf (gethash ',name *modf-nth-arg*)
           ,nth-arg )))

;; <<>>=
(defmacro define-modf-method (name nth-arg (new-val &rest args) &body body)
  `(progn
     (defmethod ,(intern (modf-name name) :modf) (,new-val ,@args)
       ,@body )
     (setf (gethash ',name *modf-nth-arg*)
           ,nth-arg )))

;; <<>>=
(defmacro modf-eval (&rest args)
  `(progn ,@args) )

;; @\section{The {\em modf} macro}

;; @The <<modf>> macro is the main entry point to the library.

;; The basic syntax of the <<modf>> macro is as follows

;; <<>>=
(defun modf-expand (new-val expr)
  (cond ((atom expr)
         new-val )
        ((eql (car expr) 'modf-eval)
         new-val )
        ((gethash (car expr) *modf-rewrites*)
         (modf-expand new-val (funcall (gethash (car expr) *modf-rewrites*) expr)) )
        ((fboundp (intern (modf-name (car expr)) :modf))
         (let ((form (gensym)))
           (modf-expand `(let ((,form ,(nth (gethash (car expr) *modf-nth-arg*)
                                            expr )))
                           (,(intern (modf-name (car expr)) :modf)
                             ,new-val ,form ,@(cddr expr) ))
                        (nth (gethash (car expr) *modf-nth-arg*) expr) )))
        ((gethash (car expr) *modf-expansions*)
         (let ((form (gensym)))
           (multiple-value-bind (builder)
               (funcall (gethash (car expr) *modf-expansions*) expr form new-val)
             (modf-expand `(let ((,form ,(nth (gethash (car expr)
                                                       *modf-nth-arg* )
                                              expr )))
                             ,builder )
                          (nth (gethash (car expr) *modf-nth-arg*) expr) ))))
        (t (error "Don't know how to handle \"~A\"" expr)) ))

;; <<>>=
(defmacro modf (place value &rest args)
  (if args
      (destructuring-bind (next-symbol next-place next-value &rest next-args) args
        `(let ((,next-symbol ,(modf-expand value place)))
           (modf ,next-place ,next-value ,@next-args) ))
      (modf-expand value place) ))

