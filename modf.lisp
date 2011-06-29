
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
      (gethash ',name *modf-nth-arg*)
      ,nth-arg
      (gethash ',name *modf-expansions*)
      (lambda (,expr ,val ,new-val)
        ,@body ))))

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

;;<<>>=
(defmacro modf-fn (symbol)
  "Expand to the defined Modf function.  Basically, \(MODF-FN SYM) is the
functional analog of #'\(SETF SYM)."
  `(function ,(intern (modf-name symbol) :modf)) )

;; <<>>=
(defmacro define-modf-function (name nth-arg (new-val &rest args) &body body)
  "Define a new modf function.  It inverts NAME forms by modifying the NTH-ARG
term of the arguments of the place form in the MODF macro."
  (setf (gethash name *modf-nth-arg*)
        nth-arg )
  `(defun ,(intern (modf-name name) :modf) (,new-val ,@args)
     ,@body ))

;; <<>>=
(defmacro define-modf-method (name nth-arg (new-val &rest args) &body body)
  "Define a new modf method.  It inverts NAME forms by modifying the NTH-ARG
term of the arguments of the place form in the MODF macro.  This method can
specialize on any of ARGS."
  ;; Side effect in a macro, I know.  How can you do this via EVAL-WHEN if the
  ;; rest of the macro-expansion depends on the side effect.
  (setf (gethash name *modf-nth-arg*)
        nth-arg )
  `(defmethod ,(intern (modf-name name) :modf) (,new-val ,@args)
     ,@body ))

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

;; @\subsection{How does modf expansion work?}

;; @In order to expand modf expressions, we need to work from the inside out.
;; This means the inner most form is the first we need to deal with.  For a
;; moment let's limit ourselves to the simple case of cons cells.  When <<modf>>
;; recieves an expression like (car (cdr x)), it needs to first figure out how
;; to replace the cdr of x while storing the car of x.  Thus the first part of
;; the modf expansion should look like

;; (let ((tmp x)) ; Grab x to protect against multiple evaluation
;;   ;; The builder code
;;   (cons (car x)
;;         ... )

;; Then, <<modf>> needs to replace ``...'' with the method of replacing the car
;; of a cons cell.  Since we know we will be operating on the cdr of x, we first
;; grab that value (just like we grabbed x above)

;; (let ((tmp x))
;;   (cons (car x)
;;         (let ((tmp (cdr x)))
;;           ... )))

;; We do this as an optimization, this way we only need to drill down into the
;; structure one time.  We then expand the outer accessor

;; (let ((tmp x))
;;   (cons (car x)
;;         (let ((tmp (cdr x)))
;;           (cons ...
;;                 (cdr tmp) ))))

;; Notice that at each level we only have need of one <<tmp>> symbol.  The last
;; question is, what do we fill into the ``...''.  That spot will be filled with
;; the new value.

;; Now, how can we implement this in code?  The nature of the problem is
;; actually a bit backwards from how we normally think about it.  We need a
;; function that will build up the expansion as we talk the code tree, and
;; passes it to the next level of recursion.  The next level then wraps that
;; expansion.  Looking at the procedure above, we see the information such a
;; function needs to work:

;; \begin{enumerate}
;;  \item The previous level's expansion.  In the above example that is:

;; (cons new-val (cdr tmp))

;;  \item The data it will be operating on

;;  \item

;; \end{enumerate}

;; To be completed later

(defun container-arg-n (expr)
  (cond ((eql (car expr) 'cl:apply)
         (1+ (gethash (cadadr expr) *modf-nth-arg* 'no-nth-arg)) )
        (t (gethash (car expr) *modf-nth-arg* 'no-nth-arg)) ))

(defun modf-fn-defined? (expr)
  (cond ((eql (car expr) 'cl:apply)
         (fboundp (intern (modf-name (cadadr expr)) :modf)) )
        (t (fboundp (intern (modf-name (car expr)) :modf))) ))

(defun expansions-defined? (expr)
  (gethash (car expr) *modf-expansions*) )

(defun accessor-in (expr)
  (case (car expr)
    (cl:apply (cadadr expr))
    (otherwise (car expr)) ))

(defun apply-expression? (expr)
  (eql (car expr) 'cl:apply) )

(defun funcall-expression? (expr)
  (eql (car expr) 'cl:funcall) )

(defun expandable? (expr)
  (cond ((atom expr) nil)
        ((eql (first expr) 'modf-eval) nil)
        (t expr) ))

;; <<>>=
(defun modf-expand (new-val expr form)
  (cond ((atom expr)
         new-val )
        ((eql (car expr) 'modf-eval)
         new-val )
        ;; First, try rewrite rules
        ((gethash (car expr) *modf-rewrites*)
         (modf-expand new-val (funcall (gethash (car expr) *modf-rewrites*) expr)
                      form ))
        ;; Then, see if an expansion is defined
        ((expansions-defined? expr)
         (let ((enclosed-obj-sym (gensym)) )
           (multiple-value-bind (builder)
               (funcall (gethash (accessor-in expr) *modf-expansions*)
                        expr form new-val )
             (modf-expand
              `(let ((,form
                      ,(let ((enclosed-obj (nth (container-arg-n expr)
                                                expr )))
                         (let ((it (and (expandable? enclosed-obj)
                                        (container-arg-n enclosed-obj) )))
                           (if it
                               (replace-nth it
                                            enclosed-obj
                                            enclosed-obj-sym )
                               enclosed-obj )))))
                 ,builder )
              (nth (container-arg-n expr) expr)
              enclosed-obj-sym ))))
        ;; Lastly, This must be a modf function or method
        (t 
         (let ((enclosed-obj-sym (gensym)) )
           (modf-expand
            `(let ((,form
                    ,(let ((enclosed-obj (nth (container-arg-n expr)
                                              expr )))
                       (let ((it (and (expandable? enclosed-obj)
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
                      (t
                       `(,(intern (modf-name (car expr)) :modf)
                         ,new-val
                         ,@(cdr
                            (replace-nth
                             (container-arg-n expr)
                             expr form ))))))
            (nth (container-arg-n expr) expr)
            enclosed-obj-sym )))))
        ;; (t (error "Don't know how to handle \"~A\"" expr)) ))

;; <<>>=
(defmacro modf (place value &rest more)
  "Make a new object \(which may use some of the old object) such that PLACE
evaluates to VALUE.

MORE should have the form...

MORE : NIL
     | (TEMPORARY-BINDING ANOTHER-MODF-PLACE ANOTHER-VALUE . MORE)

Use it to specify a temporary binding for the new object created which will be
used in the subsequence MODF-PLACE NEW-VALUE pairs until the end of the MODF
form."
  (if more
      (destructuring-bind (next-symbol next-place next-value &rest next-more) more
        `(let ((,next-symbol ,(modf-expand value place (gensym))))
           (modf ,next-place ,next-value ,@next-more) ))
      (modf-expand value place (gensym)) ))

;; <<>>=
(defmacro modf-eval (&rest args)
  `(progn ,@args) )

