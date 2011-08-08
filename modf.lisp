
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
  (setf
   (gethash name *modf-nth-arg*)
   nth-arg )
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

;; @The macro <<modf-fn>> provides a similar functionality to <<setf>>'s
;; <<#'(setf fn)>>.  This can be used to access the function Modf will use to
;; invert a form.

;;<<>>=
(defmacro modf-fn (symbol)
  "Expand to the defined Modf function.  Basically, \(MODF-FN SYM) is the
functional analog of #'\(SETF SYM)."
  `(function ,(intern (modf-name symbol) :modf)) )

;; @The macros <<define-modf-function>> and <<define-modf-method>> are a way to
;; define a function or method that Modf will call with the new value and the
;; arguments to the accessor.  This function should invert the accessor into a
;; builder.

;; <<>>=
(defmacro define-modf-function (name nth-arg (new-val &rest args) &body body)
  "Define a new modf function.  It inverts NAME forms by modifying the NTH-ARG
term of the arguments of the place form in the MODF macro."
  ;; Side effect in a macro, I know.  How can you do this via EVAL-WHEN if I
  ;; want it to run even if not a toplevel macro?
  (setf (gethash name *modf-nth-arg*)
        nth-arg )
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ',name *modf-nth-arg*)
             ,nth-arg ))
     (defun ,(intern (modf-name name) :modf) (,new-val ,@args)
       ,@body )))

;; <<>>=
(defmacro define-modf-method (name nth-arg (new-val &rest args) &body body)
  "Define a new modf method.  It inverts NAME forms by modifying the NTH-ARG
term of the arguments of the place form in the MODF macro.  This method can
specialize on any of ARGS."
  ;; Side effect in a macro, I know.  How can you do this via EVAL-WHEN if I
  ;; want it to run even if not a toplevel macro?
  (setf (gethash name *modf-nth-arg*)
        nth-arg )
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ',name *modf-nth-arg*)
             ,nth-arg ))
     (defmethod ,(intern (modf-name name) :modf) (,new-val ,@args)
       ,@body )))

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
;; function, called <<modf-expand>>, needs in order to work:

;; \begin{enumerate}
;;  \item <<new-val>>: The value we want <<expr>> to evaluate to.

;;  \item <<expr>>: The form we need to expand.

;;  \item <<form>>: The symbol which it should use to bind the value of the
;;  enclosed container argument in <<expr>>.  This is for optimization purposes.

;; \end{enumerate}

;; On the first call to <<modf-expand>>, the arguments should be the new value
;; we want inserted, the full modf form to be expanded, and a gensym.  In our
;; example above


(defun container-arg-n (expr)
  (cond ((eql (car expr) 'cl:apply)
         (1+ (gethash (cadadr expr) *modf-nth-arg* 1)) )
        (t (gethash (car expr) *modf-nth-arg* 1)) ))

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

(defvar *accessor-heuristics* t
  "This controls whether we should make educated guesses regarding inverting
structure slot readers.  For strictly correct behavior, set this to nil.")

(defun late-invert (func new-val obj &rest args)
  "This is a generic catch as much as you can function.  It attempts to identify
class accessor functions, structure accessor functions, and provides late MODF
defined functions \(i.e. you used MODF before using DEFINE-MODF-FUNCTION).

All of this functionality is less than ideal efficiency wise, but working over
efficiency any day, right?  If you want better performance, define all of you
functions ahead of time."
  (cond
    ;; Check to see if the function is a defined modf function/method.
    ((fboundp (intern (modf-name func) :modf))
     (apply (symbol-function (intern (modf-name func) :modf))
            new-val obj args ))
    ;; Check to see if this is a generic function and there are no extra
    ;; arguments (which means it might be a class slot accessor)
    #+closer-mop
    ((and (not args)
          (typep (symbol-function func) 'generic-function) )
     (late-class-reader-inverter func new-val obj) )
    ;; Check to see if this is likely a structure accessor function
    ((and *accessor-heuristics*
          (not args)
          (typep obj 'structure-object)
          (let ((struct-name (symbol-name (type-of obj))))
            (equal struct-name (subseq (symbol-name func) 0 (length struct-name))) ))
     (let ((new-struct (copy-structure obj)))
       ;; We use eval here because this setf form is hard to invert.  We could,
       ;; in principle, using GET-SETF-EXPANSION.
       (eval `(setf (,func ,new-struct) ',new-val))
       new-struct ))
    (t (error "How shall I invert ~S?" func))))

#+closer-mop
(defun late-class-reader-inverter (func new-val obj)
  #+ecl
  ;; ECL seems to work a bit more intuitively.  Effective slots know their
  ;; readers.
  (let* ((class (class-of obj))
         (slots (closer-mop:class-slots class))
         (new-instance (make-instance class)))
    (loop for slot in slots do
             (cond ((member func (closer-mop:slot-definition-readers slot))
                    (setf (slot-value new-instance
                                      (closer-mop:slot-definition-name slot) )
                          new-val ))
                   ((slot-boundp obj (closer-mop:slot-definition-name slot))
                    (setf (slot-value new-instance (closer-mop:slot-definition-name
                                                    slot ))
                          (slot-value obj (closer-mop:slot-definition-name slot)) ))
                   (t (slot-makunbound new-instance
                                       (closer-mop:slot-definition-name slot) ))))
    new-instance )
  #-ecl
  (let* ((class (class-of obj))
         (slot-groups (mapcar #'closer-mop:class-direct-slots
                              (closer-mop:class-precedence-list class) ))
         (new-instance (make-instance class))
         slot-found )
    (loop
      for slots in slot-groups do
         (loop
           for slot in slots do
              (cond ((member func (closer-mop:slot-definition-readers slot))
                     (setf slot-found t
                           (slot-value new-instance
                                       (closer-mop:slot-definition-name slot) )
                           new-val ))
                    ((and (not slot-found)
                          (slot-boundp obj (closer-mop:slot-definition-name slot)) )
                     (setf (slot-value new-instance (closer-mop:slot-definition-name
                                                     slot ))
                           (slot-value obj (closer-mop:slot-definition-name slot)) ))
                    ((not slot-found)
                     (slot-makunbound new-instance
                                      (closer-mop:slot-definition-name slot) )))))
    new-instance ))

;; <<>>=
(defun modf-expand (new-val expr enclosed-obj-sym env)
  (cond ((or (atom expr) (eql (car expr) 'modf-eval))
         `(let ((,enclosed-obj-sym ,expr))
            ,new-val ))
        ((macro-function (car expr) env)
         (modf-expand new-val (funcall (macro-function (car expr) env) expr env)
                      enclosed-obj-sym env))
        ;; First, try rewrite rules
        ((gethash (car expr) *modf-rewrites*)
         (modf-expand new-val (funcall (gethash (car expr) *modf-rewrites*) expr)
                      enclosed-obj-sym env))
        ;; Okay, we are going to call modf-expand
        (t (let* ((obj-sym (gensym))
                  (new-val (if enclosed-obj-sym
                               `(let ((,enclosed-obj-sym
                                        ,(replace-nth (container-arg-n expr) expr
                                                      obj-sym )))
                                  ,new-val )
                               new-val )))
             (modf-expand
              (cond
                ;; Then, see if an expansion is defined
                ((expansions-defined? expr)
                 ;; bind form to the enclosed object
                 (funcall (gethash (accessor-in expr) *modf-expansions*)
                          expr obj-sym new-val ))
                ;; Lastly, This must be a modf function or method
                ((apply-expression? expr)
                 (if (modf-fn-defined? expr)
                     `(apply (modf-fn ,(cadadr expr))
                             ,new-val
                             ,@(cddr
                                (replace-nth
                                 (container-arg-n expr)
                                 expr obj-sym )))
                     `(apply #'late-invert ',(cadadr expr) ,new-val
                             ,@(cddr
                                (replace-nth
                                 (container-arg-n expr)
                                 expr obj-sym )))))
                (t
                 (if (modf-fn-defined? expr)
                     `(funcall (modf-fn ,(car expr))
                               ,new-val
                               ,@(cdr
                                  (replace-nth
                                   (container-arg-n expr)
                                   expr obj-sym )))
                     `(late-invert ',(car expr) ,new-val
                                   ,@(cdr
                                      (replace-nth
                                       (container-arg-n expr)
                                       expr obj-sym ))))))
              (nth (container-arg-n expr) expr)
              obj-sym env)))))

;; <<>>=
(defmacro modf (place value &rest more &environment env)
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
        `(let ((,next-symbol ,(modf-expand value place nil env)))
           (modf ,next-place ,next-value ,@next-more) ))
      (modf-expand value place nil env) ))

(defun find-container (place)
  (cond ((atom place)
         place )
        ((gethash (car place) *modf-rewrites*)
         (find-container (funcall (gethash (car place) *modf-rewrites*) place)) )
        ((not (expandable? place))
         (error "You can only use FSETF if you are modfifying a container.  I don't have a place to set when given ~A." place) )
        (t (let ((nth-arg (container-arg-n place)))
             (unless nth-arg
               (error "I can't figure out which argument is the container in ~A."
                      place ))
             (find-container (nth nth-arg place)) ))))

;; <<>>=
(defmacro fsetf (place value &rest more)
  ;; First we need to find the "container" variable
  (let ((container (find-container place)))
    (with-gensyms (val-sym)
      (if more
          `(let ((,val-sym ,value))
             (setf ,container (modf ,place ,val-sym))
             (fsetf ,@more) )
          `(let ((,val-sym ,value))
             (setf ,container (modf ,place ,val-sym))
             ,val-sym )))))

;; <<>>=
(defmacro modf-eval (&rest args)
  `(progn ,@args) )

