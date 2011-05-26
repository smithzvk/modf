
(defpackage :modf
    (:use :cl :iter)
  (:import-from :alexandria #:with-gensyms) 
  (:export #:modf
           #:modf-eval
           #:define-modf-rewrite
           #:define-modf-function
           #:define-modf-method
           #:define-modf-expander ))

