
(defpackage :modf
    (:use :cl)
  (:import-from :alexandria #:with-gensyms) 
  (:export #:modf
           #:modf-eval
           #:modf-fn
           #:define-modf-rewrite
           #:define-modf-function
           #:define-modf-method
           #:define-modf-expander ))

