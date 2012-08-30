
(defpackage :modf
    (:use :cl :iterate)
  (:import-from :alexandria #:with-gensyms)
  (:export #:modf
           #:modf-eval
           #:modf-fn
           #:fsetf
           #:define-modf-rewrite
           #:define-modf-function
           #:define-modf-method
           #:define-modf-expander))

