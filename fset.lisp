
(in-package :modf)

;; @\section{FSet Integration}

(define-modf-expander fset:lookup (expr val new-val)
  (values
    `(fset:with ,val ,(third expr) ,new-val)
    #'second ))

(define-modf-rewrite fset:@ (expr)
  `(fset:lookup ,@(rest expr)) )
