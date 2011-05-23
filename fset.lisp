
(in-package :modf)

;; @\section{FSet Integration}

;; @This works with seqs and maps.

(define-modf-function fset:lookup 1 (new-val collection key)
  (fset:with collection key new-val) )

(define-modf-rewrite fset:@ (expr)
  `(fset:lookup ,@(rest expr)) )

(define-modf-function fset:subseq 1
    (new-val collection start
             &optional (end (fset:size collection)) )
  (let ((seq (fset:convert 'fset:seq new-val)))
    (fset:concat (fset:subseq collection 0 start)
                 (fset:concat
                  (fset:subseq seq 0 (min (fset:size seq) (- end start)))
                  (fset:subseq collection
                               (+ start
                                  (min (fset:size seq) (- end start)) ))))))

(define-modf-function fset:last 1 (new-val collection)
  (fset:with-last (fset:less-last collection) new-val) )

(define-modf-function fset:first 1 (new-val collection)
  (fset:with-first (fset:less-first collection) new-val) )

(define-modf-function fset:less-first 1 (new-val collection)
  (fset:with-first (fset:convert 'fset:seq new-val) (fset:first collection)) )

(define-modf-function fset:less-last 1 (new-val collection)
  (fset:with-last (fset:convert 'fset:seq new-val) (fset:last collection)) )

(define-modf-rewrite fset:tail (expr)
  `(fset:less-first ,@(rest expr)) )
