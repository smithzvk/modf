
(in-package :modf)

;; @The point of this file is to remove big library dependencies by placing the
;; handful of functions I use here instead of keeping them in a big library.
;; This means that there is more code to maintain, but it also makes the library
;; more portable and means I can be much sloppier in my toolbox library, so I
;; guess it's a win.

(defun mkstr (&rest args)
  "MaKe STRing"
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))
