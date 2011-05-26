
(asdf:defsystem :modf
  :name "A SETF like macro for functional programming"
  :author "Zachary Smith <zachkostsmith@gmail.com>"
  :license "LLGPL"
  :description
  "This library simplifies functional programming by making it easier to make
new data structures with specified changes in place."
  :components ((:file "package")
               (:file "utils")
               (:file "modf")
               (:file "rewrite-rules")
               (:file "basic"))
  :serial t
  :depends-on (:alexandria :iterate :closer-mop) )


