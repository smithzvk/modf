
(asdf:defsystem :modf
  :name "A SETF like macro for functional programming"
  :author "Zachary Smith <zachkostsmith@gmail.com>"
  :license "LLGPL"
  :description
  "This library simplifies functional programming by making it easier to make
new data structures with specified changes in place."
  :components ((:file "package")
               (:file "modf")
               (:file "rewrite-rules")
               (:file "basic")
               (:file "fset") )
  :serial t
  :depends-on (:toolbox :iterate) )

(asdf:defsystem :modf-fset
  :name "FSet extensions for MODF"
  :author "Zachary Smith <zachkostsmith@gmail.com>"
  :license "LLGPL"
  :components ((:file "fset"))
  :serial t
  :depends-on (:toolbox :iterate :modf :fset) )
