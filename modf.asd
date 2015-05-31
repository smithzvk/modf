
(asdf:defsystem :modf
  :author "Zachary Smith <zachkostsmith@gmail.com>"
  :license "3 Clause BSD (http://opensource.org/licenses/BSD-3-Clause)"
  :description "A SETF like macro for functional programming"
  :long-description
  "This library simplifies functional programming by making it easier to make
new data structures with specified changes in place."
  :components ((:file "package")
               (:file "utils")
               (:file "modf")
               (:file "rewrite-rules")
               (:file "basic")
               (:file "modf-def"))
  :serial t
  :depends-on (:alexandria
               #-abcl :closer-mop
               :iterate))


