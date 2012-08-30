
(asdf:defsystem :modf-test
  :name "Modf test suite"
  :author "Zachary Smith <zachkostsmith@gmail.com>"
  :license "Modified BSD"
  :components ((:file "test-setup")
               (:file "modf-test"))
  :serial t
  :depends-on (:iterate :stefil :modf))