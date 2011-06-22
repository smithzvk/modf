
(asdf:defsystem :modf
  :name "Modf test suite"
  :author "Zachary Smith <zachkostsmith@gmail.com>"
  :license "Modified BSD"
  :components ((:file "modf-test"))
  :serial t
  :depends-on (:alexandria :closer-mop :iterate :stefil) )