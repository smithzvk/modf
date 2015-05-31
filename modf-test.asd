
(asdf:defsystem :modf-test
  :author "Zach Kost-Smith <zachkostsmith@gmail.com>"
  :license "3 Clause BSD (http://opensource.org/licenses/BSD-3-Clause)"
  :description "Modf test suite"
  :components ((:file "test-setup")
               (:file "modf-test"))
  :serial t
  :depends-on (:iterate :stefil :modf))
