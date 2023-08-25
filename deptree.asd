(asdf:defsystem #:deptree
  :description "ASDF systems dependency listing and archiving tool for Common Lisp"
  :author "Eugene Zaikonnikov <eugene@funcall.org>"
  :license "MIT"
  :serial t
  :depends-on (#:tar/create)
  :components ((:file "deptree")))
