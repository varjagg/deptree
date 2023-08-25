(asdf:defsystem #:deptree
  :description "System dependency listing and archiving tool for Common Lisp"
  :author "Eugene Zaikonnikov <eugene@funcall.org>"
  :license "MIT"
  :serial t
  :depends-on (#:cl-fad #:salza2)
  :components ((:file "deptree")))
