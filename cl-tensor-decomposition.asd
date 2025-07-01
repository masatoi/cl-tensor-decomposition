#|
  This file is a part of cl-tensor-decomposition project.
|#

(defsystem "cl-tensor-decomposition"
  :version "0.1.0"
  :author "Satoshi Imai"
  :license "MIT"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "core"))))
  :description "A tensor decomposition library for Common Lisp which support sparse tensor."
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "cl-tensor-decomposition-test"))))
