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
                ((:file "core")
                 (:file "tensor" :depends-on ("core"))
                 (:file "reporting" :depends-on ("core" "tensor"))
                 (:file "model-selection" :depends-on ("core" "tensor"))
                 (:file "diagnostics" :depends-on ("core" "tensor" "reporting")))))
  :description "A tensor decomposition library for Common Lisp which support sparse tensor."
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "cl-tensor-decomposition-test"))))

;;; Experimental subsystem for CORCONDIA
;;; Load explicitly with: (asdf:load-system :cl-tensor-decomposition/corcondia)
(defsystem "cl-tensor-decomposition/corcondia"
  :version "0.1.0"
  :author "Satoshi Imai"
  :license "MIT"
  :depends-on ("cl-tensor-decomposition")
  :components ((:module "src"
                :components
                ((:file "corcondia"))))
  :description "EXPERIMENTAL: CORCONDIA (Core Consistency Diagnostic) for CP decomposition.
WARNING: This implementation is designed for dense tensors. Results on sparse tensors
may be unreliable. Use cross-validation for model selection with sparse data instead.")
