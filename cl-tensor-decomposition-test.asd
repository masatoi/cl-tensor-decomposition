#|
  This file is a part of cl-tensor-decomposition project.
|#

(defsystem "cl-tensor-decomposition-test"
  :defsystem-depends-on ("prove-asdf")
  :author ""
  :license ""
  :depends-on ("cl-tensor-decomposition"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "cl-tensor-decomposition"))))
  :description "Test system for cl-tensor-decomposition"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
