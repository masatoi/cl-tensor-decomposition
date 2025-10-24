#|
  This file is a part of cl-tensor-decomposition project.
|#

(defsystem "cl-tensor-decomposition-test"
  :author ""
  :license ""
  :depends-on ("cl-tensor-decomposition"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "cl-tensor-decomposition"))))
  :description "Test system for cl-tensor-decomposition"

  :perform (test-op (op c) (uiop:symbol-call :rove '#:run c)))
