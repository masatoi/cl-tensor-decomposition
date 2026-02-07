#|
  Experimental: Test system for CORCONDIA (Core Consistency Diagnostic)

  WARNING: CORCONDIA is designed for dense tensors. Results on sparse tensors
  may be unreliable. Use cross-validation for model selection with sparse data.
|#

(defsystem "cl-tensor-decomposition-corcondia-test"
  :author ""
  :license ""
  :depends-on ("cl-tensor-decomposition/corcondia"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "corcondia-test"))))
  :description "Test system for CORCONDIA (experimental)"

  :perform (test-op (op c) (uiop:symbol-call :rove '#:run c)))
