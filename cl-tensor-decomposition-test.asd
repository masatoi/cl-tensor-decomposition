#|
  This file is a part of cl-tensor-decomposition project.
|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((quit-on-debug
          (lambda (condition hook)
            (declare (ignore hook))
            (format *error-output*
                    "~&[cl-tensor-decomposition-test] Unhandled condition: ~A~%"
                    condition)
            (uiop:quit 1))))
    (unless (and (boundp '*debugger-hook*)
                 (functionp *debugger-hook*))
      (setf *debugger-hook* quit-on-debug))
    #+sbcl
    (setf sb-ext:*invoke-debugger-hook*
          (lambda (condition hook)
            (funcall quit-on-debug condition hook)))))

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
