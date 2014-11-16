;;; (c) 2014 Deepak Surti, Vsevolod Dyomkin

;;; Test operation to test entire NLP test suite
;;; This test operation is also used by Travis CI

(defparameter *test-failures-or-errors* 0)

(defmethod asdf:perform ((o asdf:test-op)
                         (s (eql (asdf:find-system :cl-nlp))))
  (asdf:load-system :cl-nlp)
  (run-all-tests)
  (test-suite-status))

(defun run-all-tests ()
  "Run all test modules and keep track of number of failures/errors."
  (setf *test-failures-or-errors* 0)
  (dolist (package '(:ncore :ncorp :nlearn :ntag :nlp-user))
    (unless (should-test:test :package package)
      (incf *test-failures-or-errors* 1))))

(defun test-suite-status ()
  "Used by CI as unix command status."
  (values *test-failures-or-errors*))
