;;; (c) 2014 Deepak Surti, Vsevolod Dyomkin

;;; Test operation to test entire NLP test suite
;;; This test operation is also used by Travis CI

(defmethod asdf:perform ((o asdf:test-op)
                         (s (eql (asdf:find-system :cl-nlp))))
  (asdf:load-system :cl-nlp)
  ;;; TO DO: Fix and add other package tests
  (every #'identity (mapcar #'(lambda (pkg) (should-test:test :package pkg))
                            '(:ncore :ncorp :nlearn :ntag :nlp-user))))
