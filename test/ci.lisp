;;; Test operation to test entire NLP test suite
;;; This test operation is also used by Travis CI

(defmethod asdf:perform ((o asdf:test-op)
                         (s (eql (asdf:find-system :cl-nlp))))
  (asdf:load-system :cl-nlp)
  ;;; TO DO: Fix and add other package tests
  (should-test:test :package :ncore)
  t)
