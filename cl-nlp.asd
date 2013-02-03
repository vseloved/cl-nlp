;;; (c) 2013 Vsevolod Dyomkin

(in-package #:asdf)

(defsystem #:cl-nlp
  :version "0.0.1"
  :description "NLP toolkit for Common Lisp"
  :creator "Vsevolod Dyomkin <vseloved@gmail.com>"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :license "Apache 2.0"
  :depends-on (#:rutils #:cl-ppcre)
  :serial t
  :components
  ((:module #:src
            :serial t
            :components
            ((:file "packages")
             (:file "util")
             (:module #:corpora
                      :serial t
                      :components
                      ((:file "reading")))
             (:file "test-util")
             (:module #:core
                      :serial t
                      :components
                      ((:file "tokenization")
                       #+nil (:file "distance")
                       #+nil (:file "ngram")))
             (:module #:generation
                      :serial t
                      :components
                      ((:file "generation")))))))