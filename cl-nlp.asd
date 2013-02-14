;;; (c) 2013 Vsevolod Dyomkin

(in-package #:asdf)

(defsystem #:cl-nlp
  :version "0.0.2"
  :description "NLP toolkit for Common Lisp"
  :author "Vsevolod Dyomkin <vseloved@gmail.com>"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :license "Apache 2.0"
  :depends-on (#:rutils #:cl-fad #:cl-ppcre #:cxml)
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
                      ((:file "corpus")
                       (:file "brown")
                       (:file "nps-chat")))
             (:file "test-util")
             (:module #:core
                      :serial t
                      :components
                      ((:file "tokenization")
                       (:file "freq")
                       #+nil (:file "distance")
                       #+nil (:file "ngram")))
             (:module #:generation
                      :serial t
                      :components
                      ((:file "markov-chain")))
             (:file "user")))))
