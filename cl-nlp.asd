;;; (c) 2013 Vsevolod Dyomkin

(in-package #:asdf)


(defsystem #:cl-nlp
  :version "0.0.4"
  :description "NLP toolkit for Common Lisp."
  :author "Vsevolod Dyomkin <vseloved@gmail.com>"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :license "Apache 2.0"
  :depends-on (#:rutils #:cl-fad #:cl-ppcre #:cxml #:drakma)
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
                      ((:file "util")
                       (:file "corpus")
                       (:file "brown")
                       (:file "nps-chat")
                       (:file "reuters")))
             (:file "test-util")
             (:module #:core
                      :serial t
                      :components
                      ((:file "measures")
                       (:file "tokenization")
                       (:file "ngrams")
                       (:file "language-models")
                       (:file "indexing")
                       (:file "learning")))
             (:module #:generation
                      :serial t
                      :components
                      ((:file "markov-chain")))
             (:module #:syntax
                      :serial t
                      :components
                      ((:file "pos-tag")
                       (:file "hmm")))
             (:file "user")))))